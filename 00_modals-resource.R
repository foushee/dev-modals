###############################################################################
### Epistemic Modals in Children & Adults 
### Study ID 45.0, Language & Cognitive Development Lab
### Ruthe Foushee
### Libraries and utility functions for analysis - load first
### Updated 04 April 2021 in prep for Jeff Anderson's UCB Undergraduate Thesis
###############################################################################

# Libraries
suppressMessages(library(dplyr))
library(ggplot2)
library(forcats)
library(lme4)
library(boot)
library(data.table)
library(tidyr)
library(ggpmisc)
library(car)
library(perm)
library(ggridges)
library(kableExtra)
#library(stargazer)

###############################################################################
# Utility Functions
###############################################################################
# Bootstrap function
set.seed(1)

"bootstrap"<- function(x,nboot,theta,...,func=NULL) {
  call <- match.call()
  
  n <- length(x)
  bootsam<- matrix(sample(x,size=n*nboot,replace=TRUE),nrow=nboot)
  thetastar <- apply(bootsam,1,theta,...)
  func.thetastar <- NULL; jack.boot.val <- NULL; jack.boot.se <- NULL;
  if(!is.null(func)){
    match1 <- function(bootx,x){
      duplicated(c(bootx,x))[(length(x)+1) : (2*length(x))]
    } 
    matchs <- t(apply(bootsam,1,match1,x))
    func.thetastar <- func(thetastar)
    jack.boot <- function(inout,thetastar,func){
      func(thetastar[!inout])
    }
    jack.boot.val <- apply(matchs,2,jack.boot,thetastar,func)
    
    if(sum(is.na(jack.boot.val)>0)) {
      cat("At least one jackknife influence value for func(theta) is undefined", 
          fill=TRUE)
      cat(" Increase nboot and try again",fill=TRUE)
      return()
    }
    
    if( sum(is.na(jack.boot.val))==0) {
      jack.boot.se <- sqrt( ((n-1)/n)*sum( (jack.boot.val-mean(jack.boot.val))^2 )  )
      
    }
  }
  
  return(list(thetastar=thetastar, func.thetastar=func.thetastar,
              jack.boot.val=jack.boot.val, jack.boot.se=jack.boot.se,
              call=call))
}

################################################################################
# Bootstrapping confidence intervals 

theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(
    1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(
    1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - 
    mean(x,na.rm=na.rm)}
na.mean <- function(x) {mean(x,na.rm=T)}
na.sum <- function(x) {sum(x,na.rm=T)}

#### return mean proportion as a percent
percent <- function(x){100*na.mean(x)}

################################################################################
# Ggplot variables
################################################################################
# Plotting colors and labels
modal.colors <- c('can' = '#FDE725FF', 
                       'could' = '#3CBB75FF',
                       'should' = '#33638DFF',
                       'will' = '#440154FF')

modal.colors3 <- c(#'wont' = 'gray10', 
                   'wont' = '#FCA636FF',
                  'could' = '#3CBB75FF',
                  'should' = '#33638DFF',
                  'will' = '#440154FF')

modal.fills <-  c('can'='#FDE72580', 
                  'could' = '#3CBB7580',
                  'should' = '#33638D80',
                  'will' = '#44015480')

prop.labels <- c('20' = '20%\nProbability',
                 '80' = '80%\nProbability',
                 '100' = '100%\nProbability')

################################################################################
# Plotting themes
## 
modals.theme <- theme(
  panel.border = element_rect(colour="gray30", fill=NA),
  #axis.title.x = element_text(size=18, colour="gray42"),
  axis.title.x = element_text(size=12, colour="gray30", family='serif'),
  axis.text.x = element_text(size=12, colour="gray30", family='serif'),
  axis.ticks.x = element_blank(),
  axis.title.y = element_text(size=12, colour="gray30", family='serif'),
  axis.text.y = element_text(size=12, colour="gray30", family='serif'),
  panel.background = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  legend.position="right",
  legend.title = element_text(size=12, colour="gray30", family='serif'),
  legend.text  = element_text(size=12, colour="gray30", family='serif'),
  plot.title = element_text(size=14, colour="gray30", family='serif'),
  strip.text.x = element_text(size=14, colour="gray30", family='serif'),
  strip.background = element_rect(colour="gray30", fill=NA),
)

# No axis titles
modals.density.theme <- theme(
  panel.border = element_rect(colour="gray30", fill=NA),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text.y = element_text(size=12, colour="gray30", family='serif'),
  axis.text.x = element_text(size=12, colour="gray30", family='serif'),
  panel.background = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  legend.position="right",
  legend.title = element_text(size=12, colour="gray30", family='serif'),
  legend.text  = element_text(size=12, colour="gray30", family='serif'),
  plot.title = element_text(size=14, colour="gray30", family='serif'),
  strip.text.x = element_text(size=14, colour="gray30", family='serif'),
  strip.background = element_rect(colour="gray30", fill=NA),
)

options(digits = 2)

# Faceted barplot theme
modals.barplot.theme <- theme(strip.placement = 'outside',
        strip.background = element_rect(fill='white'),
        strip.text.x = element_text(family='serif', size=14, color='gray10'),
        strip.text.y = element_text(family='serif', size=12, color='gray10', angle = 0),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(family='serif', size=14, color='gray10'),
        axis.text.x=element_text(family='serif', size=12, color='gray10', angle=60, hjust=1),
        panel.background = element_rect(fill='white'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill='gray95', color='gray10'),
        legend.background = element_rect(fill='gray95'),
        legend.text = element_text(family='serif', size=12, color='gray10'),
        legend.title = element_text(family='serif', size=14, color='gray10'),
        plot.margin = margin(30, 10, 10, 20),
        plot.title = element_text(family='serif', size=16, 
                                  face='bold', color='gray10',
                                  margin=margin(2, 0, 20, 0)))

# Faceted barplot theme - sans serif
modals.barplot.theme.sans <- theme(strip.placement = 'outside',
                              strip.background = element_rect(fill='white'),
                              strip.text.x = element_text(family='sans', size=14, color='gray10'),
                              strip.text.y = element_text(family='sans', size=12, color='gray10', angle = 0),
                              axis.text.y=element_blank(),
                              axis.ticks.y=element_blank(),
                              axis.title=element_text(family='sans', size=14, color='gray10'),
                              axis.text.x=element_text(family='sans', size=12, color='gray10', angle=60, hjust=1),
                              panel.background = element_rect(fill='white'),
                              panel.grid = element_blank(),
                              #plot.background = element_rect(fill='gray95', color='gray10'),
                              #legend.background = element_rect(fill='gray95'),
                              legend.text = element_text(family='sans', size=12, color='gray10'),
                              legend.title = element_text(family='sans', size=14, color='gray10'),
                              plot.margin = margin(30, 10, 10, 20),
                              plot.title = element_text(family='sans', size=16, 
                                                        face='bold', color='gray10',
                                                        margin=margin(2, 0, 20, 0)))

# Faceted line plot theme
modals.lineplot.theme <- theme(strip.placement = 'outside',
      strip.background = element_rect(fill='white'),
      strip.text.x = element_text(family='serif', size=14, color='gray10'),
      strip.text.y = element_text(family='serif', size=12, color='gray10', angle = 0),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title=element_text(family='serif', size=14, color='gray10'),
      axis.text.x=element_text(family='serif', size=12, color='gray10'),
      panel.background = element_rect(fill='white'),
      panel.grid = element_blank(),
      plot.background = element_rect(fill='gray90', color='gray10'),
      legend.background = element_rect(fill='gray90'),
      legend.text = element_text(family='serif', size=12, color='gray10'),
      legend.title = element_text(family='serif', size=14, color='gray10'),
      plot.margin = margin(30, 10, 10, 20),
      plot.title = element_text(family='serif', size=16, 
                                face='bold', color='gray10',
                                margin=margin(2, 0, 20, 0)))

# Faceted line plot theme - sans serif
modals.lineplot.theme.sans <- theme(strip.placement = 'outside',
                               strip.background = element_rect(fill='white'),
                               strip.text.x = element_text(family='sans', size=14, color='gray10'),
                               strip.text.y = element_text(family='sans', size=12, color='gray10', angle = 0),
                               axis.text.y=element_blank(),
                               axis.ticks.y=element_blank(),
                               axis.title=element_text(family='sans', size=14, color='gray10'),
                               axis.text.x=element_text(family='sans', size=12, color='gray10'),
                               panel.background = element_rect(fill='white'),
                               panel.grid = element_blank(),
                               #plot.background = element_rect(fill='gray90', color='gray10'),
                               #legend.background = element_rect(fill='gray90'),
                               legend.text = element_text(family='sans', size=12, color='gray10'),
                               legend.title = element_text(family='sans', size=14, color='gray10'),
                               plot.margin = margin(30, 10, 10, 20),
                               plot.title = element_text(family='sans', size=16, 
                                                         face='bold', color='gray10',
                                                         margin=margin(2, 0, 20, 0)))
