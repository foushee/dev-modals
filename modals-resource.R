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
#library(ggcorrplot)
library(ggridges)
library(kableExtra)

#### user-defined functions
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
##########################################################################################
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

##########################################################################################
# Bootstrapping confidence intervals 
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}
na.mean <- function(x) {mean(x,na.rm=T)}
na.sum <- function(x) {sum(x,na.rm=T)}


#### return mean proportion as a percent
percent <- function(x){100*na.mean(x)}

#####
my.formula <- y ~ x

####
##########################################################################################
#### ggplot variables

#### for figures
modal.colors <- c('can' = '#FDE725FF', 
                       'could' = '#3CBB75FF',
                       'should' = '#33638DFF',
                       'will' = '#440154FF')
modal.fills <-  c('can' = '#FDE72580', 
                  'could' = '#3CBB7580',
                  'should' = '#33638D80',
                  'will' = '#44015480')
prop.labels <- c(20 = '20%\nProbability',
                 80='80%\nProbability',
                 100='100%\nProbability')
#aoi.colors <- c('illustration' = 'lightsteelblue',
#                'gif' = 'orange1')
#aoi.fills <- c('illustration' = '#b0c4de60',
 #            'gif' = '#FFA50060')
#aoi.labels <- c('Illustration', 'GIF')
#comp.labels <- c('SIMPLE', 'COMPLEX')
#names(comp.labels) <- c('simple', 'complex')
#names(aoi.labels) <- c('illustration', 'gif')


#### themes
storybook.theme <- theme(
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

options(digits = 2)

#### themes
sb.density.theme <- theme(
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