
### read in csvs, match cols
c <- read.csv('Gen. 2 FULL Children.csv')
c$age_group <- 'child'
c$age_year <- c$X
c <- c[, !names(c) %in% c('X')]

a <- read.csv('Gen. 2 FULL Adults.csv')
a$age_group <- 'adult'
a$age_year <- 'Adult'

### new df with both
d <- rbind(c, a)

### new vars
d$stimulus <- d$trial_name
d$proportion <- gsub('[_couldwishan2.jpg]+$', '', d$stimulus)
d$proportion <- as.numeric(as.character(d$proportion))
d$modal_pair <- gsub('^[1208_]*', '', d$stimulus)
d$modal_pair <- gsub('[2.jpg]*', '', d$modal_pair)
d$left_modal <- gsub('_[shouldcaniw]*$', '', d$modal_pair)
d$right_modal <- gsub('^[shouldcaniw]*_', '', d$modal_pair)
d$modal_pair[d$modal_pair %in% c('should_will', 'will_should')] <- 'should-will'
d$modal_pair[d$modal_pair %in% c('could_will', 'will_could')] <- 'could-will'
d$modal_pair[d$modal_pair %in% c('could_should', 'should_could')] <- 'could-should'
d$modal_pair[d$modal_pair %in% c('could_can', 'can_could')] <- 'can-could'
d$selected_modal <- d$left_modal
d$selected_modal[d$response_29Left_16_right==16] <- d$right_modal[d$response_29Left_16_right==16]
d$will <- 0
d$will[d$selected_modal=='will'] <- 1
d$would <- 0
d$would[d$selected_modal=='would'] <- 1
d$can <- 0
d$can[d$selected_modal=='can'] <- 1
d$could <- 0
d$could[d$selected_modal=='could'] <- 1
d$should <- 0 
d$should[d$selected_modal=='should'] <- 1
d$stronger_modal <- 'will'
d$stronger_modal[d$modal_pair=='could-should'] <- 'should'
d$stronger_modal[d$modal_pair=='can-could'] <- 'could'

d <- d[, !names(d) %in% c('participant.side', 'response_29Left_16_right')]
d$prob.label <- '20%\nProbability'
d$prob.label[d$proportion==80] <- '80%\nProbability'
d$prob.label[d$proportion==100] <- '100%\nProbability'
write.csv(d, 'dev_modals_data.csv')

table(d$age_group)
