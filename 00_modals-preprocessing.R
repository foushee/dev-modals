###############################################################################
### Epistemic Modals in Children & Adults 
### Study ID 45.0, Language & Cognitive Development Lab
### Ruthe Foushee
### Preprocessing: merging child and adult data, coercing to longform
### Updated 04 April 2021 in prep for Jeff Anderson's UCB Undergraduate Thesis
###############################################################################

### Read in csvs, match cols
c <- read.csv('data_anonymized/matlab-export_anonymized/Gen. 2 FULL Children.csv')
c$age_group <- 'child'
c$age_year <- c$X
c <- c[, !names(c) %in% c('X')]

a <- read.csv('data_anonymized/matlab-export_anonymized/Gen. 2 FULL Adults.csv')
a$age_group <- 'adult'
a$age_year <- 'Adult'

### New df with both children and adults
d <- rbind(c, a)

### New variables
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
d$stronger_modal[d$modal_pair=='can-could'] <- NA

d$stronger[d$stronger_modal!= d$selected_modal] <- 0
d$stronger[d$stronger_modal==d$selected_modal] <- 1
d$stronger[d$modal_pair=='can-could'] <- NA

d <- d[, !names(d) %in% c('participant.side', 'response_29Left_16_right')]
d$prob.label <- '20%\nProbability'
d$prob.label[d$proportion==80] <- '80%\nProbability'
d$prob.label[d$proportion==100] <- '100%\nProbability'

d$adult_modal <- 'no-consensus'
d$adult_modal[d$modal_pair=='could-should' & d$proportion==20] <- 'could'
d$adult_modal[d$modal_pair=='could-will' & d$proportion==20] <- 'could'
d$adult_modal[d$modal_pair=='could-will' & d$proportion==80] <- 'could'
d$adult_modal[d$modal_pair=='should-will' & d$proportion==80] <- 'should'
d$adult_modal[d$modal_pair=='could-should' & d$proportion==100] <- 'should'
d$adult_modal[d$modal_pair=='could-will' & d$proportion==100] <- 'will'
d$adult_modal[d$modal_pair=='should-will' & d$proportion==100] <- 'will'

d$adultlike[d$adult_modal==d$selected_modal] <- 1
d$adultlike[d$adult_modal!=d$selected_modal] <- 0
d$adultlike[d$adult_modal=='no-consensus'] <- NA

# Write new csv for use in analyses
write.csv(d, 'dev_modals_data.csv')

jeff_data <- read.csv('data_anonymized/Gen+3.+Survey_September+22,+2022_13.07.csv')[-1,]
names(jeff_data)
jeff_data$partID <- jeff_data$Subject.Number
jd <- jeff_data[c(20, 33:77)]

j <- jd %>% gather(trial_name, response, -partID, -age) 

### New variables
j$stimulus <- j$trial_name
j$proportion <- gsub('[_LR1_tcouldwishan2.jpg]+$', '', j$stimulus)
j$proportion <- gsub('X', '', j$proportion)
j$proportion <- as.numeric(as.character(j$proportion))
j$modal_pair <- gsub('^[X1208_]*', '', j$stimulus)
j$modal_pair <- gsub('_[LR]*_[12]*$', '', j$modal_pair)
j$left_modal <- gsub('_[shouldcaniwt]*$', '', j$modal_pair)
j$right_modal <- gsub('^[shouldcaniwt]*_', '', j$modal_pair)
j$trial_number <- gsub('^[shouldcaniwt8012X_]*_', '', j$stimulus)
j$modal_pair[j$modal_pair %in% c('wont_could', 'could_wont')] <- 'wont-could'
j$modal_pair[j$modal_pair %in% c('wont_should', 'should_wont')] <- 'wont-should'
j$modal_pair[j$modal_pair %in% c('should_will', 'will_should')] <- 'should-will'
j$modal_pair[j$modal_pair %in% c('could_will', 'will_could')] <- 'could-will'
j$modal_pair[j$modal_pair %in% c('could_should', 'should_could')] <- 'could-should'
j <- j %>% filter(response=='On')

j$selected_modal <- j$left_modal
j$selected_modal[j$trial_number %in% c('L_2', 'R_2')] <- j$right_modal[j$trial_number %in% c('L_2', 'R_2')]
j$will <- 0
j$will[j$selected_modal=='will'] <- 1
j$would <- 0
j$would[j$selected_modal=='would'] <- 1
j$wont <- 0
j$wont[j$selected_modal=='wont'] <- 1
j$could <- 0
j$could[j$selected_modal=='could'] <- 1
j$should <- 0 
j$should[j$selected_modal=='should'] <- 1
j$stronger_modal <- 'will'
j$stronger_modal[j$modal_pair=='could-should'] <- 'should'
j$stronger_modal[j$modal_pair=='wont-should'] <- 'should'
j$stronger_modal[j$modal_pair=='wont-could'] <- 'could'

j$stronger[j$stronger_modal!= j$selected_modal] <- 0
j$stronger[j$stronger_modal==j$selected_modal] <- 1

#j <- j[, !names(j) %in% c('participant.side', 'response_29Left_16_right')]
j$prob.label <- '0%\nProbability'
j$prob.label[j$proportion==20] <- '20%\nProbability'
j$prob.label[j$proportion==80] <- '80%\nProbability'
j$prob.label[j$proportion==100] <- '100%\nProbability'

j$adult_modal[j$modal_pair=='could-should' & j$proportion==20] <- 'could'
j$adult_modal[j$modal_pair=='could-should' & j$proportion==80] <- 'should'
j$adult_modal[j$modal_pair=='could-should' & j$proportion==100] <- 'should'

j$adult_modal[j$modal_pair=='could-will' & j$proportion==20] <- 'could'
j$adult_modal[j$modal_pair=='could-will' & j$proportion==80] <- 'could'
j$adult_modal[j$modal_pair=='could-will' & j$proportion==100] <- 'will'

j$adult_modal[j$modal_pair=='should-will' & j$proportion==80] <- 'should'
j$adult_modal[j$modal_pair=='should-will' & j$proportion==100] <- 'will'

j$adult_modal[j$modal_pair=='wont-could' & j$proportion==0] <- 'wont'
j$adult_modal[j$modal_pair=='wont-could' & j$proportion==20] <- 'could'

j$adult_modal[j$modal_pair=='wont-should' & j$proportion==0] <- 'wont'

j$adultlike[j$adult_modal==j$selected_modal] <- 1
j$adultlike[j$adult_modal!=j$selected_modal] <- 0

j$age_group <- j$age
j$age_group[j$age_group!='adult'] <- 'child'
j$age[j$age_group=='adult'] <- 20
j$age <- as.numeric(as.character(j$age))
j$age_year <- round(j$age, 0)
j <- j[j$age>0, ]
j$age_bin <- '6-7.5'
j$age_bin[j$age>=7.5] <- '7.5-9'
j$age_bin[j$age>=9] <- '9-10.5'
j$age_bin[j$age>11] <- 'Adult'
j$age_group[j$age>11] <- 'adult'

write.csv(j, 'data_anonymized/dev_modals_exp3_data.csv')
j[j$proportion==0,]$selected_modal
