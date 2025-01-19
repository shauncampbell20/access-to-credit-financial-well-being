#### Shaun Campbell
#### COSC 6520 Project 1
#### Part 3: Analysis

# Clear working director
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load required packages
library(data.table)
library(ggplot2)
library(caret)
library(randomForest)

# Read in FWBS data (only columns of interest)
fwb <- fread(file.path('./data', 'NFWBS_PUF_2016_data.csv'), header = TRUE,
             select = c('PPMSACAT','PPREG4','PPREG9','PPETHM','PPGENDER','agecat','PPINCIMP','HOUSERANGES','SAVINGSRANGES','ENDSMEET','FWBscore',
               'MANAGE1_3','REJECTED_1','REJECTED_2','VALUERANGES','MORTGAGE','PPMARIT','MILITARY','PCTLT200FPL'))

# Metro indicator
fwb$MSA <- 'Non-Metro'
fwb[which(fwb$PPMSACAT == 1),'MSA'] <- 'Metro'

# Name the levels of the census region variable
fwb$census_region <- ifelse(fwb$PPREG4 == 1, 'Northeast',
                            ifelse(fwb$PPREG4 == 2, 'Midwest',
                                   ifelse(fwb$PPREG4 == 3, 'South','West')))

# Name the levels of the census division variable
fwb$census_division <- ifelse(fwb$PPREG9 == 1, 'New England',
                            ifelse(fwb$PPREG9 == 2, 'Mid-Atlantic',
                                   ifelse(fwb$PPREG9 == 3, 'East-North Central',
                                          ifelse(fwb$PPREG9 == 4, 'West-North Central',
                                                 ifelse(fwb$PPREG9 == 5, 'South Atlantic',
                                                        ifelse(fwb$PPREG9 == 6, 'East-South Central',
                                                               ifelse(fwb$PPREG9 == 7, 'West-South Central',
                                                                      ifelse(fwb$PPREG9 == 8, 'Mountain','Pacific'))))))))

# Name the levels of the race/ethnicity variable
fwb$race_ethnicity <- ifelse(fwb$PPETHM == 1, 'White, Non-Hispanic',
                             ifelse(fwb$PPETHM == 2, 'Black, Non-Hispanic',
                                    ifelse(fwb$PPETHM == 3, 'Other, Non-Hispanic','Hispanic')))

# Name the levels of the gender variable
fwb$gender <- ifelse(fwb$PPGENDER == 1, 'Male', 'Female')

# Name the levels of the age variable
# Combine some levels to align with LAR data
fwb$age <- ifelse(fwb$agecat == 1, '18-24',
                  ifelse(fwb$agecat == 2, '25-34',
                         ifelse(fwb$agecat == 3, '35-44',
                                ifelse(fwb$agecat == 4, '45-54',
                                       ifelse(fwb$agecat %in% c(5,6,7), '55-74','75+')))))

# Name the levels of the income variable
# Combine some levels to align with LAR data
fwb$income1 <- ifelse(fwb$PPINCIMP %in% c(1,2,3), 'Less than $39,999',
                     ifelse(fwb$PPINCIMP == 4, '$40,000 to $49,999',
                            ifelse(fwb$PPINCIMP == 5, '$50,000 to $59,999',
                                   ifelse(fwb$PPINCIMP == 6, '$60,000 to $74,999',
                                          ifelse(fwb$PPINCIMP == 7, '$75,000 to $99,999',
                                                 ifelse(fwb$PPINCIMP == 8, '$100,000 to $149,999','$150,000 or more'))))))

# DTI not used - no good way to determine it
fwb$dti1 <- ''

# Use savings amount to approximate the maximum amount one could use for a down payment
fwb$down_payment1 <- ifelse(fwb$SAVINGSRANGES %in% c(1,2,3,4), '$0-4,999',
                           ifelse(fwb$SAVINGSRANGES == 5, '$5,000-19,999',
                                  ifelse(fwb$SAVINGSRANGES == 6, '$20,000-74,999',
                                         ifelse(fwb$SAVINGSRANGES == 7,'$75,000 or more', 'Unknown'))))

# Rename levels for military status
fwb$military <- ifelse(fwb$MILITARY == 1, 'Yes','No')

# Rename levels for marital status
fwb$married <- ifelse(fwb$PPMARIT == 1, 'Yes','No')

# Create some indicators for further analysis
# Whether person has a credit card, whether they were rejected for credit in the past
# Whether they own a home, and whether they have credit in general (credit card or own home)
fwb$have_cc <- ifelse(fwb$MANAGE1_3 == 1, 'No', 
                      ifelse(fwb$MANAGE1_3 == -1, 'Unknown', 'Yes'))
fwb$rejected <- as.factor(ifelse(fwb$REJECTED_1 == 1, 0, 1))
fwb$own_home <- ifelse(fwb$MORTGAGE == -2, 'No', 'Yes')
fwb$have_credit <- ifelse(fwb$have_cc == 'Yes' | fwb$own_home == 'Yes', 'Yes','No')

# Select column subset
fwb <- fwb[,c('MSA','census_region','census_division','race_ethnicity','gender','age','income1','dti1','down_payment1','military','married',
              'have_cc','rejected','own_home','have_credit','FWBscore','PCTLT200FPL')]

# Load the random forest model
rf_model <- readRDS(file.path('./models', 'rf_model.rds'))

# Predict probability of approved using RF model and add probabilities as columns
# Set predicted class column using cutoff of 0.125
newpreds <- predict(rf_model, fwb, type = "prob")
fwb <- cbind(fwb, newpreds)
names(fwb)[names(fwb) == '1'] <- 'prob_approved'
names(fwb)[names(fwb) == '0'] <- 'prob_denied'
fwb$approved_class_pred <- as.factor(ifelse(fwb$prob_approved >= 0.125, 1, 0))

# Evaluate performance of negative predictions using rejected indicator
cm <- confusionMatrix(fwb$approved_class_pred, fwb$rejected, positive = '1')
cm
cm[["byClass"]]
461/(461+181)

# Evaluate performance of positive predictions using own home predictor
cm <- confusionMatrix(fwb$approved_class_pred, as.factor(ifelse(fwb$own_home=='Yes',1,0)), positive = '1')
cm
cm[["byClass"]]
2868/(2868+1296)

# Regression model of FWBscore on approval probability
mod <- lm(FWBscore ~ prob_approved, data=fwb)
summary(mod)

# Regression model of FWBscore on income and down payment (savings)
mod2 <- lm(FWBscore ~ income1+down_payment1, data=fwb)
summary(mod2)

mod_black_non_hispanic <- lm(FWBscore ~ prob_approved, data=fwb[fwb$race_ethnicity == 'Black, Non-Hispanic',])
summary(mod_black_non_hispanic)

mod_white_non_hispanic <- lm(FWBscore ~ prob_approved, data=fwb[fwb$race_ethnicity == 'White, Non-Hispanic',])
summary(mod_white_non_hispanic)

mod_hispanic <- lm(FWBscore ~ prob_approved, data=fwb[fwb$race_ethnicity == 'Hispanic',])
summary(mod_hispanic)

mod_other_non_hispanic <- lm(FWBscore ~ prob_approved, data=fwb[fwb$race_ethnicity == 'Other, Non-Hispanic',])
summary(mod_other_non_hispanic)

mod_lt40 <- lm(FWBscore ~ prob_approved, data=fwb[fwb$income1 == 'Less than $39,999',])
summary(mod_lt40)

mod_gt150 <- lm(FWBscore ~ prob_approved, data=fwb[fwb$income1 == '$150,000 or more',])
summary(mod_gt150)

# Mean FWBscores and approval probabilities by race/ethnicity
for (x in c('Other, Non-Hispanic','White, Non-Hispanic','Hispanic','Black, Non-Hispanic')) {
  cat(x, mean(fwb[fwb$race_ethnicity == x,]$FWBscore), mean(fwb[fwb$race_ethnicity == x,]$prob_approved),'\n')
}

# Mean FWBscores and approval probabilities by income
names(fwb)[names(fwb) == 'income1'] <- 'Income'
for (x in c('Less than $39,999','$40,000 to $49,999','$50,000 to $59,999','$60,000 to $74,999','$75,000 to $99,999','$100,000 to $149,999','$150,000 or more')) {
  cat(x, mean(fwb[fwb$Income == x,]$FWBscore),mean(fwb[fwb$Income == x,]$prob_approved),'\n')
}

# Mean FWBscores and approval probabilities by savings
for (x in c('$0-4,999','$20,000-74,999','$5,000-19,999','$75,000 or more','Unknown')) {
  cat(x, mean(fwb[fwb$down_payment1 == x,]$FWBscore),mean(fwb[fwb$down_payment1 == x,]$prob_approved),'\n')
}

# Scatter plot of FWBscore against approval probability
ggplot(fwb, aes(x=prob_approved, y = FWBscore)) + geom_point(col='blue', alpha=0.3, position='jitter') + 
  geom_smooth(method="lm", linewidth=2, col='darkblue') +
  labs(x = "Approval Probability", y = "FWB Score", title = "Financial Well-Being vs Credit Approval Probability") +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(hjust = 0.6), 
  ) 

# Scatter plot of FWBscore against approval probability by credit status
ggplot(fwb[fwb$prob_approved<=1&fwb$prob_approved>=0], aes(x=prob_approved, y = FWBscore, color=have_credit)) + 
  geom_point(alpha=.5, position='jitter') +
  geom_smooth(method='lm', linewidth=2) +
  labs(x = "Approval Probability", y = "FWB Score", title = "FWB vs Approval Probability by Credit Status") +
  theme(
    text = element_text(size = 20),
    legend.position = 'bottom',
    plot.title = element_text(hjust = 0.6), 
  ) 

# Scatter plot of FWBscore against approval probability by income
ggplot(fwb, aes(x=prob_approved, y = FWBscore, color=Income)) + 
  geom_point(alpha=.5, position='jitter') +
  geom_smooth(method='lm', linewidth=1) +
  labs(x = "Approval Probability", y = "FWB Score", title = "FWB vs Approval Probability by Income") +
  theme(
    text = element_text(size = 12),
    legend.position = 'bottom',
    plot.title = element_text(hjust = 0.6), 
  ) 

# Scatter plot of FWBscore against approval probability by race/ethnicity
ggplot(fwb, aes(x=prob_approved, y = FWBscore, color=race_ethnicity)) +
  geom_point(alpha=.5, position='jitter') + geom_smooth(method="lm", linewidth=2) +
  labs(x = "Approval Probability", y = "FWB Score", title = "FWB vs Approval Probability by Race/Ethnicity") +
  theme(
    text = element_text(size = 12),
    legend.position = 'bottom',
    plot.title = element_text(hjust = 0.6), 
  ) 

g1 = fwb[which(fwb$have_credit == 'Yes'), ]$FWBscore
g2 = fwb[which(fwb$have_credit == 'No'), ]$FWBscore
t.test(g1, g2, var.equal = TRUE)
