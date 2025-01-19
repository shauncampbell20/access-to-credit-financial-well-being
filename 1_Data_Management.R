#### Shaun Campbell
#### COSC 6520 Project 1
#### Part 1: Data Management

# Clear working directory
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load required packages
library(readxl)
library(data.table)

# Enable parallel processing in fread to make it faster
options(datatable.fread.use.names = TRUE)
options(datatable.num.threads = parallel::detectCores())

# Read LAR data - only read in the columns needed
# ***Sample data (100,000 rows)***
df <- fread(file.path('./data', "2018_public__lar_three_year_sample.csv"), header=TRUE,
            select = c('action_taken','loan_purpose','open_end_line_of_credit','business_or_commercial_purpose','occupancy_type',
                       'applicant_age','county_code','state_code','derived_ethnicity','derived_race','applicant_sex',
                       'income','debt_to_income_ratio','combined_loan_to_value_ratio','loan_amount','property_value',
                       'derived_loan_product_type','co_applicant_credit_score_type'))

names(df)[names(df) == 'debt_to_income_ratio'] <- 'dti'
names(df)[names(df) == 'combined_loan_to_value_ratio'] <- 'ltv'

# Only records where application was approved or denied 
# Only home purchase, not HELOC, not for business, for primary residence
# Remove records with an invalid age
df <- df[df$action_taken %in% c(1,2,3),] 
df$action_taken <- as.factor(ifelse(df$action_taken == 1, 1, ifelse(df$action_taken == 2, 1, 0)))
df <- df[which(df$loan_purpose == 1),]  #Home purchases
df <- df[which(df$open_end_line_of_credit == 2),] #Not HELOC
df <- df[which(df$business_or_commercial_purpose == 2),] #Not for business purpose
df <- df[which(df$occupancy_type == 1),] #Primary residence
df <- df[-which(df$applicant_age=='8888'),] #Valid ages

# Use county-msa crosswalk to set indicator for metropolitan areas
msa <- read_excel(file.path('./data', "qcew-county-msa-csa-crosswalk.xlsx"), sheet = "Feb. 2013 Crosswalk")
df$MSA <- 'Non-Metro'
df[which(df$county_code %in% msa$`County Code`),'MSA'] <- 'Metro'

# Use state to determine census regions
df$census_region <- 'Other'
df[which(df$state_code %in% c('CT','ME','MA','NH','RI','VT','NJ','NY','PA')),'census_region'] <- 'Northeast'
df[which(df$state_code %in% c('IN','IL','MI','OH','WI','IA','KS','MN','MO','NE','ND','SD')),'census_region'] <- 'Midwest'
df[which(df$state_code %in% c('DE','DC','FL','GA','MD','NC','SC','VA','WV',
                              'AL','KY','MS','TN','AR','LA','OK','TX')),'census_region'] <- 'South'
df[which(df$state_code %in% c('AZ','CO','ID','NM','MT','UT','NV','WY','AK','CA','HI','OR','WA')),'census_region'] <- 'West'
df <- df[-which(df$census_region == 'Other'),]

# Use state to determine census divisions
df$census_division <- 'Other'
df[which(df$state_code %in% c('CT','ME','MA','NH','RI','VT')),'census_division'] <- 'New England'
df[which(df$state_code %in% c('NJ','NY','PA')),'census_division'] <- 'Mid-Atlantic'
df[which(df$state_code %in% c('IN','IL','MI','OH','WI')),'census_division'] <- 'East-North Central'
df[which(df$state_code %in% c('IA','KS','MN','MO','NE','ND','SD')),'census_division'] <- 'West-North Central'
df[which(df$state_code %in% c('DE','DC','FL','GA','MD','NC','SC','VA','WV')),'census_division'] <- 'South Atlantic'
df[which(df$state_code %in% c('AL','KY','MS','TN')),'census_division'] <- 'East-South Central'
df[which(df$state_code %in% c('AR','LA','OK','TX')),'census_division'] <- 'West-South Central'
df[which(df$state_code %in% c('AZ','CO','ID','NM','MT','UT','NV','WY')),'census_division'] <- 'Mountain'
df[which(df$state_code %in% c('AK','CA','HI','OR','WA')),'census_division'] <- 'Pacific'

# Race and Ethnicity are combined in the FWBS data, so combine them here
# The possible combinations of race and ethnicity need to be reduced into one 
# of 4 categories to match the FWBS data: Hispanic, White, Non-Hispanic, Black, Non-Hispanic, and Other, Non-Hispanic
df[-which(df$derived_ethnicity %in% c('Hispanic or Latino','Not Hispanic or Latino')),'derived_ethnicity'] <- 'Unknown'
df[which(df$derived_ethnicity == 'Hispanic or Latino'),'race_ethnicity'] <- 'Hispanic'
df[which(df$derived_ethnicity == 'Not Hispanic or Latino' & 
           df$derived_race == 'White'),'race_ethnicity'] <- 'White, Non-Hispanic'
df[which(df$derived_ethnicity == 'Not Hispanic or Latino' & 
           df$derived_race == 'Black or African American'),'race_ethnicity'] <- 'Black, Non-Hispanic'
df[which(df$derived_ethnicity == 'Not Hispanic or Latino' & 
           df$derived_race %in% c('2 or more minority races','American Indian or Alaska Native','Asian',
                                  'Native Hawaiian or Other Pacific Islander')),'race_ethnicity'] <- 'Other, Non-Hispanic'
df[-which(df$race_ethnicity %in% c('Hispanic','White, Non-Hispanic','Black, Non-Hispanic','Other, Non-Hispanic')),'race_ethnicity'] <- 'Unknown'

# Name factor levels for applicant sex
df$gender <- ifelse(df$applicant_sex == 1, 'Male',ifelse(df$applicant_sex == 2, 'Female', 'Unknown'))

# Rename and combine levels for age to match with FWBS data
df$age <- ifelse(df$applicant_age == '<25', '18-24',
                 ifelse(df$applicant_age == '25-34', '25-34',
                        ifelse(df$applicant_age == '35-44', '35-44',
                               ifelse(df$applicant_age == '45-54', '45-54',
                                      ifelse(df$applicant_age == '>74', '75+', '55-74')))))

# Bin income into categories that match the FWBS data
df <- df[-which(is.na(df$income)),]
df$income1 <- ifelse(df$income < 40, 'Less than $39,999',
                     ifelse(df$income < 50, '$40,000 to $49,999',
                            ifelse(df$income < 60, '$50,000 to $59,999',
                                   ifelse(df$income < 75, '$60,000 to $74,999',
                                          ifelse(df$income < 100, '$75,000 to $99,999',
                                                 ifelse(df$income < 150, '$100,000 to $149,999','$150,000 or more'))))))

# Debt to Income **(not ultimately used because no good corollary is present in the FWBS data)**
df$dti1 <- ifelse(df$dti %in% c('<20%','20%-<30%','30%-<36%'), '<36%',
                  ifelse(df$dti %in% c('36','37','38','39','40','41','42','43','44','45','46','47','48','49'),'36%-49%',
                         ifelse(df$dti %in% c('>60%','50%-60%'),'>50%','Unknown')))
df[df$dti1 == 'Unknown','dti1'] <- '36%-49%'

# Down payment approximated by value of property minus loan amount
# Replace missing values with median property value
# Down payment then binned to match level for savings amount in FWBS data
df$property_value <- as.numeric(df$property_value)
df$loan_amount <- as.numeric(df$loan_amount)
df[is.na(df$property_value),'property_value'] <- 245000
df$down_payment <- df$property_value - df$loan_amount
df$down_payment1 <- ifelse(df$down_payment < 5000, '$0-4,999',
                           ifelse(df$down_payment < 20000, '$5,000-19,999',
                                  ifelse(df$down_payment < 75000, '$20,000-74,999',
                                         ifelse(is.infinite(df$down_payment),'Unknown','$75,000 or more' ))))


# Marital status indicator created. Co-applicant credit score type of 10 translates to no co-applicant
# All other values have a co-applicant except for 1111 which is "exempt" 
# Exempt records are removed to avoid needing a small "unknown" category (none in sample data)
#df <- df[-which(df$co_applicant_credit_score_type == 1111),]
df$married <- ifelse(df$co_applicant_credit_score_type == 10, "No", "Yes")

# Military status indicator used based on whether the loan was a VA mortgage
df$military <- ifelse(df$derived_loan_product_type %in% c("VA:First Lien", "VA:Subordinate Lien"), "Yes", "No")

# Select columns that will be used for model creation
df1 <- df[,c('action_taken','MSA','census_division','census_region','race_ethnicity',
             'gender','age','income1','dti1','down_payment1','married','military')]

# Save the prepared data set to csv
write.csv(df1, file.path('./data', 'data.csv'))
