####################################################
# Data Cleaning for demographics.csv
####################################################


library(tidyverse)

# Read in demographic data
demographics <- read_csv('./Data/Original/demographics.csv')

# First check the class of each of the fields
str(demographics)

# Notes:
# all vars are strings apart from date
# gender and resident status definitely need cleaning, need to check race and date of birth

# cleaning 'gender':
# first determine all unique values in field
demographics%>%pull(gender)%>%unique()
demographics%>%pull(gender)%>%table()

# standardise entries as 'Male' or 'Female'
demographics <- demographics%>%
  mutate(gender = as.factor(case_when(gender == 'm' ~ 'Male',
                                         gender == 'f' ~ 'Female',
                                         gender == 'Male' ~ 'Male',
                                         gender == 'Female' ~ 'Female')))

# check counts
demographics%>%pull(gender)%>%table()

# cleaning 'race':
# first determine all unique values in field
demographics%>%pull(race)%>%unique()
demographics%>%pull(race)%>%table()

# only problem with {capitalisation of chinese} and {India missing n} creating non-standardised entries

# standardise entries as 'Indian'/'Chinese'/'Malay'/'Others'
demographics <- demographics%>%
  mutate(race = as.factor(ifelse(race=='chinese','Chinese',
                       ifelse(race=='India','Indian',race))))

# check counts
demographics%>%pull(race)%>%table()

# cleaning 'resident_status':
# first determine all unique values in field
demographics%>%pull(resident_status)%>%unique()
demographics%>%pull(resident_status)%>%table()

# 2 labels for citizen, relabel as per data dictionary

# standardise entries as 'Citizen'/'Permanent resident'/'Foreigner'
demographics <- demographics%>%
  mutate(resident_status = as.factor(case_when(resident_status == 'Singaporean' ~ 'Citizen',
                                    resident_status == 'Singapore citizen' ~ 'Citizen',
                                    resident_status == 'PR' ~ 'Permanent resident',
                                    resident_status == 'Foreigner' ~ 'Foreigner')))
         
# check counts
demographics%>%pull(resident_status)%>%table()

# check date_of_birth for implausible dates:
demographics%>%pull(date_of_birth)%>%summary()

ggplot(demographics,aes(x=date_of_birth))+
  geom_histogram()

# Given dates of admission range between 2011 and 2015, all ages at admission are >18
# Nothing suspicious observed in dob.

# save cleaned version of data
saveRDS(demographics,'./Data/Derived/demographics_clean.RDS')

