####################################################
# Data Cleaning for clinical_data.csv
####################################################

library(tidyverse)

# Read in clinical data
admissions <- read_csv('./Data/Original/clinical_data.csv')

# First check the class of each of the fields
str(admissions)

# Notes: 
# dates need to be converted
# inconsistency in medical_history_hbp compared to other medical_history_* fields

# Change dates to be date format
admissions <- admissions %>% 
  mutate(date_of_admission = dmy(date_of_admission),
         date_of_discharge = dmy(date_of_discharge))

# check distribution of admissions (counts in 30d groups) for discontinuity
ggplot(admissions,aes(x=date_of_admission))+
  geom_histogram(binwidth = 30)

# no discontinuity observed

# check to see whether all admissions are distinct for an individual patient
# note only need to check those who had multiple admissions
recur_adm_ids <- admissions %>%
  group_by(id)%>%
  summarise(N=n())%>%
  filter(N>1)%>%
  pull(id)

recur_admission <- admissions %>% filter(id %in% recur_adm_ids)%>%
  arrange(id,date_of_admission)

recur_admission$overlap<-0
for(i in 2:nrow(recur_admission)){
  recur_admission$overlap[i]<- ifelse(recur_admission$id[(i-1)]!=recur_admission$id[i],0,
                                      ifelse(recur_admission$date_of_discharge[(i-1)]>recur_admission$date_of_admission[i],1,0))
}

#review those with overlaps:
View(recur_admission[sort(c(which(recur_admission$overlap==1)-1,which(recur_admission$overlap==1))),])

# inconsistent data between entries on med hist and trt
# check for more entries from these patients:

check_ids <- recur_admission[which(recur_admission$overlap==1),]$id
admissions %>% 
  filter(id %in% check_ids)

# all entries are those with overlap. Therefore remove all these entries.
admissions <- admissions %>% 
  filter(!id %in% check_ids)

# create field for length of stay:
admissions <- admissions %>%
  mutate(los = difftime(date_of_discharge,date_of_admission,units = 'days'))

# check distribution of los for sense check
ggplot(admissions,aes(x=los))+
  geom_histogram(binwidth = 1)

# values seem plausible

# Check medical history fields
# Where appropriate, convert medical history fields to be binary (0 = No, 1 = Yes)

# First find all unique values in medical_history_hbp:
admissions %>% pull(medical_history_hbp)%>%unique()

# Note: mixture of text and numeric labelling
# Convert text labelling to numeric:
admissions <- admissions%>%
  mutate(medical_history_hbp = case_when(medical_history_hbp == '0' ~ 0,
                                          medical_history_hbp == '1' ~ 1,
                                          medical_history_hbp == 'No' ~ 0,
                                          medical_history_hbp == 'Yes' ~ 1))

# Now check unique values for each of the medical_history fields
lapply(admissions%>%select(starts_with('medical_history')),unique)

# All appear okay - proceed to treatment columns

# Now check unique values for each of the treatment fields
lapply(admissions%>%select(starts_with('trt')),unique)

# All appear okay - proceed to symptom columns

# Now check unique values for each of the symptom fields
lapply(admissions%>%select(starts_with('symptom_')),unique)

# All appear okay - proceed to check ranges of weight and height columns
admissions%>%pull(weight)%>%summary()
admissions%>%pull(height)%>%summary()

# ranges appear reasonable, now check BMI

# Create BMI variable
admissions <- admissions %>%
  mutate(BMI = weight/((height/100)^2))

# Check range of BMI:
admissions%>%pull(BMI)%>%summary()

# Range seems reasonable
# Note: would usually expect median to be lower in healthy population for region but may be reasonable because of selected population 
# i.e. patients with MDD that were admitted to secondary care 

# Move on to CGI-S fields
# Check values:
lapply(admissions %>% select(starts_with('cgi')), unique)

# All within appropriate range
# Check for monotonicity in change:

# Create new variable for improvement in CGI-S
admissions <- admissions%>%
  mutate(cgis_imp = cgis_adm-cgis_dis)

# Check score change:
admissions %>% pull(cgis_imp) %>% table()

# Note: negative values are concerning but not implausible - patient could be transferred to specialist centre on discharge.

# From  doi: 10.2147/NDT.S358367 :
# Patients with 
#     1. ≥2 points decrease from baseline, or 
#     2. a CGI-S score of ≤3 (mildly depressed to normal),
# were considered responders to prescribing
# based on the cut-off described previously in the literature

# Consider whether length of stay is correlated with change in cgis for sense check:
ggplot(admissions,aes(x=cgis_imp,y=los))+
  geom_point()

# No stand out features observed

# Check gaf score:
admissions %>% pull(gaf_lv) %>% table()

# all within range

# save clean version of data

saveRDS(admissions,'./Data/Derived/admissions_clean.RDS')
