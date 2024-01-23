####################################################
# Join data for analysis
####################################################

library(tidyverse)
library(furniture)
library(reshape2)

#load cleaned data and relabel id var so consistent across all
admissions <- readRDS('./Data/Derived/admissions_clean.RDS')%>%
  rename(patient_id = id)
demographics <- readRDS('./Data/Derived/demographics_clean.RDS')
costs <- readRDS('./Data/Derived/admission_total_costs.RDS')

# join data
data <- left_join(left_join(admissions,costs),demographics)

# perform patient selection to cohort of interest, i.e. at least one year follow up from admission:
data_selected <- data%>%
  filter(date_of_discharge<=as.Date(max(data$date_of_admission)-years(1)) &
           date_of_admission>=as.Date(min(data$date_of_admission)+years(1)))

# create a variable to count number of hospitalisations in the previous 12 months
data_selected$prev_hosps <- 0
for (i in 1:nrow(data_selected)){
  data_selected$prev_hosps[i] <- data%>%filter(patient_id==data_selected$patient_id[i] &
                                                 date_of_admission<data_selected$date_of_admission[i] &
                                                 date_of_discharge>(data_selected$date_of_admission[i]-years(1)))%>%
    nrow()
}

# create a variable to determine whether there was a subsequent hospitalisation within 1 year
data_selected$readmitted_1yr <- 0
for (i in 1:nrow(data_selected)){
  data_selected$readmitted_1yr[i] <- data%>%filter(patient_id==data_selected$patient_id[i] &
                                                 date_of_admission>data_selected$date_of_discharge[i] &
                                                 date_of_admission<(data_selected$date_of_discharge[i]+years(1)))%>%
    nrow()
}


# create a variable to determine whether there was a subsequent hospitalisation within 60 days
data_selected$readmitted_60 <- 0
for (i in 1:nrow(data_selected)){
  data_selected$readmitted_60[i] <- data%>%filter(patient_id==data_selected$patient_id[i] &
                                                     date_of_admission>data_selected$date_of_discharge[i] &
                                                     date_of_admission<(data_selected$date_of_discharge[i]+days(60)))%>%
    nrow()
}

saveRDS(data_selected,'./Data/Derived/selected_admissions.RDS')


