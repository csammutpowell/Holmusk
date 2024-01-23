####################################################
# Review billing data
####################################################


library(tidyverse)

bill_id <- read_csv('./Data/Original/bill_id.csv')
bill_amount <- read_csv('./Data/Original/bill_amount.csv')

# Notes: odd that amount is given to 4 dp - ignore for now.

# check range of amounts for any anomalies:
bill_amount%>%pull(amount)%>%summary()
ggplot(bill_amount,aes(x=amount))+
  geom_histogram(binwidth = 100)

# No negative amounts
# Some very costly admissions but plausible - good to check against length of stay as sense check

# join data
bill_data <- left_join(bill_id,bill_amount)

# check for multiple entries per admission:
bill_data %>%
  group_by(patient_id,date_of_admission)%>%
  summarise(N=n())%>%
  pull(N)%>%
  table()

# every admission has 4 costs associated with it

# pool together to give total cost of admission
bill_total <- bill_data %>%
  group_by(patient_id,date_of_admission)%>%
  summarise(cost = sum(amount))

# check range of amounts for any anomalies:
bill_total%>%pull(cost)%>%summary()

# save total costs:
saveRDS(bill_total,'./Data/Derived/admission_total_costs.RDS')

