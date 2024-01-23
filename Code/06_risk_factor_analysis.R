# perform modelling with outcomes to determine associations between risk factors
library(tidyverse)

data <- readRDS('./Data/Derived/data_with_outcome.RDS')

# impute missing data:
# strategy 1: impute with mode
data_imp <- data %>%
  mutate(tumour_imp = ifelse(is.na(tumour),0,tumour),
         sud_imp = ifelse(is.na(substance_use),0,substance_use))

# full model first:

model <- glm(outcome_cgi_hosp1y ~ I((age_at_admission-40)/10) +
               race +
               I((BMI-25)/10) +
               I((BMI-25)/10)^2 +
               resident_status+
               gender +
               diabetes +
               hypertension + 
               tumour_imp + 
               anxiety + 
               mood +
               sud_imp + 
               trt_the+
               prescribing +
               symptom_count +
               I(prev_hosps>0)+
               baseline_cgi_high+
               gaf_regrouped,
             data=data_imp, family=binomial)

summary(model)

# risk factors for TRD from literature:
model_lit <- glm(outcome_cgi_hosp1y ~ I((age_at_admission-40)/10) +
               gender +
               hypertension + 
               anxiety + 
               sud_imp + 
               symptom_count +
               I(prev_hosps>0)+
               baseline_cgi_high,
             data=data_imp, family=binomial)

summary(model_lit)
