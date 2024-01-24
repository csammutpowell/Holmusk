# perform modelling with outcomes to determine associations between risk factors
library(tidyverse)
library(questionr)

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
               I(prescribing=='Polypharmacy') +
               symptom_count +
               I(prev_hosps>0)+
               baseline_cgi_high+
               gaf_regrouped,
             data=data_imp, family=binomial)

summary(model)
odds.ratio(model)

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
odds.ratio(model_lit)

# repeat for sensitivity analyses with varying outcomes

# 60 days hospitalisation
# full model first:

model_60 <- glm(outcome_cgi_hosp60d ~ I((age_at_admission-40)/10) +
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
               I(prescribing=='Polypharmacy') +
               symptom_count +
               I(prev_hosps>0)+
               baseline_cgi_high+
               gaf_regrouped,
             data=data_imp, family=binomial)

summary(model_60)
odds.ratio(model_60)

# risk factors for TRD from literature:
model_lit_60 <- glm(outcome_cgi_hosp60d ~ I((age_at_admission-40)/10) +
                   gender +
                   hypertension + 
                   anxiety + 
                   sud_imp + 
                   symptom_count +
                   I(prev_hosps>0)+
                   baseline_cgi_high,
                 data=data_imp, family=binomial)

summary(model_lit_60)
odds.ratio(model_lit_60)

# no hospitalisation restriction
# full model first:

model_cgionly <- glm(outcome_cgi_only ~ I((age_at_admission-40)/10) +
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
               I(prescribing=='Polypharmacy') +
               symptom_count +
               I(prev_hosps>0)+
               baseline_cgi_high+
               gaf_regrouped,
             data=data_imp, family=binomial)

summary(model_cgionly)
odds.ratio(model_cgionly)

# risk factors for TRD from literature:
model_lit_cgionly <- glm(outcome_cgi_only ~ I((age_at_admission-40)/10) +
                   gender +
                   hypertension + 
                   anxiety + 
                   sud_imp + 
                   symptom_count +
                   I(prev_hosps>0)+
                   baseline_cgi_high,
                 data=data_imp, family=binomial)

summary(model_lit_cgionly)
odds.ratio(model_lit_cgionly)
