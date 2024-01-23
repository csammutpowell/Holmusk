####################################################
# Join data for analysis
####################################################

library(tidyverse)
library(furniture)
library(reshape2)

#load cleaned data 
data <- readRDS('./Data/Derived/selected_admissions.RDS')%>%
  mutate(age_at_admission=as.numeric(difftime(date_of_admission,date_of_birth,units='days')/365.25),
         diabetes = as.factor(medical_history_dia),
         hypertension = as.factor(medical_history_hbp),
         renal = as.factor(medical_history_ren),
         tumour = as.factor(medical_history_tum),
         anxiety = as.factor(medical_history_anx),
         mood = as.factor(medical_history_mood),
         substance_use = as.factor(medical_history_sud),
         trt_the = as.factor(trt_the),
         symptom_sleep = as.factor(symptom_1),
         symptom_anhedonia = as.factor(symptom_2),
         symptom_appetite = as.factor(symptom_3),
         symptom_depressed = as.factor(symptom_4),
         symptom_suicidal = as.factor(symptom_5),
         gaf_lv = as.factor(gaf_lv),
         cgis_adm = as.factor(cgis_adm))

# create treatment and prescribing grouping:
data <- data %>%
  mutate(presc_antidep = trt_adt==1 | trt_ssr==1,
         presc_non_antidep = trt_anx==1 | trt_con==1 | trt_oth==1,
         prescribing = case_when((presc_non_antidep==0) & (presc_antidep==0) ~ 'None',
                                 (presc_non_antidep==0) & (presc_antidep==1) ~ 'Antidepressant',
                                 (presc_non_antidep>0) & (presc_antidep==0) ~ 'Non-antidepressant',
                                 (presc_non_antidep==1) & (presc_antidep==1) ~ 'Polypharmacy'),
         treatment = ifelse(trt_the==1,paste0(prescribing,' + Therapy'),prescribing))

data%>%pull(prescribing)%>%table()
data%>%pull(treatment)%>%table()

# baseline table across all admissions
baseline_table_adm <- table1(data,
       age_at_admission,
       gender,
       BMI,
       race,
       resident_status,
       diabetes,
       hypertension,
       renal,
       tumour,
       anxiety,
       mood,
       substance_use,
       trt_the,
       symptom_sleep,
       symptom_anhedonia,
       symptom_appetite,
       symptom_depressed,
       symptom_suicidal,
       as.factor(prev_hosps),
       gaf_lv,
       cgis_adm,
       splitby=~prescribing, total=T, na.rm=F,
       FUN2 = function(x){paste0(round(quantile(x,0.5),1),' (',round(quantile(x, 0.25),1),',',round(quantile(x, 0.75),1),')')},
       second = 'BMI' 
)

write_csv(as.data.frame(baseline_table_adm),'./Outputs/baseline_table_admissions.csv')


####################################################
# create outcome variable and check event rate:

data <- data %>%
  mutate(outcome_cgi_only = 1-as.numeric(cgis_dis<=3 | (as.numeric(cgis_adm)-as.numeric(cgis_dis)>=2)),
         outcome_cgi_hosp1y = outcome_cgi_only==1 | readmitted_1yr>0,
         outcome_cgi_hosp60d = outcome_cgi_only==1 | readmitted_60>0,
         baseline_cgi_high = as.numeric(cgis_adm)>=4,
         gaf_regrouped = ifelse(gaf_lv == 8,0,
                                ifelse(gaf_lv %in% c(6,7),1,
                                       ifelse(gaf_lv %in% c(4,5),2,3))),
         symptom_count = as.numeric(symptom_sleep) + 
           as.numeric(symptom_anhedonia)+
           as.numeric(symptom_appetite) +
           as.numeric(symptom_depressed) + 
           as.numeric(symptome_suicidal))

data%>%select(outcome_cgi_only)%>%table()
data%>%select(outcome_cgi_hosp1y)%>%table()
data%>%select(outcome_cgi_hosp60d)%>%table()


saveRDS(data,'./Data/Derived/data_with_outcome.RDS')

#######################################################

# check for correlation between symptoms:
round(data%>%
        select(symptom_1,symptom_2,symptom_3,symptom_4,symptom_5)%>%
        cor(),3)
