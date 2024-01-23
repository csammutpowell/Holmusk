# characteristics of TR admissions vs no-TR admissions

library(tidyverse)
library(furniture)

data <- readRDS('./Data/Derived/data_with_outcome.RDS')%>%
  mutate(los=as.numeric(los))

# investigate los and costs associated with prescribing
table1(data,
       los,
       cost,
       splitby = ~prescribing,total = T,
       FUN2 = function(x){paste0(round(quantile(x,0.5),1),' (',round(quantile(x, 0.25),1),',',round(quantile(x, 0.75),1),')')},
       second = c('los','cost') 
)

# investigate los and costs associated with cgis (improvement of >1 or below 4)
table1(data,
       los,
       cost,
       splitby = ~outcome_cgi_only,total = T,
       FUN2 = function(x){paste0(round(quantile(x,0.5),1),' (',round(quantile(x, 0.25),1),',',round(quantile(x, 0.75),1),')')},
       second = c('los','cost') 
)