# Holmusk Task
This directory provides all the code and documentation for the technical interview task. 

The task requires use of synthetic data, provided by Holmusk, to understand the treatment of major depressive disorder.

The data provided consists of 4 files:
1. demographics (demographics.csv)
2. secondary care data (clinical_data.csv)
3. linkage data between admission and billing (bill_id.csv)
4. billing amounts (bill_amount.csv)

Data can be linked across all files using either patient ID or billing ID and date of admission.

## Initial observations
### Data Strengths:
1. Coverage over risk factors, i.e. age, BMI, gender, race(?), comorbidities, treatments and symptoms, GAF
2. Multiple outcomes to consider: Length of stay, cost of stay, CGI-S at arrival, change in CGI-S arrival to discharge 

### Data limitations: 
1. Date of MDD diagnosis is not available.
2. Data only representative of only those who experienced a hospitalisation.
3. Missing follow up dates, therefore unable to determine whether only 1 hospitalisation for majority because of loss of follow up or not.
4. Comorbidities and treatments lack granularity (i.e. binary classification within last 24 months rather than date diagnosed/prescribed, respectively) but recent changes in treatment (<1 month) could be indicative of instability and not representative of effectiveness of medication.
5. Details of admission are not provided, e.g. treatment during admission, primary complaint/reason for admission.

## Analysis process
1. Clean and standardize individual data files (*_clean.R)
2. Join data and select cohort of interest (*_selection.R)
3. Analyse baseline demographics to summarise the population of interest (*_summary.R)
4. Perform main and sensitivity analyses (*_analyses.R)

## Outputs
The analyses will be written into an accompanying presentation which will explain the research question, provide the study design, approach, results and key findings.

