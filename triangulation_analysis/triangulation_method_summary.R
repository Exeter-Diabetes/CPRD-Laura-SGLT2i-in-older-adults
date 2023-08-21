#
# Triangulation method summary -------------------------------------------------
#

#
## Multivariable regression (unmatched) ----------------------------------------
#

source("MVR.R")

### Save the results ----

save(list = paste0("MVR_",which_outcome,"_model_summary"), file = paste0(result_path,"/MVR_",which_outcome,"_modelsummary_",year_type,"_c",censoring_type,"_",population_type,".Rdata")) 


### clean workspace ----

rm(list=setdiff(ls(), c(dont_delete, "dont_delete")))

#
## Propensity score matched multivariable regression ---------------------------
#

source("PSM.R")

### Save the results ----

save(list = paste0("PSM_",which_outcome,"_model_summary"), file = paste0(result_path,"/PSM_",which_outcome,"_modelsummary_",year_type,"_c",censoring_type,"_",population_type,".Rdata")) 

### clean workspace ----

rm(list=setdiff(ls(), c(dont_delete, "dont_delete")))

#
## Instrumental variable method (ppIV previous patient) ------------------------
#

source("IV_prevpatient.R")

### Save the results ----

save(list = paste0("IV_prevpatient_",which_outcome,"_model_summary"), file = paste0(result_path,"/IV_prevpatient_",which_outcome,"_modelsummary_",year_type,"_c",censoring_type,"_",population_type,".Rdata")) 
save(list = paste0("IV_prevpatient_",which_outcome,"_model1"),        file = paste0(result_path,"/IV_prevpatient_",which_outcome,"_model1_",year_type,"_c",censoring_type,"_",population_type,".Rdata")) 

### clean workspace ----

rm(list=setdiff(ls(), c(dont_delete, "dont_delete")))

#
## Instrumental  variable method (by Ertefaie et al. 2017) ---------------------
#

source("IV_Ertefaie.R")

### Save the results ----

save(list = paste0("IV_Ertefaie_",which_outcome,"_model_summary"), file = paste0(result_path,"/IV_Ertefaie_",which_outcome,"_modelsummary_",year_type,"_c",censoring_type,"_",population_type,".Rdata")) 
save(list = paste0("IV_Ertefaie_",which_outcome,"_model1"),        file = paste0(result_path,"/IV_Ertefaie_",which_outcome,"_model1_",year_type,"_c",censoring_type,"_",population_type,".Rdata")) 

# code only for sensitivity analysis
#save(list = paste0("IV_Ertefaie_",which_outcome,"_model_summary"), file = paste0(result_path,"/IV_Ertefaie_",which_outcome,"_modelsummary_notwicecohort_",year_type,"_c",censoring_type,"_",population_type,".Rdata")) 
#save(list = paste0("IV_Ertefaie_",which_outcome,"_model1"),        file = paste0(result_path,"/IV_Ertefaie_",which_outcome,"_model1_notwicecohort_",year_type,"_c",censoring_type,"_",population_type,".Rdata")) 


### clean workspace ----

rm(list=setdiff(ls(), c(dont_delete, "dont_delete")))





