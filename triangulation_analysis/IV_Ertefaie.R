#
# Instrumental variable method (by Ertefaie et al. 2017) -----------------------
#


## load needed data ----

load(paste0(data_path,"/study_cohort_",year_type,"_prepared.Rdata"))

#load(paste0(data_path,"/data_sa.Rdata"))                                        # code only for sensitivity analysis
#study_cohort_prepared <- data_sa                                                # code only for sensitivity analysis
#rm(data_sa)                                                                     # code only for sensitivity analysis

if(population_type == "study_population"){
  
  data <- study_cohort_prepared
  
  }else if(population_type == "not_elderly"){
    
    data <- study_cohort_prepared[study_cohort_prepared$elderly == 0, ]
    
    }else if(population_type == "elderly"){
      
      data <- study_cohort_prepared[study_cohort_prepared$elderly == 1, ]
      
      }else if(population_type == "study_population_female"){
        
        data <- study_cohort_prepared[study_cohort_prepared$gender == 2, ]
        
        }else{
          
          data <- study_cohort_prepared[study_cohort_prepared$gender == 1, ]
          
          }

rm(study_cohort_prepared)


## define variable set ----

source(paste0("variableset_", which_outcome,".R"))

## complete case data on the outcome variable ----

data_ccY <- data[complete.cases(data[ , Y]), ]


## data preparation step ----

# we need to exclude from this subset dataset all provider with less than 2 treated patients

enough_data <- as.numeric(names(which(table(data_ccY$pracid) >= 2) == TRUE))
data_rm     <- data_ccY[data_ccY$pracid%in%enough_data,  ]



## Step 1 ----


if(population_type == "study_population_female" | population_type == "study_population_male"){
  
  Ertefaie_model_step1_formula <- as.formula(paste(X, " ~ ", paste0(c(all_W_notgender, "(1|pracid)"), collapse =  " + ")))
  
}else{
  
  Ertefaie_model_step1_formula <- as.formula(paste(X, " ~ ", paste0(c(all_W, "(1|pracid)"), collapse =  " + ")))
  
}

Ertefaie_model_step1         <- glmer(Ertefaie_model_step1_formula, family = binomial("logit"), data = data_rm) # uses the complete case dataset

b_j_hat             <- ranef(Ertefaie_model_step1)$pracid$"(Intercept)"
b_j_hat_median      <- median((exp(b_j_hat))/(1 + exp(b_j_hat)))
IV_j_ePP            <- ifelse((exp(b_j_hat))/(1 + exp(b_j_hat)) > b_j_hat_median, 1, 0)

# as some pracid have NAs for all rows, no random effect is estimated
# this pracids need to be excluded from the data for step 2 of the method

pracid_raneff <- as.numeric(rownames(ranef(Ertefaie_model_step1)$pracid))       # these are the provider for which a random intercept has been estimated
data_ePP2     <- data_rm[data_rm$pracid%in%pracid_raneff,  ]  

# Merge IV data to cohort data

data_IV             <- data.frame(cbind(as.numeric((rownames(ranef(Ertefaie_model_step1)$pracid))), IV_j_ePP))
colnames(data_IV)   <- c("pracid", "IV_ePP")

data_merged         <- merge(data_ePP2, data_IV, by = "pracid")
data_ePP2           <- data_merged

## Step 2 ----

Z                <- "IV_ePP"

if(population_type == "study_population_female" | population_type == "study_population_male"){

  all_W_step2                 <- all_W_notgender[!all_W_notgender%in%variables_withNAs]

  }else{

    all_W_step2               <- all_W[!all_W%in%variables_withNAs]

    }


if(outcome_variable_type == "binary"){
  
  followup_time    <- data_ePP2[ , paste("followup_days_", which_outcome, "_c"  , censoring_type, sep = "")]
  
  }else{
  
    followup_time  <- NULL
    
    }


IV_Ertefaie_model1_formula     <- as.formula(paste(X, " ~ ", paste0(c(all_W_step2, Z), collapse =  " + ")))
IV_Ertefaie_model1             <- glm(IV_Ertefaie_model1_formula, family = binomial, data = data_ePP2)          # uses all rows of the data 

data_ePP2$X_hat_IV_Ertefaie    <- as.numeric(IV_Ertefaie_model1$fitted.values)
X_hat                          <- "X_hat_IV_Ertefaie"

IV_Ertefaie_model2_formula     <- as.formula(paste(Y, " ~ ", paste0(c(X_hat, all_W_step2), collapse =  " + ")))

if(outcome_variable_type == "binary"){
  
  IV_Ertefaie_model2             <- glm(IV_Ertefaie_model2_formula, offset = log(followup_time[!followup_time == 0]), family = poisson(link = log), data = data_ePP2[!followup_time == 0, ])
  
}else{
  
  IV_Ertefaie_model2             <- lm(IV_Ertefaie_model2_formula, data = data_ePP2)
  
}


## Extraction of the results ----

assign(paste0("IV_Ertefaie_",which_outcome,"_model_summary"), summ(IV_Ertefaie_model2, confint = TRUE, digits = 4))
assign(paste0("IV_Ertefaie_",which_outcome,"_model1"), IV_Ertefaie_model1)

