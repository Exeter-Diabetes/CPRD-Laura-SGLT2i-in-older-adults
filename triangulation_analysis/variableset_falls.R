#
# Variable set for fall models -------------------------------------------------
#


X                <- "treatment"

Y                <- paste("outcome_falls_c"  , censoring_type, sep = "") 

followup_time    <- data[ ,paste("followup_days_falls_c"  , censoring_type, sep = "")]

W_general        <- c("ethnicity_cat", "deprivation", "smoking_cat", "dstartdate_age", "gender_chr", 
                      "tx_startyear", "dstartdate_dm_dur_all", "prehba1c", "preegfr", "prebmi", "prealt",
                      "drugline_all", "ncurrtx", "INS")
W_medical        <- c("m3_prior_oestrogens", "m3_prior_oralsteroids", "m3_prior_ksparing_diuretics",
                      "m3_prior_loop_diuretics", "m3_prior_thiazide_diuretics", "m3_prior_statins", 
                      "m3_prior_ACEinhibitors")
W_history        <- c("predrug_lowerlimbfracture_cat", "predrug_falls_cat", "predrug_amputation_cat", 
                      "predrug_dka_cat", "predrug_dementia_cat", "predrug_cancer_cat", "predrug_asthma_cat", 
                      "predrug_copd_cat", "predrug_heartfailure_cat", "predrug_cvd_cat", "predrug_cld_cat", 
                      "predrug_osteoporosis_cat")

all_W            <- c(W_general, W_medical, W_history)
all_W_notgender  <- c(all_W[!all_W%in%c("gender_chr")])
all_variables    <- c(X, Y,  W_general, W_medical, W_history)




