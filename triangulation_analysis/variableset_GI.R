#
# Variable set for genital infection models ------------------------------------
#


X                <- "treatment"

Y                <- paste("outcome_gi_c"  , censoring_type, sep = "")

followup_time    <- data[ ,paste("followup_days_GI_c"  , censoring_type, sep = "")]

W_general        <- c("ethnicity_cat", "deprivation", "smoking_cat", "dstartdate_age", "gender_chr", 
                      "tx_startyear", "dstartdate_dm_dur_all", "prehba1c", "preegfr", "prebmi", "prealt",
                      "drugline_all", "ncurrtx", "INS")
W_medical        <- c("m3_prior_immunosuppressants", "m3_prior_oestrogens", "m3_prior_oralsteroids")
W_history        <- c("predrug_gi_cat")

all_W            <- c(W_general, W_medical, W_history)
all_W_notgender  <- c(all_W[!all_W%in%c("gender_chr")])
all_variables    <- c(X, Y,  W_general, W_medical, W_history)


