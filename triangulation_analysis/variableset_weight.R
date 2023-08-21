#
# Variable set for osmotic symptoms models -------------------------------------
#


X                   <- "treatment"

Y                   <- "outcome_weight"

W_general           <- c("ethnicity_cat", "deprivation", "smoking_cat" ,"dstartdate_age", 
                         "gender_chr", "tx_startyear", "dstartdate_dm_dur_all", 
                         "prehba1c", "preegfr", "preweight", "prealt", "drugline_all", "ncurrtx", "INS")

all_W               <- c(W_general)
all_W_notgender     <- c(all_W[!all_W%in%c("gender_chr")])
all_variables       <- c(X, Y,  W_general)


