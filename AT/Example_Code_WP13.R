#####################################
# Example Code WP13 AT

# ---------------------------------------------------------------------------------------------------------------------
# load/install libraries

# install R library
install.packages(c("data.table", "simPop", "synthPop", "devtools", "reticulate", "keras"))
devtools::install_github("qwertzlbry/RAPID") # risk/utility measures
keras::install_keras() # installs python/keras and other stuff needed

# install python library
system("~/.virtualenvs/r-keras/bin/pip install -U 'mostlyai[local]'")

# set virt environment ~ potentially neeed to start a new session 
# bevore specifying this
Sys.setenv(RETICULATE_PYTHON = "~/.virtualenvs/r-keras/bin/python")

# load R library
library(data.table)
library(simPop)
library(synthpop)
library(devtools)
library(RAPID)

# ---------------------------------------------------------------------------------------------------------------------
# load data
data(eusilcP) # population data
setDT(eusilcP)

cnames <- colnames(eusilcP)
income_h <- cnames[cnames %like% "^hy"]

# sum of personal income sources
eusilcP[, IncomeP := rowSums(.SD), .SDcols = patterns("^py")]
# sum of household income sources
eusilcP[, IncomeH := rowSums(.SD[1,]), by = .(hid), .SDcols = patterns("^hy")]

# ---------------------------------------------------------------------------------------------------------------------
# Apply synthetic data generations tools

# -----------------------
# simPop
inp <- specifyInput(data = eusilcP, hhid = "hid", hhsize = "hsize", strata = "region", population = TRUE)

# define basic household variables
simPop_data <- simStructure(data = inp, method = "direct", basicHHvars = c("age", "gender"))

# model additional variables
simPop_data <- simCategorical(simPop_data, additional = c("citizenship"), method = "ranger",
                         nr_cpus = 1)

simPop_data <- simCategorical(simPop_data, additional = c("ecoStat"), method = "ranger",
                         nr_cpus = 1)

simPop_data <- simContinuous(simPop_data, additional = c("IncomeP"), method = "multinom",
                             nr_cpus = 1)
simPop_data <- simContinuous(simPop_data, additional = c("IncomeH"), method = "multinom",
                             nr_cpus = 1)

simPop_data@pop@data
simPop_data <- copy(simPop_data@pop@data)
# -----------------------
# synthpop
synth_vars <- c(
  "age", "gender",
  "region",
  "citizenship",
  "ecoStat", "IncomeP",
  "IncomeH"
)
synthpop_data <- synthpop::syn(
  subset(eusilcP, select = synth_vars),
  visit.sequence = synth_vars
)

synthpop_data$syn
synthpop_data <- synthpop_data$syn
setDT(synthpop_data)

# -----------------------
# tabularARGN ~ mostlyAI

# split data in Personfile and household file
eusilcP_house <- eusilcP[!duplicated(hid),.(hid, hsize, region)]
eusilcP_pers <- eusilcP[,.(hid, age, gender, citizenship, IncomeP, IncomeH)]

fwrite(eusilcP_house, file = "eusilcP_house.csv")
fwrite(eusilcP_pers, file = "eusilcP_pers.csv")

reticulate::source_python("TabularARGN.py")

tabularARGN_house <- fread("eusilcP_house_TabularARGAN.csv")
tabularARGN_house[, V1 := NULL]
tabularARGN_pers <- fread("eusilcP_pers_TabularARGAN.csv")
tabularARGN_pers[, V1 := NULL]
tabularARGAN_data <- merge(tabularARGN_house, tabularARGN_pers, by = "hid")
# ---------------------------------------------------------------------------------------------------------------------
# Estimate risk & utility measures

# --------------------------------
# estimate number of replicated uniques
repl_uniques <- function(data_synth, data_orig, key_vars){
  
  uniques_orig <- data_orig[,.(N_orig = .N),by=c(key_vars)]
  uniques_synth <- data_synth[,.(N_synth = .N),by=c(key_vars)]
  
  uniques_dt <- merge(uniques_orig, uniques_synth, by=c(key_vars), all = TRUE)
  uniques_dt[is.na(N_orig), N_orig := 0]
  uniques_dt[is.na(N_synth), N_synth := 0]
  uniques_dt[,rep_uniques := N_orig==1 & N_synth==1]
  
  return(uniques_dt)
}

key_vars <- c("age", "gender", "region", "IncomeP")
rep_u_simPop <- repl_uniques(data_synth = simPop_data, data_orig = eusilcP, key_vars = key_vars)
rep_u_simPop[,sum(N_synth[rep_uniques])/sum(N_synth)]

rep_u_synthpop <- repl_uniques(data_synth = synthpop_data, data_orig = eusilcP, key_vars = key_vars)
rep_u_synthpop[,sum(N_synth[rep_uniques])/sum(N_synth)]

rep_u_tabularARGAN <- repl_uniques(data_synth = tabularARGAN_data, data_orig = eusilcP, key_vars = key_vars)
rep_u_tabularARGAN[,sum(N_synth[rep_uniques])/sum(N_synth)]

res <- data.table(Method = c("simPop", "synthPop", "tabularARGAN"), repl.U = c(
  rep_u_simPop[,sum(N_synth[rep_uniques])/sum(N_synth)],
  rep_u_synthpop[,sum(N_synth[rep_uniques])/sum(N_synth)],
  rep_u_tabularARGAN[,sum(N_synth[rep_uniques])/sum(N_synth)]
))

print(res)

# --------------------------------
# Apply RAPID
# predict variable income

eusilcP[, Intercept := 1]
simPop_data[, Intercept := 1]
synthpop_data[, Intercept := 1]
tabularARGAN_data[, Intercept := 1]

# 2 different models: Intercept model and other model
vars_model <- list(c("Intercept"),c("gender", "age", "region"))

# sensitive variable to predict
v <- c("IncomeP")

# filter synthetic population and original population
# when modelling income
filter_age <- 14

# different parameters, see ?rapid
num_eps <- c(0.05, 500)
error_metric <- c("symmetric","absolute")

output_results <- list()

# iterate over settings
for(m in vars_model){
  
  for(i in 1:length(num_eps)){
    
    
    result_simPop <- rapid(
      original_data = eusilcP[age > filter_age],
      synthetic_data = simPop_data[age > filter_age],
      quasi_identifiers = m,
      sensitive_attribute = v,
      model_type = "rf",
      num_epsilon = num_eps[i],
      num_error_metric = error_metric[i],
      return_all_records = TRUE)
    
    dt_simPop <- as.data.table(result_simPop$risk$rows_risk_df)
    dt_simPop[, ID:= .I]
    dt_simPop <- dt_simPop[,.(ID, at_risk)]
    dt_simPop[,Model := paste(m, collapse = " + ")]
    dt_simPop[,Sens_var := v]
    dt_simPop[,Error_metric := error_metric[i]]
    dt_simPop[,Method := "simPop"]
    
    result_synthpop <- rapid(
      original_data = eusilcP[age > filter_age],
      synthetic_data = synthpop_data[age > filter_age],
      quasi_identifiers = m,
      sensitive_attribute = v,
      model_type = "rf",
      num_epsilon = num_eps[i],
      num_error_metric = error_metric[i],
      return_all_records = TRUE)
    
    dt_synthpop <- as.data.table(result_synthpop$risk$rows_risk_df)
    dt_synthpop[, ID:= .I]
    dt_synthpop <- dt_synthpop[,.(ID, at_risk)]
    dt_synthpop[,Model := paste(m, collapse = " + ")]
    dt_synthpop[,Sens_var := v]
    dt_synthpop[,Error_metric := error_metric[i]]
    dt_synthpop[,Method := "synthpop"]
    
    
    result_tabularARGAN <- rapid(
      original_data = eusilcP[age > filter_age],
      synthetic_data = tabularARGAN_data[age > filter_age],
      quasi_identifiers = m,
      sensitive_attribute = v,
      model_type = "rf",
      num_epsilon = num_eps[i],
      num_error_metric = error_metric[i],
      return_all_records = TRUE)
    
    dt_tabularARGAN <- as.data.table(result_tabularARGAN$risk$rows_risk_df)
    dt_tabularARGAN[, ID:= .I]
    dt_tabularARGAN <- dt_tabularARGAN[,.(ID, at_risk)]
    dt_tabularARGAN[,Model := paste(m, collapse = " + ")]
    dt_tabularARGAN[,Sens_var := v]
    dt_tabularARGAN[,Error_metric := error_metric[i]]
    dt_tabularARGAN[,Method := "tabularARGAN"]
    
    output_results <- c(output_results, list(dt_simPop, dt_synthpop, dt_tabularARGAN))
    
  }
}
output_results <- rbindlist(output_results)

out_rapid <- output_results[, .(mean(at_risk)), by = .(Method, Model, Error_metric)]
print(out_rapid)
