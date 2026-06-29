##################################################################
##                       Run launcher job                       ##
##################################################################

cpu <- "0-9"
job <- file.path("~/WP13/DE/R/04_risk.R")

cmd <- paste("taskset --cpu-list", cpu, "Rscript", job)
system(cmd)