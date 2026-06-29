#################################################################
##                       Tabular utility                       ##
##       Calculates normalized Hellinger distance metric       ##
#################################################################

# Function to calculate our Hellinger metric ------------------------------

#--- Arguments
# dt1, dt2: two data.tables (original & synthetic count tables)
# vars: all variables needed for tabulation
# count_col: name of count column

hellinger_dt <- function(dt1, dt2, vars, count_col = "N") {
  
  # Ensure data.table
  d1 <- data.table::copy(data.table::as.data.table(dt1))
  d2 <- data.table::copy(data.table::as.data.table(dt2))
  
  # Rename count columns
  data.table::setnames(d1, "N", "N_orig")
  data.table::setnames(d2, "N", "N_syn")
  
  # Merge both count tables
  merged <- merge(d1, d2, by = vars, all = TRUE)
  
  # replace missing counts with 0
  merged[is.na(N_orig), N_orig := 0]
  merged[is.na(N_syn), N_syn := 0]
  
  # Normalizing cell counts so that the sum of all counts is equal to 1
  sum_orig <- sum(merged$N_orig)
  sum_syn <- sum(merged$N_syn)
  
  p_i <- merged$N_orig/sum_orig
  q_i <- merged$N_syn/sum_syn
  
  # Calculate Hellinger distance
  term_h <- sum((sqrt(p_i) - sqrt(q_i))^2)
  out <- sqrt(0.5 * term_h)
  out
}
