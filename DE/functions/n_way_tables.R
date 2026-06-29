#################################################################
##                        n_way_tables                         ##
##         Build all possible n-way table combinations         ##
##       where at least one target variable is involved.       ##
#################################################################

#--- Arguments:
# keys, targets: character vectors without overlap assumed
# sizes: integer vector of desired sizes (>=1)

n_way_tables <- function(keys, targets, sizes) {
  
  # safety
  vars_count <- length(keys) + length(targets)
  sizes <- sizes[sizes >= 1 & sizes <= vars_count]
  if (length(sizes) == 0) return(list())
  
  keys <- as.character(keys)
  targets <- as.character(targets)
  
  # output list
  out <- vector("list", 0)
  out_i <- 0L
  
  for (k in sizes) {
    
    # condition for number of targets t: must be at least 1 and <= min(k, length(targets))
    t_min <- max(1L, k - length(keys))     # at least to fill size
    t_max <- min(k, length(targets))
    
    # search for key-target-combinations that fulfills our condition
    if (t_min > t_max) next 
    
    for (t in seq.int(t_min, t_max)) {
      
      # number of key variables
      k_keys <- k - t
      
      # choose all t-target combos
      target_combs <- if (t == 0) list(character(0)) else combn(targets, t, simplify = FALSE)
      
      # choose all k_keys key combos
      key_combs <- if (k_keys == 0) list(character(0)) else combn(keys, k_keys, simplify = FALSE)
      
      # preallocate space for the cross product
      m <- length(target_combs) * length(key_combs)
      if (m == 0) next
      start <- out_i + 1L
      out[(start):(out_i + m)] <- vector("list", m)
      pos <- 0L
      
      for (ti in seq_along(target_combs)) {
        tvec <- target_combs[[ti]]
        for (ki in seq_along(key_combs)) {
          pos <- pos + 1L
          kvals <- key_combs[[ki]]
          # combine keys and targets; optionally sort to ensure consistent ordering
          out[[out_i + pos]] <- c(kvals, tvec)
        }
      }
      out_i <- out_i + m
    }
  }
  
  # trim if preallocated with zero-growth pattern
  if (out_i < length(out)) out <- out[seq_len(out_i)]
  out
}
