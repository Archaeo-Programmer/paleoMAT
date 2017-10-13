library(dplyr)

run_tf <- function(pollen, climate, fossil = NULL, func, ...) {
  
  # This function allows us to clean up the randomRF method, providing the option to
  # serialize the process of analyzing multi-model outputs.  It makes sure that
  # datasets use consistent columns, accept extra inputs, and returns a consistent
  # table for output.
  
  
  # First, convert all pollen values to proportions, using only taxa which have some 
  #  presence in the dataset.  This is the result of an error that is returned some times
  #  from methods with all zeros for a taxon.  This may be problematic for datasets where
  #  the calibration dataset has absences but the focal dataset has presence for a
  #  particular taxon.
  
  pollen_pct_p <- pollen %>% 
    dplyr::select(which(colSums(pollen) > 0)) %>% 
    analogue::tran(method = "proportion")
  
  # If there is no
  
  if (!is.null(fossil)) {
    
    foss_pct_p <- fossil %>% 
      dplyr::select(which(colSums(fossil) > 0)) %>% 
      analogue::tran(method = "proportion")
    
    keep_cols <- colnames(pollen[colSums(pollen_pct_p) > 0])
    keep_cols <- keep_cols[keep_cols %in% colnames(foss_pct_p)]
    
    pollen_input <- pollen_pct_p[, keep_cols]
    fossil_input <- foss_pct_p[, keep_cols]
    
  } else {
    pollen_input <- pollen_pct_p
    fossil_input <- pollen_input
  }
  
  model_test <- suppressWarnings(randomTF(spp = pollen_input, 
                                          env = climate,
                                          fos = fossil_input,
                                          n = 99,
                                          fun = func,
                                          ...))
  
  sig_table <- data.frame("variance" = model_test$EX,
                          "p-value" = model_test$sig)
  
  sig_table <- round(sig_table, 2)
  colnames(sig_table)[1] <- "% Explained"
  colnames(sig_table)[2] <- "p-value"
  
  return(list(table = knitr::kable(sig_table), values = sig_table))
}