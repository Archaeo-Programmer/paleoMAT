library(analogue)

getMAT <- function(fossil.data,modern.data,recon.years){  
  fossil.data <- fossil.data[fossil.data$YEAR %in% recon.years,]
  
  fossil.data[,-1:-5][fossil.data[,-1:-5]<0] <- 0
  modern.data[,-1:-4][modern.data[,-1:-4]<0] <- 0
  fossil.data[,-1:-5][is.na(fossil.data[,-1:-5])] <- 0
  modern.data[,-1:-4][is.na(modern.data[,-1:-4])] <- 0
  
  # Convert rows from counts to proportions
  fossil.data.prop <- as.data.frame(t(apply(fossil.data[,-1:-5],1,FUN=function(x){x/sum(x)})))
  modern.data.prop <- as.data.frame(t(apply(modern.data[,-1:-4],1,FUN=function(x){x/sum(x)})))
  
  # Remove incomplete cases from fossil data
  fossil.data <- fossil.data[complete.cases(fossil.data.prop),]
  fossil.data.prop <- fossil.data.prop[complete.cases(fossil.data.prop),]

  ## fit the MAT model using the squared chord distance measure
  sqchord.mat <- mat(modern.data.prop, modern.data$MODERN_CLIMATE, method = "SQchord")
  
  fossil.mat <- predict(sqchord.mat, newdata=fossil.data.prop)
  
  fossil.predictions <- fossil.mat$predictions$model$predicted[fossil.mat$predictions$model$k,]
  
  mat.deviation <- fossil.predictions-fossil.data$MODERN_CLIMATE
  
  # remove outliers > or < 1 SD from the mean
  years <- fossil.data$YEAR
  years <- years[mat.deviation>(mean(mat.deviation)-1*sd(mat.deviation)) & mat.deviation<(mean(mat.deviation)+1*sd(mat.deviation))]
  mat.deviation <- mat.deviation[mat.deviation>(mean(mat.deviation)-1*sd(mat.deviation)) & mat.deviation<(mean(mat.deviation)+1*sd(mat.deviation))]
  
  mat.deviation.interpolation <- approx(x=years, y=mat.deviation, xout=seq(from=head(recon.years,n=1),to=tail(recon.years,n=1),by=1), rule=2)
  
  return(mat.deviation.interpolation) 
}
