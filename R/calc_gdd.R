calc_gdd <- function(tmin, tmax, t.base, t.cap=NULL){
  if(length(tmin)!=length(tmax)){
    stop("tmin and tmax  must have same length!")
  }
    
    # Floor tmax and tmin at Tbase
    tmin[tmin < t.base] <- t.base
    tmax[tmax < t.base] <- t.base
    
    # Cap tmax and tmin at Tut
    if(!is.null(t.cap)){
      tmin[tmin > t.cap] <- t.cap
      tmax[tmax > t.cap] <- t.cap
    }
    
    GDD <- ((tmin+tmax)/2)-t.base
  
  return(GDD)
}