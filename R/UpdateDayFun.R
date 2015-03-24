UpdateDayFun <- function(PopMat, day){
  # produces births each day (births not pulsed)
  # 
  # Args
  # 
  # PopMat = matrix of animals currently alive in population
  # BirthRate = prop of total population who reproduce each year
  #
  # Returns
  # 
  # matrix with same number of columns as PopMat, but with Day updated to be i
  
  PopMat$Day <- rep(day, dim(PopMat)[1])
  return(PopMat)
}