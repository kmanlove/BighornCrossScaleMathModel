DiseaseInducedMortFun <- function(PopMat, alpha)
{
  # produces disease-induced mortalities each day (mortalities not pulsed)
  # 
  # Args
  # 
  # PopMat = matrix of animals currently alive in population
  # alpha = proportionality constant governing P(death | Load)
  #
  # Returns
  # 
  # matrix with same number of columns as PopMat, updated so that new Is moved to I
  
  PopMatNew <- PopMat
  Survive <- rep(NA, dim(PopMat)[1])
  
  for(i in 1:dim(PopMat)[1])
  {
    if(PopMat$State[i] == "I"){
      Die[i] <- min(1, rbinom(1, 1, alpha * 1 / PopMat$SizeOfL[i]))
      PopMatNew$State[i] <- ifelse(Die[i] == 1, "D", "I")
    } 
  }
  
  return(PopMatNew)
}