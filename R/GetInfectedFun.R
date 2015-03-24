GetInfectedFun <- function(PopMat, beta)
{
  # produces (natural) mortalities each day (mortalities not pulsed)
  # 
  # Args
  # 
  # PopMat = matrix of animals currently alive in population
  # beta = force of infection constant
  #
  # Returns
  # 
  # matrix with same number of columns as PopMat, updated so that new Is moved to I
  
  PopMat$SizeOfL <- as.numeric(PopMat$SizeOfL)
  PopMatNew <- PopMat
  
  lambda <- beta * sum(as.numeric(PopMat$SizeOfL))
  for(i in 1:dim(PopMat)[1])
    {
    if(PopMat$State[i] == "S"){
      PopMatNew$State[i] <- ifelse(rbinom(1, 1, min(1, lambda)) == 1, "I", "S")
      PopMatNew$DaysInState[i] <- 0
      PopMatNew$SizeOfL[i] <- 1
    } else{
      PopMatNew$DaysInState[i] <- as.numeric(PopMat$DaysInState[i]) + 1
    } 
  }
  return(PopMatNew)
}