InitializeIBMFun <- function(N.0, timesteps)
{
  # Initializes population states and PopMat dataframe
  # 
  # Args
  # 
  # PopMat = matrix of animals currently alive in population
  # alpha = proportionality constant governing P(death | Load)
  #
  # Returns
  # 
  # Initial PopMat dataframe
  
  PopMat <- as.data.frame(matrix(NA, nrow = N.0, ncol = 6))
  names(PopMat) <- c("Individ", "Day", "State", "SizeOfL", "SizeOfD", "DaysInState")
  PopMat$Individ <- seq(1 : N.0)
  PopMat$Day <- rep(1, dim(PopMat)[1])
  PopMat$State[1] <- "I"
  PopMat$State[2 : N.0] <- "S"
  PopMat$SizeOfL[1] <- 1
  PopMat$SizeOfL[2 : N.0] <- 0
  PopMat$SizeOfD <- rep(0, dim(PopMat)[1])
  PopMat$DaysInState <- rep(0, dim(PopMat)[1])
  
  return(PopMat)
}