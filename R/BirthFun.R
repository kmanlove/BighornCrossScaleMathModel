BirthFun <- function(PopMat, BirthRate)
{
  # produces births each day (births not pulsed)
  # 
  # Args
  # 
  # PopMat = matrix of animals currently alive in population
  # BirthRate = prop of total population who reproduce each year
  #
  # Returns
  # 
  # matrix with same number of columns as PopMat, but with one new row for each
  #   new birth
  PopMat <- as.data.frame(PopMat)
  
  PopSize <- dim(PopMat)[1]
  NewBirths <- rbinom(1, PopSize, BirthRate / 365)
  if(NewBirths >= 1){
  BirthMat <- matrix(NA, nrow = NewBirths, ncol = dim(PopMat)[2])
  BirthMat[1, ] <- seq(max(PopMat[1, ]) + 1, NewBirths)
  BirthMat[2, ] <- rep("S", NewBirths) # current state
  BirthMat[3, ] <- rep(0, NewBirths) # size of L
  BirthMat[4, ] <- rep(0, NewBirths) # size of D
  BirthMat[5, ] <- rep(0, NewBirths) # days in state
  BirthMat[6, ] <- rep(0, NewBirths) # age in days
  BirthFrame <- as.data.frame(BirthMat)
  names(BirthFrame) <- names(PopMat)
  
  PopMatNew <- ifelse(NewBirths == 0, PopMat, 
                      rbind(PopMat, BirthFrame))
  } else{
    PopMatNew <- PopMat
  }
  return(PopMatNew)
}