MortFun <- function(PopMat, MortRate)
{
  # produces (natural) mortalities each day (mortalities not pulsed)
  # 
  # Args
  # 
  # PopMat = matrix of animals currently alive in population
  # MortRate = prop of total population who die each year
  #
  # Returns
  # 
  # matrix with same number of columns as PopMat, but with mortalities removed
  
  PopSize <- dim(PopMat)[1]
  NewMorts <- rbinom(PopSize, 1, MortRate / 365)
  
  PopMatNew <- PopMat[-which(NewMorts == 1), ]
  return(PopMatNew)
}