RemoveMortsFun <- function(PopMat)
{
  PopMatNew <- subset(PopMat, State != "D")
  return(PopMatNew)
  
}