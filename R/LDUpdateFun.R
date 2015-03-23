LDUpdateFun <- function(PopMat, rho, nu, psi, epsilon, phi)
{
  # Updates L and D for individuals in I according to Holling Type III
  # 
  # Args
  # 
  # PopMat = matrix of animals currently alive in population
  # rho = 
  # nu = 
  # psi = 
  # epsilon = 
  # phi = 
  #
  # Returns
  # 
  # matrix with same number of columns as PopMat, updated so that L has adjusted
  #  according to D
  
  PopMatNew <- PopMat
  
  lambda <- beta * sum(PopMat$SizeOfL)
  for(i in 1:dim(PopMat)[1])
  {
    if(PopMat$State[i] == "I"){
      PopMatNew$SizeOfL[i] <- (rho * PopMat$SizeOfL[i]) - 
        (nu * PopMat$SizeOfL[i] ^ 2 * PopMat$SizeOfD[i]) /
        (psi ^ 2 + PopMat$SizeOfL[i] ^ 2)
      PopMatNew$SizeOfD[i] <- PopMatNew$SizeOfD[i] * 
        (phi * nu * PopMat$SizeOfL[i] ^ 2) / (psi ^ 2 + PopMat$SizeOfL[i] ^ 2) - 
        epsilon * PopMat$SizeOfD[i]
    } 
  }
  
  return(PopMatNew)
}