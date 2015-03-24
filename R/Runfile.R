#-- Individual-based model --#
source("./R/BirthFun.R")
source("./R/MortFun.R")
source("./R/GetInfectedFun.R")
source("./R/LDUpdateFun.R")
source("./R/DiseaseInducedMortFun.R")
source("./R/RemoveMortsFun.R")
source("./R/UpdateDayFun.R")
source("./R/InitializeIBMFun.R")

# PopMat is a dataframe with names
# Individ, Day, State, SizeOfL, SizeOfD, DaysInState

timesteps <- 1000
N.0 <- 1000

BirthRate <- .8
beta <- .09
alpha <- .2
rho <- 1
nu <- 1
psi <- 1
epsilon <- 1
phi <- 1

PopList <- vector("list", timesteps)
PopList[[1]] <- InitializeIBMFun(N.0)
MortsThroughTime <- rep(NA, timesteps)
InfectionsThroughTime <- rep(NA, timesteps)

for(t in 2:timesteps){
  PopList[[t]] <- BirthFun(PopList[t - 1], BirthRate, day = t)
  PopList[[t]] <- GetInfectedFun(PopList[[t]], beta)
  PopList[[t]] <- LDUpdateFun(PopList[[t]], rho, nu, psi, epsilon, phi)
  PopList[[t]] <- DiseaseInducedMortFun(PopList[[t]], alpha)
  MortsThroughTime[t] <- dim(subset(PopList[[t]], State == "D"))[1]
  InfectionsThroughTime[t] <- dim(subset(PopList[[t]], State == "I"))[1]
  PopList[[t]] <- RemoveMortsFun(PopList[[t]])
  PopList[[t]] <- UpdateDayFun(PopList[[t]], day = t)
  print(t)
}

plot(InfectionsThroughTime[-1] ~ seq(2:timesteps), type = "l")
