require(deSolve)

#---------------------------------#
#-- Lorenz model example ---------#
#-- (Initial value problem ODE) --#
#---------------------------------#

# Model parameters
parameters <- c(a = -8/3,
                b = -10, 
                c = 28)
# State variables
state <- c(X = 1,
           Y = 1,
           Z = 1)

# Model equations
Lorenz <- function(t, state, parameters)
{
  with(
    as.list(c(state, parameters)),
       {
         # rate of change
         dX <- a * X + Y * Z
         dY <- b * (Y - Z)
         dZ <- -X * Y + c * Y - Z
         
         # return the rate of change
         list(c(dX, dY, dZ))
       }
    ) # end with()
}

# Time specification
times <- seq(0, 100, by = 0.01)

# Model integration
out <- ode(y = state, times = times, func = Lorenz, parms = parameters)
head(out)

# Plots
par(oma = c(0, 0, 3, 0))
plot(out, xlab = "time", ylab = "-")
plot(out[ , "X"], out[ , "Z"], pch = ".")
mtext(outer = T, side = 3, "Lorenz model", cex = 1.5)
