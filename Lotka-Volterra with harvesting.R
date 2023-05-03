install.packages("deSolve") #Installs deSolve for the ode functions
require("deSolve") # Loads deSolve

predator.prey.lotka.volterra.harvesting <- function(times, N0, params){ #Defines the model as a function.
  N1 <- N0[1]
  N2 <- N0[2]
  with(as.list(params), {
    dN1.dt <- N1*(r1-alpha12*N2-h*N1) #Runs the first equation for the preys species
    dN2.dt <- N2*(alpha21*N1-d2-h2*N2) #Runs the second equation for the predator species
    return(list(c(dN1.dt, dN2.dt)))
  })
}

params <- c(r1 = 0.5, d2 = 0.1, alpha12 = 0.6, alpha21 = 0.1, h = 0.8, h2 = 0) #Setting the parameters to be used in the equations

N0 <- c(2,1) #Starting conditions

t.values <- seq(0, 150) #Length of time model will predict

predator.prey.out <- ode(N0, t.values, predator.prey.lotka.volterra.harvesting,
                         params) #Ode function to run the simulation under continuous time

par(mfrow=c(1,1))
matplot(predator.prey.out[,c(2,3)], type = 'l', lty = 1, lwd = 2,
        ylab='Abundance', xlab='Time')                            # Creates the plot
legend('topright', c('Prey', 'Predator'), lty = 1, lwd = 2, col =
         c('black', 'red'))





