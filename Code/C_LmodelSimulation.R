# Anej Rozman
# Financna matematika, diplomsko delo
# Code for simaluting sample paths for risk process in C-L model 

library(ggplot2)
library(dplyr)
library(actuar) # For rpareto function
library(viridis)

set.seed(42)

#---------------------------GAMMA DISTRIBUTED CLAIMS---------------------------#

# Parameters
lambda = 0.1 # Intensity of HPP
T = 365 # Time horizon of simulation
alpha = 10000 # Shape
beta = 2 # Rate
u = 10000 # Initial reserve
c = 1000 # Premium income rate

gammaRiskProcess = function(lambda, T, alpha, beta, u, c) {
  
  # Simulate arrival times
  arrivalTimes = c(0)
  t = 0
  while (t <= T) {
    interarrivalTime = rexp(1, rate = lambda)
    t = t + interarrivalTime
    arrivalTimes = c(arrivalTimes, t, t)
  }
  
  # Remove the last arrival time exceeding T and add T
  arrivalTimes = arrivalTimes[arrivalTimes < T]
  arrivalTimes = c(arrivalTimes, T)
  
  # Generate Gamma distributed claims
  gammaClaims = rgamma(length(arrivalTimes)/2-1, alpha, beta)
  
  # Simulate risk process
  riskGamma = data.frame(arrivalTimes)
  riskGamma$premiumRevenue = u + riskGamma$arrivalTimes*c
  riskGamma$cumulativeClaims = 0
  for (i in 2:length(arrivalTimes)) {
    if (i %% 2 == 1) {
      riskGamma[i, 'cumulativeClaims'] = riskGamma[i-1, 'cumulativeClaims'] + gammaClaims[(i-1)/2]
    } else {
      riskGamma[i, 'cumulativeClaims'] = riskGamma[i-1, 'cumulativeClaims'] 
    }
  }
  riskGamma$riskProcess = riskGamma$premiumRevenue - riskGamma$cumulativeClaims
  
  return(riskGamma)
}

#------------------------------------------------------------------------------#




#-------------------------PARETO DISTRIBUTED CLAIMS----------------------------#

# Parameters
lambda = 0.1 # Intensity of HPP
T = 365 # Time horizon of simulation
scale = 5000
shape = 2
u = 10000 # Initial reserve
c = 1000 # Premium income rate

ParetoRiskProcess = function(lambda, T, scale, shape, u, c) {
  
  # Simulate arrival times
  arrivalTimes = c(0)
  t = 0
  while (t <= T) {
    interarrivalTime = rexp(1, rate = lambda)
    t = t + interarrivalTime
    arrivalTimes = c(arrivalTimes, t, t)
  }
  
  # Remove the last arrival time exceeding T and add T
  arrivalTimes = arrivalTimes[arrivalTimes < T]
  arrivalTimes = c(arrivalTimes, T)
  
  # Generate Pareto distributed claims
  paretoClaims = rpareto(length(arrivalTimes)/2-1, scale, shape)
  
  # Simulate risk process
  riskPareto = data.frame(arrivalTimes)
  riskPareto$premiumRevenue = u + riskPareto$arrivalTimes*c
  riskPareto$cumulativeClaims = 0
  for (i in 2:length(arrivalTimes)) {
    if (i %% 2 == 1) {
      riskPareto[i, 'cumulativeClaims'] = riskPareto[i-1, 'cumulativeClaims'] + paretoClaims[(i-1)/2]
    } else {
      riskPareto[i, 'cumulativeClaims'] = riskPareto[i-1, 'cumulativeClaims'] 
    }
  }
  riskPareto$riskProcess = riskPareto$premiumRevenue - riskPareto$cumulativeClaims
  
  return(riskPareto[, c('arrivalTimes', 'riskProcess')])
}

ParetoRiskProcess(lambda, T, scale, shape, u, c)

#------------------------------------------------------------------------------#


#---------------------VISUALIZE RISK PROCESS SIMULATIONS-----------------------#

N = 20 # Number of simulations 

vis = function(N, type, lambda, T, scale, shape, u, c){
  
  # Initialize plot
  p = ggplot()
  
  # Plot gamma or Pareto risk processes
  for (i in 1:N){
    if (type){
      p = p + geom_line(data = gammaRiskProcess(lambda, T, scale, shape, u, c), 
                        aes(x = arrivalTimes, y = riskProcess),
                        size = 0.4,
                        color = plasma(N)[[i]]
      )
    }
    else{
      p = p + geom_line(data = ParetoRiskProcess(lambda, T, scale, shape, u, c), 
                        aes(x = arrivalTimes, y = riskProcess),
                        size = 0.4,
                        color = plasma(N)[[i]]
      )
    }
  }
  
  p = p + labs(title = "simulacija",
               x = "Čas t",
               y = "Proces tveganja U(t)") #+
               #scale_size_identity()
  
  return(p)
}
  

vis(N, FALSE, 0.1, 365, scale, shape, 10000, 500)

#------------------------------------------------------------------------------#



