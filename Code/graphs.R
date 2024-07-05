# Anej Rozman
# Financna matematika, diplomsko delo
# Code for generating graphs\pictures

library(ggplot2)
library(dplyr)
library(actuar)
library(gridExtra)
library(glue) # Implementation of f-string from python

set.seed(42)

#-------------------------------------------------------------------------------#

# Slika 1 (Poisson process and compound Poisson process sample path)

# Parameters
nSteps = 150
lambda = 0.1  # Intensity

# Generate sample path of HPP and CPP
jumpsHpp = rpois(nSteps, lambda)
samplePath = data.frame(time = 1:nSteps,
                        jumpTimes = jumpsHpp, 
                        gamma = rgamma(nSteps, shape = 20, rate = 1))

samplePath$value = cumsum(samplePath$gamma*samplePath$jumpTimes)

# Create an origin
new = data.frame(
  time = c(0, 1),
  jumpTimes = c(0, 0),
  gamma = c(0, 0),
  value = c(0, 0))

samplePath = rbind(new, samplePath[-1, , drop = FALSE])

# Create ggplot
g1 = ggplot() +
  geom_step(data = samplePath,
            aes(x = time, y = value),
            size = 1,
            color = "red") +
  geom_point(aes(x = samplePath$jumpTimes * 0:nSteps, y = -0.8,
                 size = ifelse(samplePath$jumpTimes * 0:nSteps == 0 & -0.8 == -0.8, 0, 3)),
             shape = "x",
             color = "black") +
  geom_segment(data = data.frame(x = samplePath$jumpTimes * 0:nSteps,
                                 yend = samplePath$value) %>% 
                 dplyr::filter(x != 0),
               aes(x = x, xend = x, y = 0, yend = yend), 
               linetype = "dashed",
               color = "black") +
  coord_cartesian(clip = 'off') +
  labs(title = lambda == 0.1~""~alpha == 20~" število korakov: 150",
       x = "Čas",
       y = "Vrednost") +
  scale_size_identity()

g1

# Save graph to pdf
ggsave("C:/Users/38651/OneDrive - Univerza v Ljubljani/Desktop/Diploma/Diplomski-seminar/GraphsAndPhotos/slika1.pdf",
       g1,
       device = "pdf",
       width = 8,
       height = 5)

#-------------------------------------------------------------------------------#

# Slika 2 (Simulation of two sample paths of risk process with Weibull claims,
# one falls below 0 (heavy tailed) and one survives (light tailed)."

set.seed(11)

# Parameters
lambda = 1 # Intensity of HPP
T = 50 # Time horizon of simulation
u = 1000 # Initial reserve
c = 200 # Premium income rate

################################## LIGHT TAILS #################################

# Light tailed Weibull
aLight = 2 # Shape
bLight = 434 # Rate

# Simulate arrival times for light tails
arrivalTimesLight = c(0)
t = 0
while (t <= T) {
  interarrivalTime = rexp(1, rate = lambda)
  t = t + interarrivalTime
  arrivalTimesLight = c(arrivalTimesLight, t, t)
}

# Remove the last arrival time exceeding T and add T
arrivalTimesLight = arrivalTimesLight[arrivalTimesLight < T]
#arrivalTimesLight = c(arrivalTimesLight, T)

# Generate claims
lightClaims = rweibull(length(arrivalTimesLight)/2-1, aLight, bLight)

# Simulate risk process
riskWeibullLight = data.frame(arrivalTimesLight)
riskWeibullLight$premiumRevenue = u + riskWeibullLight$arrivalTimesLight*c
riskWeibullLight$cumulativeClaims = 0
for (i in 2:length(arrivalTimesLight)) {
  if (i %% 2 == 1) {
    riskWeibullLight[i, 'cumulativeClaims'] = riskWeibullLight[i-1, 'cumulativeClaims'] + lightClaims[(i-1)/2]
  } else {
    riskWeibullLight[i, 'cumulativeClaims'] = riskWeibullLight[i-1, 'cumulativeClaims'] 
  }
}
riskWeibullLight$riskProcess = riskWeibullLight$premiumRevenue - riskWeibullLight$cumulativeClaims

for (i in 1:nrow(riskWeibullLight)){
  if (riskWeibullLight$riskProcess[i] < 0){
    riskWeibullLight[i,4] = 0
    riskWeibullLight = riskWeibullLight[1:i,]
    break
  }
}

############################### HEAVY TAILS ####################################

# Heavy tailed Weibull
aHeavy = 0.25 # Shape
bHeavy = 16 # Rate

# Simulate arrival times for heavy tails
arrivalTimesHeavy = c(0)
t = 0
while (t <= T) {
  interarrivalTime = rexp(1, rate = lambda)
  t = t + interarrivalTime
  arrivalTimesHeavy = c(arrivalTimesHeavy, t, t)
}

# Remove the last arrival time exceeding T and add T
arrivalTimesHeavy = arrivalTimesHeavy[arrivalTimesHeavy < T]
#arrivalTimesHeavy = c(arrivalTimesHeavy, T)

# Generate claims
heavyClaims = rweibull(length(arrivalTimesHeavy)/2-1, aHeavy, bHeavy)

# Simulate risk process
riskWeibullHeavy = data.frame(arrivalTimesHeavy)
riskWeibullHeavy$premiumRevenue = u + riskWeibullHeavy$arrivalTimesHeavy*c
riskWeibullHeavy$cumulativeClaims = 0
for (i in 2:length(arrivalTimesHeavy)) {
  if (i %% 2 == 1) {
    riskWeibullHeavy[i, 'cumulativeClaims'] = riskWeibullHeavy[i-1, 'cumulativeClaims'] + heavyClaims[(i-1)/2]
  } else {
    riskWeibullHeavy[i, 'cumulativeClaims'] = riskWeibullHeavy[i-1, 'cumulativeClaims'] 
  }
}
riskWeibullHeavy$riskProcess = riskWeibullHeavy$premiumRevenue - riskWeibullHeavy$cumulativeClaims

for (i in 1:nrow(riskWeibullHeavy)){
  if (riskWeibullHeavy$riskProcess[i] < 0){
    riskWeibullHeavy[i,4] = 0
    riskWeibullHeavy = riskWeibullHeavy[1:i,]
    break
  }
}


################################## VISUALIZE ###################################

# Simulation of risk processes
g2riskProcess = ggplot()
g2riskProcess = g2riskProcess + geom_line(data = riskWeibullLight, 
                    aes(x = arrivalTimesLight, y = riskProcess),
                    size = 0.7,
                    color = 'red') + 
          geom_line(data = riskWeibullHeavy, 
                    aes(x = arrivalTimesHeavy, y = riskProcess), 
                    size = 0.7, 
                    color = 'cyan3') + 
          geom_point(aes(x =riskWeibullLight[nrow(riskWeibullLight), 1], y = -0.8),
                     size = 2.5,
                     shape = "x",
                     color = "black") +
          geom_point(aes(x =riskWeibullHeavy[nrow(riskWeibullHeavy), 1], y = -0.8),
                       size = 2.5,
                       shape = "x",
                       color = "black") +
          labs(title = "Realizacija procesa tveganja z Weibullovo porazdeljenimi zahtevki",
               x = "t",
               y = "U(t)")
g2riskProcess
          
# Densities

# Light
lightDensity = data.frame(x=seq(0, 1000, by = 0.005), 
                          y=dweibull(seq(0, 1000, by = 0.005), aLight, bLight))

g2lightDensity = ggplot() + geom_line(data = lightDensity, 
                                      aes(x, y), 
                                      size = 0.7, 
                                      color = 'red') +
                            labs(title = 'Gostota X~Weibull(2, 434)',
                                 y = 'f_X', x = '') 

g2lightDensity

# Heavy
heavyDensity = data.frame(x=seq(0, 1000, by = 0.005), 
                          y=dweibull(seq(0, 1000, by = 0.005), aHeavy, bHeavy))

g2heavyDensity = ggplot() + geom_line(data = heavyDensity, 
                                      aes(x, y), 
                                      size = 0.7, 
                                      color = 'cyan3') +
                            labs(title = 'Gostota Y~Weibull(1/4, 16)',
                                 y = 'f_Y', x = '') + 
                            scale_y_continuous(limits = c(0, 0.02))

g2heavyDensity
  
# Combine plots
g2 = grid.arrange(g2lightDensity, g2heavyDensity, ncol = 2)
g2 = grid.arrange(g2, g2riskProcess, heights = c(3.5, 6.5))

g2

# Save graph to pdf
ggsave("C:/Users/38651/OneDrive - Univerza v Ljubljani/Desktop/Diploma/Diplomski-seminar/GraphsAndPhotos/slika2.pdf",
       g2,
       device = "pdf",
       width = 8,
       height = 6)

#-------------------------------------------------------------------------------#
#Slika 3 (Monte Carlo simulation for approximating ruin proability)

# Comment: 
# 1.) If you create more than 4 simulations you have to manually chose more colors
# when plotting approx. line 340)
# 2.) Currently takes approx. 10 minutes to execute simulations
library(ggplot2)
library(dplyr)
library(actuar)
library(gridExtra)
library(glue)
library(viridis)

set.seed(42)

# Parameters
lambda = 1 # Intensity of HPP
mu = 1/1000 # Claim size parameter
T = 1000 # Time horizon of simulation
u = seq(500, 25000, 500) # Initial reserve
c = 1500 # Premium income rate
N = c(10, 50, 100) # Number of simulations 

########################## RISK PROCESS SIMULATION #############################

estimateProbabilityOfRuin = data.frame(u, matrix(0, ncol = length(N), nrow = length(u)))
colnames(estimateProbabilityOfRuin) = c("u", as.character(N))



# Simulate arrival times
for (sim in N){
  cname = as.character(sim)
  print(glue("Number of simulations in batch:{cname}"))
  for (j in 1:length(u)) {
    print(glue("simulation:{j}/{length(u)}"))
    for (k in 1:sim){
      arrivalTimes = c(0)
      t = 0
      while (t <= T) {
        interarrivalTime = rexp(1, rate = lambda)
        t = t + interarrivalTime
        arrivalTimes = c(arrivalTimes, t, t)
      }
      
      # Remove the last arrival time exceeding T and add T
      arrivalTimes = arrivalTimes[arrivalTimes < T]
      #arrivalTimes = c(arrivalTimes, T)
      
      # Generate claims
      claims = rexp(length(arrivalTimes)/2-1, mu)
      # Simulate risk process
      riskProcessExp = data.frame(arrivalTimes)
      riskProcessExp$premiumRevenue = u[j] + riskProcessExp$arrivalTimes*c
      riskProcessExp$cumulativeClaims = 0
      for (i in 2:length(arrivalTimes)) {
        if (i %% 2 == 1) {
          riskProcessExp[i, 'cumulativeClaims'] = riskProcessExp[i-1, 'cumulativeClaims'] + claims[(i-1)/2]
        } else {
          riskProcessExp[i, 'cumulativeClaims'] = riskProcessExp[i-1, 'cumulativeClaims'] 
        }
      }
      riskProcessExp$riskProcess = riskProcessExp$premiumRevenue - riskProcessExp$cumulativeClaims
      # Check for ruin
      for (i in 1:nrow(riskProcessExp)){
        if (riskProcessExp$riskProcess[i] < 0){
          estimateProbabilityOfRuin[cname][j, 1] = estimateProbabilityOfRuin[cname][j, 1] + 1
          break
        }
      }

    }
  }
  # Normalize frequencies
  estimateProbabilityOfRuin[cname] = estimateProbabilityOfRuin[cname] / sim
}

# Exact probability of ruin
exactProbabilityOfRuin = function(lambda, mu, u, c){
  df = data.frame(u)
  rho = (c*mu)/lambda - 1
  df$exactProbability = (1/(1+rho))*exp(-u*mu*(rho/(1 + rho)))
  return(df)
}

exactProbabilityOfRuin = exactProbabilityOfRuin(lambda, mu, u, c)

# Merge estimates with exact values
pOfRuin = merge(estimateProbabilityOfRuin, exactProbabilityOfRuin, by='u')

############################## VISUALIZE #######################################

g3 = ggplot() +
              geom_line(data = pOfRuin,
                        aes(x = u, y = exactProbability),
                        color = "black") +
              labs(title = glue(" \u03BB={lambda}, \u03BC={mu}, c={c}"),
                   x = "Začetni kapital (u)",
                   y = glue("Verjetnost propada (\u03C8(u))"))

colors = c("cyan", "red", "blueviolet", "darkseagreen")
for (i in seq_along(N)){
  g3 = g3 + geom_line(data = pOfRuin,
                      aes(x = u, y = !!sym(as.character(N)[i])),
                      color = colors[i])
}

g3

# Save graph to pdf
ggsave("C:/Users/38651/OneDrive - Univerza v Ljubljani/Desktop/Diploma/Diplomski-seminar/GraphsAndPhotos/slika3.pdf",
       g3,
       device = "pdf",
       width = 8,
       height = 5)


#-------------------------------------------------------------------------------#
#Slika 4 (Monte Carlo simulation for approximating asymptotics of ruin probability)

set.seed(42)
library(ggplot2)
library(dplyr)
library(actuar)
library(gridExtra)
library(glue)
library(viridis)

# Parameters
lambda = 1 # Intensity of HPP
a = 1/1000 # Claim size parameters
T = 1000 # Time horizon of simulation
u = seq(5000, 50000, 5000) # Initial reserve
c = 1500 # Premium income rate
N = 1000 # Number of simulations 

########################## RISK PROCESS SIMULATION #############################
estProbRuin = data.frame(u)
estProbRuin$estimation = 0

for (j in 1:length(u)) {
  print(glue("simulation:{j}/{length(u)}"))
  for (k in 1:N){
    arrivalTimes = c(0)
    t = 0
    while (t <= T) {
      interarrivalTime = rexp(1, rate = lambda)
      t = t + interarrivalTime
      arrivalTimes = c(arrivalTimes, t, t)
    }
    
    # Remove the last arrival time exceeding T and add T
    arrivalTimes = arrivalTimes[arrivalTimes < T]
    #arrivalTimes = c(arrivalTimes, T)
    
    # Generate claims
    claims = rexp(length(arrivalTimes)/2-1, mu)
    # Simulate risk process
    riskProcessW = data.frame(arrivalTimes)
    riskProcessExp$premiumRevenue = u[j] + riskProcessExp$arrivalTimes*c
    riskProcessExp$cumulativeClaims = 0
    for (i in 2:length(arrivalTimes)) {
      if (i %% 2 == 1) {
        riskProcessExp[i, 'cumulativeClaims'] = riskProcessExp[i-1, 'cumulativeClaims'] + claims[(i-1)/2]
      } else {
        riskProcessExp[i, 'cumulativeClaims'] = riskProcessExp[i-1, 'cumulativeClaims'] 
      }
    }
    riskProcessExp$riskProcess = riskProcessExp$premiumRevenue - riskProcessExp$cumulativeClaims
    # Check for ruin
    
  }
}

# Normalize frequencies
estProbRuin$estimation = estProbRuin$estimation / N

# Coefficient exp(\ell*u)
cramerCoefficient = function(lambda, mu, u, c){
  df = data.frame(u)
  ell = mu - (lambda/c)
  df$coefficient = exp(u * ell)
  return(df)
}

cramerCoefficient = cramerCoefficient(lambda, mu, u, c)

cramerProduct = merge(cramerCoefficient, estProbRuin, by='u')
cramerProduct$product = cramerProduct$coefficient * cramerProduct$estimation


############################## VISUALIZE #######################################

g4 = ggplot() +
  geom_line(data = cramerProduct,
            aes(x = u, y = product),
            color = "cyan") +
  geom_hline(yintercept = 2/3, 
             color = "red", 
             linetype = "dashed") + 
  labs(title = glue(" \u03BB={lambda}, \u03BC={mu}, c={c}"),
       x = "Začetni kapital (u)",
       y = glue("Asimptotika verjetnosti propada (\u03C8(u))"))

g4

#Save graph to pdf
ggsave("C:/Users/38651/OneDrive - Univerza v Ljubljani/Desktop/Diploma/Diplomski-seminar/GraphsAndPhotos/slika4.pdf",
       g4,
       device = "pdf",
       width = 8,
       height = 5)


#------------------------------------------------------------------------------#
#Slika 5 (Asymptotics for large claim case)

set.seed(42)
library(ggplot2)
library(dplyr)
library(actuar)
library(gridExtra)
library(glue)
library(viridis)

# Parameters
lambda = 1 # Intensity of HPP
a = 1/4 
b = 16 # Claim size parameters
T = 1000 # Time horizon of simulation
u = seq(2000, 100000, 2000) # Initial reserve
c = 500 # Premium income rate
N = c(10, 100, 250) # Number of simulations 

########################## RISK PROCESS SIMULATION #############################

estimateProbabilityOfRuin = data.frame(u, matrix(0, ncol = length(N), nrow = length(u)))
colnames(estimateProbabilityOfRuin) = c("u", as.character(N))

# Simulate arrival times
for (sim in N){
  cname = as.character(sim)
  print(glue("Number of simulations in batch:{cname}"))
  for (j in 1:length(u)) {
    print(glue("simulation:{j}/{length(u)}"))
    for (k in 1:sim){
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
      
      # Generate weibull distributed claims
      claims = rweibull(length(arrivalTimes)/2-1, a, b)
      
      # Simulate risk process
      riskProcessW = data.frame(arrivalTimes)
      riskProcessW$premiumRevenue = u[j] + riskProcessW$arrivalTimes*c
      riskProcessW$cumulativeClaims = 0
      for (i in 2:length(arrivalTimes)) {
        if (i %% 2 == 1) {
          riskProcessW[i, 'cumulativeClaims'] = riskProcessW[i-1, 'cumulativeClaims'] + claims[(i-1)/2]
        } else {
          riskProcessW[i, 'cumulativeClaims'] = riskProcessW[i-1, 'cumulativeClaims'] 
        }
      }
      riskProcessW$riskProcess = riskProcessW$premiumRevenue - riskProcessW$cumulativeClaims
      # Check for ruin
      for (i in 1:nrow(riskProcessW)){
        if (riskProcessW$riskProcess[i] < 0){
          estimateProbabilityOfRuin[cname][j, 1] = estimateProbabilityOfRuin[cname][j, 1] + 1
          break
        }
      }
    }
  }
  # Normalize frequencies
  estimateProbabilityOfRuin[cname] = estimateProbabilityOfRuin[cname] / sim
}

# convert expression from latex to function \frac{\left(u^{\frac{3}{4}} + 6 \sqrt{u} + 24 \sqrt[4]{u} + 48\right)e^{-\frac{\sqrt[4]{u}}{2}}}{48}
tailOfIntegratedTailDistribution = function(u){
  df = data.frame(u)
  df$tail =((u^(3/4) + 6*sqrt(u) + 24*u^(1/4) + 48)*exp(-(u^(1/4)/2)))/48
  return(df)
}

tailOfIntegratedTailDistribution = tailOfIntegratedTailDistribution(u)

# Merge dataframes
asymptotic = merge(estimateProbabilityOfRuin, tailOfIntegratedTailDistribution, by='u')

# Calculate raio for each simulation
ratios = c()
for (i in seq_along(N)){
  asymptotic[glue("Ratio{as.character(N)[i]}")] = asymptotic[as.character(N)[i]] / asymptotic$tail
  ratios = c(ratios, glue("Ratio{as.character(N)[i]}"))
}

# Remove last row
#asymptotic = asymptotic[-nrow(asymptotic),]

############################## VISUALIZE #######################################
g5 = ggplot() +
  geom_hline(yintercept = 3.310345, 
             color = "black", 
             linetype = "dashed") + 
  labs(title = glue(" \u03BB={lambda}, a={a}, b={b}, c={c}"),
       x = "Začetni kapital (u)",
       y = glue("Asimptotika verjetnosti propada (\u03C8(u))"))
  
colors = c("cyan", "red", "blueviolet", "darkseagreen")
for (i in seq_along(N)){
  g5 = g5 + geom_line(data = asymptotic,
                      aes(x = u, y = !!sym(as.character(ratios[i]))),
                      color = colors[i])
}

g5

#Save graph to pdf
ggsave("C:/Users/38651/OneDrive - Univerza v Ljubljani/Desktop/Diploma/Diplomski-seminar/GraphsAndPhotos/slika5.pdf",
       g5,
       device = "pdf",
       width = 8,
       height = 5)

#------------------------------------------------------------------------------#
