# Anej Rozman
# Financna matematika, diplomsko delo
# Code for generating graphs\pictures

library(ggplot2)
library(dplyr)
library(actuar)
library(gridExtra) 
library(tidyr)
library(latex2exp)
library(glue) # f-string implementation

# To save files to a specific location, add FILEPATH, and uncomment relevant code
# Above every simulation there is a section "PARAMETERS" that can be 
# changed if so desired. 

FILEPATH = NULL

#-------------------------------------------------------------------------------#

# Slika 1 (Poisson process and compound Poisson process sample path)

set.seed(42)

# PARAMETERS
nSteps = 150
lambda = 0.1  # Intensity of HPP
alpha = 20 # Shape parameter for exponential distribution

# Generate sample path of HPP and CPP
jumpsHpp = rpois(nSteps, lambda)
samplePath = data.frame(time = 1:nSteps, 
                        jumpTimes = jumpsHpp, 
                        gamma = rgamma(nSteps, shape = alpha, rate = 1))

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
  labs(title = lambda == 0.1~""~alpha == 20,
       x = ~"Čas",
       y = "Vrednost") +
  scale_size_identity()

g1

# Save graph to pdf
#ggsave(glue("{FILEPATH}slika1.pdf"),
#       g1,
#       device = "pdf",
#       width = 8,
#       height = 5)

#-------------------------------------------------------------------------------#

# Slika 2 (Simulation of two sample paths of risk process with Weibull claims,
# one with heavy tails and one with light tails."

set.seed(11)

# PARAMETERS
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
    riskWeibullLight[i, 'cumulativeClaims'] = 
      riskWeibullLight[i-1, 'cumulativeClaims'] + lightClaims[(i-1)/2]
  } else {
    riskWeibullLight[i, 'cumulativeClaims'] = 
      riskWeibullLight[i-1, 'cumulativeClaims'] 
  }
}
riskWeibullLight$riskProcess = 
  riskWeibullLight$premiumRevenue - riskWeibullLight$cumulativeClaims

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

# Remove the last arrival time exceeding T 
arrivalTimesHeavy = arrivalTimesHeavy[arrivalTimesHeavy < T]
arrivalTimesHeavy = c(arrivalTimesHeavy, T)

# Generate claims
heavyClaims = rweibull(length(arrivalTimesHeavy)/2-1, aHeavy, bHeavy)

# Simulate risk process
riskWeibullHeavy = data.frame(arrivalTimesHeavy)
riskWeibullHeavy$premiumRevenue = u + riskWeibullHeavy$arrivalTimesHeavy*c
riskWeibullHeavy$cumulativeClaims = 0
for (i in 2:length(arrivalTimesHeavy)) {
  if (i %% 2 == 1) {
    riskWeibullHeavy[i, 'cumulativeClaims'] = 
      riskWeibullHeavy[i-1, 'cumulativeClaims'] + heavyClaims[(i-1)/2]
  } else {
    riskWeibullHeavy[i, 'cumulativeClaims'] = 
      riskWeibullHeavy[i-1, 'cumulativeClaims'] 
  }
}
riskWeibullHeavy$riskProcess = 
  riskWeibullHeavy$premiumRevenue - riskWeibullHeavy$cumulativeClaims

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
#ggsave(glue("{FILEPATH}slika2.pdf"),
#       g2,
#       device = "pdf",
#       width = 8,
#       height = 6)

#-------------------------------------------------------------------------------#
#Slika 3 (Monte Carlo simulation for approximating ruin proability)

# Comment: 
# 1.) If you create more than 4 batches of simulations you have to manually 
# chose more colors when plotting. (approx. line 340)
# 2.) With current parameters takes approx. 10 minutes to execute simulations.

set.seed(42)

# Parameters
lambda = 1 # Intensity of HPP
mu = 1/1000 # Claim size parameter
T = 1000 # Time horizon of simulation
u = seq(500, 25000, 500) # Initial reserve
c = 1500 # Premium income rate
N = c(10, 25, 100) # Number of simulations 

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
      
      # Remove the last arrival time exceeding T 
      arrivalTimes = arrivalTimes[arrivalTimes < T]
      arrivalTimes = c(arrivalTimes, T)
      
      # Generate claims
      claims = rexp(length(arrivalTimes)/2-1, mu)
      # Simulate risk process
      riskProcessExp = data.frame(arrivalTimes)
      riskProcessExp$premiumRevenue = u[j] + riskProcessExp$arrivalTimes*c
      riskProcessExp$cumulativeClaims = 0
      for (i in 2:length(arrivalTimes)) {
        if (i %% 2 == 1) {
          riskProcessExp[i, 'cumulativeClaims'] = 
            riskProcessExp[i-1, 'cumulativeClaims'] + claims[(i-1)/2]
        } else {
          riskProcessExp[i, 'cumulativeClaims'] = 
            riskProcessExp[i-1, 'cumulativeClaims'] 
        }
      }
      riskProcessExp$riskProcess = 
        riskProcessExp$premiumRevenue - riskProcessExp$cumulativeClaims
      
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

# Pivot dataframe for plotting
pOfRuinLong = pOfRuin %>% 
  pivot_longer(cols = -u, 
               names_to = "N", 
               values_to = "value")

############################## VISUALIZE #######################################

g3 = ggplot(data = pOfRuin) 
colors = c("cyan", "red", "blueviolet", "black")

g3 = g3 + geom_line(data = pOfRuinLong,
                    aes(x = u, y = value, color = N)) +
          labs(title = expression(lambda == 1 ~ "" ~ mu == 0.001 ~ " c = 1500"),
                    x = "Začetni kapital (u)",
                    y = expression(Verjetnost~propada~(psi(u)))
                  ) + 
          scale_color_manual(
            values = c("10"="cyan", "25"="red", "100"="blueviolet", "exactProbability"="black"), 
            name = "Število simulacij",
            labels = c("10 simulacij", "100 simulacij", "25 simulacij", "točna vrednost")
            ) +  
          theme(legend.position = c(0.8, 0.65),  
                legend.box.background = element_rect(color = "black", size = 0.5), 
                legend.key.size = unit(2, "lines"),  
                legend.text = element_text(size = 12) )  

g3
          
# Save graph to pdf
#ggsave(glue("{FILEPATH}slika3.pdf"),
#       g3,
#       device = cairo_pdf,
#       width = 8,
#       height = 5)

#------------------------------------------------------------------------------#
#Slika 5 (Asymptotics for large claim case, Weibull distributed claims)

set.seed(42)

# Comment: 1.) If claim size parameters are changed the density function of the 
# integrated tail distribution must be recalculated. 
# 2.) Since the number of simutaions if very high this section of code takes 
# approx. 30 minutes to execute.

# PARAMETERS
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
          riskProcessW[i, 'cumulativeClaims'] = 
            riskProcessW[i-1, 'cumulativeClaims'] + claims[(i-1)/2]
        } else {
          riskProcessW[i, 'cumulativeClaims'] = 
            riskProcessW[i-1, 'cumulativeClaims'] 
        }
      }
      riskProcessW$riskProcess = 
        riskProcessW$premiumRevenue - riskProcessW$cumulativeClaims
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

# Density of integrated tail distribution
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

# Povot long for visualization
asymptoticLong = asymptotic

asymptoticLong = asymptotic[, c("u", "Ratio10", "Ratio100", "Ratio250")]
asymptoticLong = asymptoticLong %>% 
  pivot_longer(cols = -u, 
               names_to = "N", 
               values_to = "value")

############################## VISUALIZE #######################################

g5 = ggplot(data = asymptoticLong)
colors = c("cyan", "red", "blueviolet")

g5 = g5 + geom_line(data = asymptoticLong,
                    aes(x = u, y = value, color = N)) +
          labs(title = expression(lambda == 1 ~"a=0.25 b=16 c=1500"),
               x = "Začetni kapital (u)",
               y = expression(Verjetnost~propada~(psi(u)))
               ) + 
          geom_hline(yintercept = 3.310345, 
                                  color = "black", 
                                  linetype = "dashed") + 
          annotate("text", x = 6000, y = 3.4, label = expression(1/rho), hjust = 1.1, vjust = -0.5) + 
          scale_color_manual(values = colors, 
                             name = "Število simulacij",
                             labels = c("10 simulacij", "100 simulacij", "250 simulacij")) +  
          theme(legend.position = c(0.15, 0.75),  
                legend.box.background = element_rect(color = "black", size = 0.5), 
                legend.key.size = unit(2, "lines"),  
                legend.text = element_text(size = 12) )

g5

#Save graph to pdf
#ggsave(glue("{FILEPATH}slika5.pdf"),
#       g5,
#       device = cairo_pdf,
#       width = 8,
#       height = 5)

#------------------------------------------------------------------------------#
#Slika 6 (CPP standard simulation)

set.seed(42)

# PARAMETERS
lambda = 1 # Intensity of HPP
mu = 2
sigma = 42# Parameters for normal distribution
T = 1000 # Time horizon of simulation
N = 10 # Number of simulations of CPP

############################# SIMULATION #######################################

# Simulate arrival times
arrivalTimes = c(0)
t = 0
while (t <= T) {
  interarrivalTime = rexp(1, rate = lambda)
  t = t + interarrivalTime
  arrivalTimes = c(arrivalTimes, t)
}

# Simulate CPP
sim = data.frame(arrivalTimes)
sim$EX = mu*lambda*sim$arrivalTimes
sim$VarX = lambda*arrivalTimes*(mu^2 + sigma^2)
simulations = c()
for (i in 1:N){
  sim[glue("X{i}")] = rnorm(length(arrivalTimes), mu, sigma)
  sim[glue("CPP{i}")] = cumsum(sim[glue("X{i}")])
  simulations = c(simulations, glue("CPP{i}"))
}

######################## VISUALIZE #############################################

colors = c("cyan", "red", "blueviolet", "darkorange", 
           "cyan4", "red4", "darkorchid4", "darkorange3", 
           "aquamarine", "brown1","cornflowerblue",  "darkgoldenrod1" )

g6 = ggplot() 

for (i in 1:N){
  g6 = g6 + geom_line(data = sim,
                      aes(x = arrivalTimes, y = !!sym(as.character(simulations[i]))),
                      color = colors[i])
}
g6 = g6 + geom_line(data = sim,
            aes(x = arrivalTimes, y = EX),
            size = 0.8,
            color = "black") +
          geom_line(data = sim,
                    aes(x = arrivalTimes, y = EX +3*sqrt(VarX)),
                    size = 0.7,
                    color = "black") +
          geom_line(data = sim,
                    aes(x = arrivalTimes, y = EX - 3*sqrt(VarX)),
                    size = 0.7,
                    color = "black") +
          labs(title = expression(lambda == 1 ~mu == 2~sigma == 42),
               x = "Čas (t)",
               y = TeX(r'(Vrednost $(S_t)$ )')) 

g6

# save graph to pdf
#ggsave(glue("{FILEPATH}slika6.pdf"),
#       g6,
#       device = cairo_pdf,
#       width = 8,
#       height = 5)

#------------------------------------------------------------------------------#
#Slika 7 (Panjer recursion)

set.seed(42)

# PARAMETERS
lambda = 5 # Intensity of Poisson distribution
a = 1/3.1415 # Shape parameter for exponential distribution
h = c(1, 0.1) # Steps in Panjer recursion
limX = 10
limS = 100

############# UPPER APPROXIMATION OF EXPONENTIAL DISTRIBUTION ##################

# Create dataframe with exponential cdf 
x = seq(0, limX-0.001, by = 0.001)
cdfUpper = data.frame(x, cdf = pexp(x, rate = a))

# Discretize the cdf of the exponential distribution
for (i in 1:length(h)){
  cdfUpper[glue("discreteUpper{h[i]}")] = rep(
    cumsum(actuar::discretize(pexp(x, rate = a), 0, limX, h[i], method="upper")), 
    each = 1/(0.001/h[i])
    )
}

# Plot 
g7upper = ggplot(data=cdfUpper)
g7upper = g7upper + geom_line(aes(x = x, y = cdf), color = "black") +
  geom_line(aes(x = x, y = discreteUpper1), color = "cyan") +
  geom_line(aes(x = x, y = discreteUpper0.1), color = "red") +
  labs(title = expression(Zgornja~aproksimacija~porazdelitve~"X~Exp( 1/"~pi~")"),
       x = "x",
       y ="F_X(x)"
  ) 
g7upper

############# LOWER APPROXIMATION OF EXPONENTIAL DISTRIBUTION ##################

# Discretize the cdf of the exponential distribution

cdfLower = data.frame(x, cdf = pexp(x, rate = a))

for (i in 1:length(h)){
  cdfLower[glue("discreteLower{h[i]}")] = rep(
    cumsum(actuar::discretize(pexp(x, rate = a), 0, limX, h[i], method="lower")), 
    each = 1/(0.001/h[i])
  )[1:10000]
}

# Plot
g7lower = ggplot(data=cdfLower)
g7lower = g7lower + geom_line(aes(x = x, y = cdf), color = "black") +
  geom_line(aes(x = x, y = discreteLower1), color = "blueviolet") +
  geom_line(aes(x = x, y = discreteLower0.1), color = "darkorange") +
  labs(title = expression(Spodnja~aproksimacija~porazdelitve~"X~Exp( 1/"~pi~")"),
       x = "x",
       y ="F_X(x)"
  )

g7lower

#################### PANJER RECURSION ##########################################

# Discretization of exponantial distribution  
expUpper1 = discretize(pexp(x, a), 0, limS, by = 1, method = "upper")
expUpper0.1 = discretize(pexp(x, a), 0, limS, by = 0.1, method = "upper")
expLower1 = discretize(pexp(x, rate = a), 0, limS, by = 1, method = "lower")[1:100]
expLower0.1 = discretize(pexp(x, rate = a), 0, limS, by = 0.1, method = "lower")[1:1000]

# Panjer recursion functions
panjerUpper1 = aggregateDist(method = 'recursive', 
                             model.freq = 'poisson',
                             lambda = lambda,
                             model.sev = expUpper1,
                             x.scale = 1, 
                             tol = 0.001,
                             maxit = 100000000)
panjerUpper0.1 = aggregateDist(method = 'recursive',
                               model.freq = 'poisson', 
                               lambda = lambda,
                               model.sev = expUpper0.1,
                               x.scale = 0.1,
                               tol = 0.001,
                               maxit = 100000000)
panjerLower1 = aggregateDist(method = 'recursive',
                             model.freq = 'poisson', 
                             lambda = lambda,
                             model.sev = expLower1,
                             x.scale = 1,
                             tol = 0.001,
                             maxit = 100000000)
panjerLower0.1 = aggregateDist(method = 'recursive',
                               model.freq = 'poisson', 
                               lambda = lambda,
                               model.sev = expLower0.1,
                               x.scale = 0.1,
                               tol = 0.001,
                               maxit = 100000000)

# Plot 
li = seq(0, 60, by = 0.1)
g7panjer = ggplot()
g7panjer = g7panjer + 
  geom_step(aes(x = li, y = panjerUpper1(li)), color = "cyan") + 
  geom_step(aes(x = li, y = panjerUpper0.1(li)), color = "red") + 
  geom_step(aes(x = li, y = panjerLower1(li)), color = "blueviolet") +
  geom_step(aes(x = li, y = panjerLower0.1(li)), color = "darkorange") +
  labs(title =  TeX(r'( Aproksimacija porazdelitve $S = \sum_{i=1}^NX_i$ s Panjerjevo rekurzivno shemo)'),
       x = "x",
       y ="F_S(x)"
  )
  
g7panjer

########################### VISUALIZE ##########################################

#Combine plots
g7approx = grid.arrange(g7upper, g7lower, ncol = 2)
g7 = grid.arrange(g7approx, g7panjer, heights = c(4, 6))

g7

# Save graph to pdf
#ggsave(glue("{FILEPATH}slika7.pdf"),
#       g7,
#       device = cairo_pdf,
#       width = 12,
#       height = 10)

