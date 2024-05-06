# Anej Rozman
# Financna matematika, diplomsko delo
# Code for generating graphs\pictures

library(ggplot2)
library(dplyr)
library(actuar)
library(gridExtra)

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

set.seed(99)

# Parameters
lambda = 0.3 # Intensity of HPP
T = 30 # Time horizon of simulation
u = 500 # Initial reserve
c = 10 # Premium income rate

################################## LIGHT TAILS #################################

# Light tailed Weibull
aLight = 2 # Shape
bLight = 100 # Rate

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
arrivalTimesLight = c(arrivalTimesLight, T)

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
aHeavy = 0.5 # Shape
bHeavy = 29.54 # Rate

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
arrivalTimesHeavy = c(arrivalTimesHeavy, T)

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
          labs(title = "realizacija procesa tveganja z Weibullovo porazdeljenimi zahtevki",
               x = "Čas t",
               y = "Proces tveganja U(t)")
g2riskProcess
          
# Densities

# Light
lightDensity = data.frame(x=1:1000, y=dweibull(1:1000, aLight, bLight))

g2lightDensity = ggplot() + geom_line(data = lightDensity, 
                                      aes(x, y), 
                                      size = 0.7, 
                                      color = 'red') +
                            labs(title = 'Gostota weibullove slucajne spremenljvke', 
                                 x = 'x', 
                                 y = 'f_X(x)')

g2lightDensity

# Heavy
heavyDensity = data.frame(x=1:1000, y=dweibull(1:1000, aHeavy, bHeavy))

g2heavyDensity = ggplot() + geom_line(data = heavyDensity, 
                                      aes(x, y), 
                                      size = 0.7, 
                                      color = 'cyan3') +
                            labs(title = 'Gostota weibullove slucajne spremenljvke', 
                                 x = 'x', 
                                 y = 'f_X(x)')

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

