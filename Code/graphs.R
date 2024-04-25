# Anej Rozman
# Financna matematika, diplomsko delo
# Code for generating graphs 

library(ggplot2)
library(dplyr)

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

# Slika 2



#-------------------------------------------------------------------------------#
