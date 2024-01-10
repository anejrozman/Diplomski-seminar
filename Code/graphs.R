# Anej Rozman
# Financna matematika, diplomsko delo
# Code for generating graphs 

library(simmer)
library(ggplot2)

set.seed(42)

#----------------------------------------------------------------------------------#

# Slika 1

# Function for simulating compound Poisson process
compound_poisson_process <- function(lambda, distribution_function, max_time) {
  env <- simmer()
  
  # Add generator for arrivals
  env %>% add_generator("arrivals", 
                        trajectory() %>% timeout(function() rexp(1, lambda)),
                        function() rpois(1, lambda))
  
  # Add generator for amounts
  env %>% add_generator("amounts",
                        trajectory() %>% set_attribute("amount", function() distribution_function()),
                        function() 1)  # En prihod za vsako vrednost v distribution_function
  
  # Link generators
  env %>% add_resource("server", capacity = Inf) %>%
    seize_selected(resources = "server", rule = "smallest") %>%
    timeout_from_attribute("amount") %>%
    release_selected(resources = "server")
  
  # Run simulation
  env %>% run(until = max_time)
  
  # Get data
  arrivals_data <- get_mon_arrivals(env)
  amounts_data <- get_mon_attributes(env, "amount", filter = "amounts")
  
  return(list(arrivals = arrivals_data, amounts = amounts_data))
}

# Set parameters
lambda <- 5  
max_time <- 100  

# Distribution of claim amounts
amount_distribution <- function() {rexp(1, rate = 0.2)}

# Simulate compound Poisson process
sim_data <- compound_poisson_process(lambda, amount_distribution, max_time)

# Plot results
ggplot(sim_data$arrivals, aes(x = time)) +
  geom_point(aes(y = from_resource("arrivals")), color = "blue") +
  geom_point(aes(y = from_resource("amounts")), color = "red") +
  labs(title = "Sestavljen Poissonov proces",
       x = "Čas",
       y = "Število prihodov") +
  theme_minimal()

#----------------------------------------------------------------------------------#

# Slika 2


#----------------------------------------------------------------------------------#
