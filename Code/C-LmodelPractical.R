# Anej Rozman
# Financna matematika, diplomsko delo
# Code for generating graphs 

library(actuar)
library(cascsim)
library(ggplot2)
library(dplyr)
library(gridExtra)

#------------------------------------------------------------------------------#

# Load light tailed insurance data and filter relevant info.
autoClaims = read.csv('lahkorepniZahtevki.csv')

# Because the original dataset is meant to be studied to detect fraud, I remove 
# the potential fraudulent data
#autoClaims = autoClaims[autoClaims$fraud_reported == "N",]

# Remove unnecessary data and rename columns
autoClaims = autoClaims[,c('incident_date', 
                           'total_claim_amount', 
                           'injury_claim', 
                           'property_claim', 
                           'vehicle_claim')]
names(autoClaims) = c('time', 
                      'total_claim_amount', 
                      'injury_claim', 
                      'property_claim', 
                      'vehicle_claim')
autoClaims$time = as.Date(autoClaims$time)

# Injury claims histogram
plotInjuryClaims = ggplot(autoClaims[autoClaims$injury_claim != 0, ], aes(x = injury_claim)) +
  geom_histogram(binwidth = 100, fill = "red", color = "black") +
  labs(title = "Histogram velikosti zahtevkov za poskodbe",
       x = "Velikost zahtevkov",
       y = "Stevilo zahtevkov")

# Vehicle claims histogram
plotVehicleClaims = ggplot(autoClaims, aes(x = vehicle_claim)) +
  geom_histogram(binwidth = 500, fill = "cyan", color = "black") +
  labs(title = "Histogram velikosti zahtevkov za vozilo",
       x = "Velikost zahtevkov",
       y = "Stevilo zahtevkov")

# Combine plots
plotClaims = grid.arrange(plotInjuryClaims, 
                  plotVehicleClaims, 
                  nrow = 2)
# Save graph to pdf
ggsave("C:/Users/38651/OneDrive - Univerza v Ljubljani/Desktop/Diploma/Diplomski-seminar/GraphsAndPhotos/slika2.pdf",
       plotClaims,
       device = "pdf",
       width = 8,
       height = 6)

# Plot for claim frequency
plotArrivals = ggplot(autoClaims, aes(x = time)) +
  geom_bar(fill = "red", color = "black") +
  labs(title = "Frekvenca prihodov",
       x = "Cas",
       y = "Stevilo prihodov")+
  scale_x_date(date_breaks = "15 days")

# Save graph to pdf
ggsave("C:/Users/38651/OneDrive - Univerza v Ljubljani/Desktop/Diploma/Diplomski-seminar/GraphsAndPhotos/slika3.pdf",
       plotArrivals,
       device = "pdf",
       width = 8,
       height = 4)

#------------------------------------------------------------------------------#


# Function to calculate the Pareto PDF
pareto_pdf <- function(x, alpha, xmin) {
  ifelse(x >= xmin, alpha * xmin^alpha / x^(alpha+1), 0)
}

# Parameters
alpha <- 2  # Shape parameter
xmin <- 1   # Scale parameter

# Generate x values
x_values <- seq(xmin, 10, by = 0.1)  # Adjust range as needed

# Calculate PDF values
pdf_values <- pareto_pdf(x_values, alpha, xmin)

# Create a data frame
df <- data.frame(x = x_values, pdf = pdf_values)

# Plot
ggplot(df, aes(x, pdf)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Pareto Distribution PDF",
       x = "x",
       y = "Density")



