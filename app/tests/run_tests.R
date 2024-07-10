# Load the circular package
#library(circular)
#library(rjson)

if (!require("pacman")) install.packages ("pacman")
require('pacman')

p_load(data.table,shiny,shinyFiles,shinycssloaders,circular,ggplot2,shinyWidgets,tools,grid,gridExtra,tidyverse,CircStats,readxl,rjson,optparse, install=TRUE, update=FALSE)

source("../src/circular_statistics.R")

# Generate angular values from a von Mises distribution
# mu is the mean direction, kappa is the concentration parameter, and n is the number of observations
mu <- 0  # Mean direction in radians
kappa <- 2  # Concentration parameter
n <- 100  # Number of observations

input <- list(
    circ_units = "radians",
    cond_mean_direction = 180.0
)

directional_values <- rvonmises(n, mu = circular(mu), kappa = kappa)

# Convert to a data frame
directional_data_df <- data.frame(angles = directional_values)

# Print the first few rows of the data frame
head(df)

axial_values <- directional_values/2.0

# Convert to a data frame
axial_data_df <- data.frame(angles_rad = axial_values, angles_deg = axial_values*180.0/pi)

parameters <- fromJSON(file = "../parameters/parameters.json")

print(axial_data_df)

#parameters

statistics <- compute_axial_statistics(axial_data_df, "angles_rad", input, parameters)

print("Output in radians")
statistics

input$circ_units <- "degrees"

statistics <- compute_axial_statistics(axial_data_df, "angles_deg", input, parameters)

print("Output in degrees")
statistics

circular_data <- circular_unit_conversion(axial_values*180.0/pi, input$circ_units, "radians")
circular_data