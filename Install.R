#Run the first time you install the app to install the required Packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(shiny, DT, bslib, shinyalert, dplyr, stringr, tidyr, tibble, lubridate, data.table, openair)
pacman::p_loaded() # Prints out loaded Packages to check if all were loaded correctly
