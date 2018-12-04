
#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr, 
               doBy, 
               readxl,
               lubridate,
               ggplot2,
               knitr,
               imputeTS,
               matrixStats,
               DEoptim,     # For DAMM model parameter optimality
               car,         # For stats
               lme4,        # For stats
               LMERConvenienceFunctions,  # For stats
               multcomp,    # For stats
               #lmerTest,    # For stats
               pbkrtest,    # For stats
               cowplot,
               viridis,
               sciplot)    


#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("scripts", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)


