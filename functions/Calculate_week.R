################################################################################
# This function calculate week number based on hydrological/water year ...

# for reservoir operation (STARFIT Algorithm) October >> September

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

# References:

# Turner, S. W., Doering, K., & Voisin, N. (2020). Data-driven reservoir ...

# simulation in a large-scale hydrological and water resource model... 

# Water Resources Research, 56(10), e2020WR027902.

# Turner, S. W., Steyaert, J. C., Condon, L., & Voisin, N. (2021). Water ...

# storage and release policies for all large reservoirs of conterminous ... 

# United States. Journal of Hydrology, 603, 126843.

################################################################################

# set R library path here ...


library(lubridate)


Calculate_week <- function(date) {
  # Convert the input to a Date object if it's not already
  date <- as.Date(date)
  
  # Extract the year and convert it to numeric
  year <- as.numeric(format(date, "%Y"))
  
  # Determine the start date of the current or previous hydrological year
  current_year_start <- as.Date(paste0(year, "-10-01"))
  previous_year_start <- as.Date(paste0(year - 1, "-10-01"))
  
  # Determine the relevant year start date based on the input date
  if (date < current_year_start) {
    year_start <- previous_year_start
  } else {
    year_start <- current_year_start
  }
  
  # Calculate the number of complete weeks since the start of the hydrological year
  week_number <- as.integer(difftime(date, year_start, units = "weeks")) + 1
  
  return(week_number)
}
