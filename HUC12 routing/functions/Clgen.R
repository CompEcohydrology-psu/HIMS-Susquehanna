################################################################################
# This function calculates the daylength, distribution of ... 

# radiation throughout the day and maximum radiation for day

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

# References: 

# SWAT source codes - https://swat.tamu.edu/software/swat/

# SWAT 2009 theory

################################################################################


clgen<-function(iida,routingParams){
  
  
  lat <- routingParams$lat  # latitude of subbasin
 
  xx <- 0
 
  xx <- lat / 57.296 # convert degrees to radians (2pi/360=1/57.296)
                          
  latsin <- sin(xx)
	  
  latcos <- cos(xx)
  
  # Reset prior day category for precipitation
  
  #if (percp_day >= 0.1) {
  
   # npcp <- 2
  
 # }else{
    
  #  npcp <- 1
  
  #}
  
  # Calculate Daylength 
  
    # calculate solar declination: equation 2.1.2 in SWAT manual
  
  sd <- 0.
  
  sd <- asin(.4 * sin((iida - 82.) / 58.09))  # 365/2pi = 58.09
  
  
  # calculate the relative distance of the earth from the sun
  
  # the eccentricity of the orbit
  
  # equation 2.1.1 in SWAT manual
  
  dd <- 0.
  
  dd <- 1.0 + 0.033 * cos(iida / 58.09)
  
  
  # daylength = 2 * Acos(-Tan(sd) * Tan(lat)) / omega
  
  # where the angular velocity of the earth's rotation, omega, is equal
  
     # to 15 deg/hr or 0.2618 rad/hr and 2/0.2618 = 7.6374
  
      # equation 2.1.6 in SWAT manual
  
  ch <- 0.
  
  h <- 0.
  
  ch <- -latsin * tan(sd) / latcos
  
  
  if (ch > 1.) {    # ch will be >= 1. if latitude exceeds +/- 66.5 deg in winter
    
  h <- 0.
  
  }else if (ch >= -1.) {
    
  h <- acos(ch)
  
  }else{
    
    h <- 3.1416        # latitude exceeds +/- 66.5 deg in summer
    
  }
  
  dayl <- 7.6394 * h
  
  
  return(dayl)
  
  
} # end of function


