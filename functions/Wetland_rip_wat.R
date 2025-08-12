################################################################################

# Routing water through riparian wetland

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

# References: 

# 1. SWAT source codes

# 2. SWAT 2009 theory

# 3. Liu, Y., Yang, W., & Wang, X. (2008). Development of a SWAT extension module to

# simulate riparian wetland hydrologic processes at a watershed scale. 

# Hydrological Processes: An International Journal, 22(16), 2901-2915.

################################################################################


Wetland_rip_wat <- function(watyld, S_wet, routingParams){ 


  
  
  # watyld: Water from upland area [m3]. 
  
  # S_wet: Water flux between wetland and channel [m3].
  
  # routingParams: List of routing parameters.
  
  
  wet_vol <- routingParams$wet_vol      # volume of water in wetland at begining of day [m3].
  
  wet_nvol <- routingParams$wet_nvol    # wetland normal volume [m3].
  
  wet_nsa <- routingParams$wet_nsa      # Surface area of wetland at normal water level [ha].
  
  wet_mxsa <- routingParams$wet_mxsa    # Surface area of wetlands at maximum water level [ha].
  
  wet_mxvol <- routingParams$wet_mxvol  # Volume of water stored in wetlands when filled to maximum water level [m3].
  
  wet_k <- routingParams$wet_k          # Hydraulic conductivity of soil at the place of wetland [mm/hr].
  
  wet_fr <- routingParams$wet_fr        # drainage area fraction [-]
  
  
  
  wet_nsa <-  wet_nsa * 10000 # covert to m2.
  
  wet_mxsa <- wet_mxsa * 10000 # convert to m2.
  
  
  # calculate shape parameters for surface area equation:
  
 # wetdif <- 0.
  
 # wetdif <- wet_mxvol - wet_nvol
  
 # if ((wet_mxsa - wet_nsa) > 0. & wetdif > 0.) {
    
   #   lnvol <- 0.
    
    #  lnvol <- log10(wet_mxvol) - log10(wet_nvol)
    
   # if (lnvol > 1.e-4) {
      
    #  bw2 <- (log10(wet_mxsa) - log10(wet_nsa)) / lnvol
      
   # } else {
      
   #   bw2 <- (log10(wet_mxsa) - log10(wet_nsa)) / 0.001
      
  #  }
    
  #  if (bw2 > 0.9) {bw2 <- .9}
    
   #   bw1 <- (wet_mxsa / wet_mxvol) ^ bw2
    
 # } else {
    
   #   bw2 <- .9
    
   #   bw1 <- (wet_nsa / wet_nvol) ^ .9
    
  #} 



  
  if (wet_fr > 0.) {  
    
    # store initial values ----------------------------------------------------
    
    vol <- 0.  
    
    vol <- wet_vol  
    
    
    # calculate water balance for day -----------------------------------------
    
    # Precipitation and evaporation are accounted in the landscape process.
    
    wetsa <- 0. 
    
   # wetsa <- bw1 * wet_vol ^ bw2 # surface area of wetland on current day [m2].

     wetsa <- wet_nsa
    
    # wetev <- 10. * evwet * pet_day * wetsa
    
    wetsep <- wet_k * wetsa * 0.024  # seepage from wetland on day [m3] 
    
    # wetpcp <- percp_day * wetsa * 10.
    
    
    
    
    # calculate water flowing into wetland from upland area ---------------------------
    
    
    wetflwi <- watyld 
    
    
    # new water volume for day ------------------------------------------------
    
    wetev <- 0  # accounted in landscape
    
    wetpcp <- 0 # accounted in landscape
    
    wet_vol <- wet_vol - wetsep - wetev + wetpcp + wetflwi 
	
    wetflwo <- 0 

    wetdep <- 0
    
	
	if(S_wet < 0){ # channel >> wetland
    
	
	  wet_vol <- wet_vol -  S_wet

	
	}
	
	
	
	if (wet_vol < 0.001) { # r2
      
      # check for volume deficit in wetland
      
      # reduce seepage so that the wetland volume is zero
      
      
      wetsep <- wetsep + wet_vol
      
      wet_vol <- 0 

      wet_voli <- wet_vol # water volume before release [m3].

	 
    }else{

      wet_voli <- wet_vol

	
	if(S_wet >= 0){ # channel >> wetland
	
	
	  wetflwo <- S_wet  # wetland outflow [m3].
    
	
	  wet_vol <- wet_vol -  wetflwo
	
	
	
	}
	
}
    
	if(wet_vol < 0.001){

      wetdep <- 0

    }else{
         
    #  wetdep <- bw1 ^ (-1) * wet_vol ^ (1-bw2)   # Water depth in wetland [m].

	 wetdep <- wet_vol / wet_nsa  
  }

    
  }  # r1

  
  output<-c(wet_vol,wetdep,wet_voli,wetflwo)
  
  
  return(output)
  
  
  
} # function
