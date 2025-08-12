
################################################################################

# Routing nutrients through riparian wetland

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

# References: 

# 1. SWAT source codes

# 2. SWAT 2009 theory


################################################################################

Wetland_rip_nut <- function(watyld,wet_vol,wet_voli, S_wet, psolyld, psedyld,
                            
                            orgnyld, no3yld, varoute_2, varoute_4,
                            
                            varoute_5,varoute_6,varoute_7,
                            
                            varoute_14,varoute_15, routingParams, i_mo ){ 
  
  
  
  # watyld:    Water from upland area [m3].
  
  # wet_vol:   Water stored in wetland at end of day [m3].
  
  # wet_voli:  Water stored in wetland before realease [m3].
  
  # S_wet:     Water exchange between wetland and channel [m3].
  
  # orgnyld:   Organic Nitrogen from upland area [kg]. 
  
  # psedyld:   Sediment attached Phosphorous from upland area [kg].
  
  # no3yld:    Nitrate from upland area [kg]. 
  
  # psolyld:   Soluble Phosphorus from upland area [kg]. 
  
  # i_mo:      Month.
  
  
  
  vol <- routingParams$wet_vol          # Volume of water in wetland at begining of day [m3].
  
  wet_fr <- routingParams$wet_fr        # Drainage area fraction [-].
  
  wet_psol <- routingParams$wet_psol    # Available soluble P in wetland [kg].
  
  wet_psed <-routingParams$wet_psed     # Available sediment attached P in wetland [kg].
  
  wet_orgn <- routingParams$wet_orgn    # Available organic N in wetland [kg].
  
  wet_no3 <- routingParams$wet_no3      # Available nitrate in wetland [kg].
  
  wet_no2 <- routingParams$wet_no2      # Available nitrite in wetland [kg].
  
  wet_nh4 <- routingParams$wet_nh4      # Available amonium in wetland [kg].
  
  wet_nvol <- routingParams$wet_nvol    # Wetland normal storage [m3].
  
  wet_nsa <- routingParams$wet_nsa      # Surface area of wetland at normal water level [ha].
  
  wet_mxsa <- routingParams$wet_mxsa    # Surface area of wetland at maximum water level [ha].
  
  wet_mxvol <- routingParams$wet_mxvol  # Volume of water stored in wetlands when filled to maximum water level [m3].
  
  
  organicn <- routingParams$organicn    # Organic N concentration in channel [mg/L].
  
  ammonian <- routingParams$ammonian    # Ammonia concentration in channel [mg/L].
  
  nitriten <- routingParams$nitriten    # Nitrite concentration in channel [mg/L].
  
  nitraten <- routingParams$nitraten    # Nitrate concentration in channel [mg/L].
  
  organicp <- routingParams$organicp    # Sediment attached phosphorus concentration in channel [mg/L].
  
  disolvp <- routingParams$disolvp      # Soluble phosphorus concentration in channel [mg/L].
  
  rchwtr <- routingParams$rchstor       # Water stored in channel at begining of day [m3].
  
  
  psetlw <- c(routingParams$psetlw / 365,routingParams$psetlw / 365)   # Phosphorus setelling rate m/year >> m/day
  
  nsetlw <- c(routingParams$nsetlw / 365,routingParams$nsetlw / 365)   # Nitrogen setelling rate  >> m/year >> m/day
  
  
  ipnd1<-1
  
  ipnd2<-1
  
  
 
  
  # calculate shape parameters for surface area equation:
  
 # wetdif <- 0.
  
 # wetdif <- wet_mxvol - wet_nvol
  
 # if ((wet_mxsa - wet_nsa) > 0. & wetdif > 0.) {
    
   # lnvol <- 0.
    
   # lnvol <- log10(wet_mxvol) - log10(wet_nvol)
    
   # if (lnvol > 1.e-4) {
      
   #   bw2 <- (log10(wet_mxsa) - log10(wet_nsa)) / lnvol
      
   # } else {
      
   #   bw2 <- (log10(wet_mxsa) - log10(wet_nsa)) / 0.001
      
   # }
    
   # if (bw2 > 0.9) {bw2 <- .9}
    
  #  bw1 <- (wet_mxsa / wet_mxvol) ^ bw2
    
 # } else {
    
  #  bw2 <- .9
    
  #  bw1 <- (wet_nsa / wet_nvol) ^ .9
    
  #} 
  
  
  
  
  
  wetsa <- 0. # surface area of wetland on current day [ha]
  
  #wetsa <- bw1 * vol ^ bw2 # [ha]

   wetsa <- wet_nsa  # [ha]
  
  
  # surface flux
  
  wet_orgn_out <- 0             # Organic N exchange between channel and wetland [kg].
  
  wet_no3_out  <- 0             # NO3 exchange between channel and wetland [kg].
  
  wet_psed_out <- 0             # Sediment attached P exchange between channel and wetland [kg].
  
  wet_psol_out <- 0             # Soluble P exchange between channel and wetland [kg].
  
  wet_no2_out  <- 0             # NO2 exchange between channel and wetland [kg].
  
  wet_nh4_out  <- 0             # NH4 exchange between channel and wetland [kg].
  
 
  
  
  if (wet_fr > 0.) {  
    
    
    
    
    # --compute nitrogen and phosphorus levels in wetland at beginning of day ----
    
    # equation 29.1.1 in SWAT manual
    
    
    
    wet_psol <- wet_psol + psolyld 
    
    wet_psed <- wet_psed + psedyld 
    
    wet_orgn <- wet_orgn + orgnyld
    
    wet_no3  <-  wet_no3 + no3yld 


    
    
    
    if(S_wet < 0) {  # surface flux  channel >> wetland
      
      
      orgn_ch <-  (varoute_4 +  organicn * rchwtr / 1000) / (rchwtr + varoute_2)  # [kg/m3]
      
      no3_ch  <- (varoute_6  +  nitraten * rchwtr / 1000) / (rchwtr + varoute_2)
      
      psed_ch <- (varoute_5  +  organicp * rchwtr / 1000) / (rchwtr + varoute_2)
      
      psol_ch <- (varoute_7  +  disolvp * rchwtr / 1000) / (rchwtr + varoute_2)
      
      no2_ch  <- (varoute_15 +  nitriten * rchwtr / 1000) / (rchwtr + varoute_2)
      
      nh4_ch  <- (varoute_14 +  ammonian * rchwtr / 1000) / (rchwtr + varoute_2)
      
      
      wet_orgn_out <- S_wet *  orgn_ch
      
      wet_no3_out  <- S_wet *  no3_ch
      
      wet_psed_out <- S_wet *  psed_ch
      
      wet_psol_out <- S_wet *  psol_ch
      
      wet_no2_out  <- S_wet *  no2_ch
      
      wet_nh4_out  <- S_wet *  nh4_ch
      
      
      
      
      wet_orgn <- wet_orgn  -  wet_orgn_out
      
      wet_no3  <- wet_no3   -  wet_no3_out
      
      wet_psed <- wet_psed  -  wet_psed_out
      
      wet_psol <- wet_psol  -  wet_psol_out
      
      wet_no2  <- wet_no2   -  wet_no2_out
      
      wet_nh4  <- wet_nh4   -  wet_nh4_out 
      
      
      
    }  
    
    
    
    
    # tip: upland area is not source of NO2 and NH4.
    
    
    
    if (wet_voli < 0.001) { 
      
      
      wet_psol <- 0.
      
      wet_psed <- 0.
      
      wet_orgn <- 0.
      
      wet_no3  <- 0.
      
      wet_no2  <- 0.
      
      wet_nh4  <- 0.
      
      
      
      
    } else {
      
      
    
      
      # determine settling rate for nutrients -----------------------------------
      
      
      if (i_mo >= ipnd1 & i_mo <= ipnd2) {
        
        iseas <- 1
        
      } else {
        
        iseas <- 2
        
      }
      
      phosk  <- 0.
      
      nitrok <- 0.
      
      phosk  <- psetlw[iseas] * wetsa * 10000  / wet_vol
      
      phosk  <- min(phosk, 1.)
      
      nitrok <- nsetlw[iseas] * wetsa * 10000  / wet_vol
      
      nitrok <- min(nitrok, 1.)
      
      
      
      # remove nutrients by settling --------------------------------------------
      
      
      wet_psol <- wet_psol * (1. - phosk)
      
      wet_psed <- wet_psed * (1. - phosk)
      
      wet_orgn <- wet_orgn * (1. - nitrok)
      
      wet_no3  <- wet_no3  * (1. - nitrok)
      
      wet_no2  <- wet_no2  * (1. - nitrok)
      
      wet_nh4  <- wet_nh4  * (1. - nitrok)
      
      
      
      
      if (wet_orgn < 1.e-6) {wet_orgn <- 0.0}
      
      if (wet_no3  < 1.e-6) {wet_no3  <- 0.0}
      
      if (wet_psed < 1.e-6) {wet_psed <- 0.0}
      
      if (wet_psol < 1.e-6) {wet_psol <- 0.0}
      
      if (wet_no2  < 1.e-6) {wet_no2  <- 0.0}
      
      if (wet_nh4  < 1.e-6) {wet_nh4  <- 0.0}
	  
	  
	  
	  # update nutrient pools in wetlands ---------------------------------------
    
    if(S_wet > 0){ # surface flux wetland >> channel
      
      
      yy <- S_wet / (wet_vol + S_wet)
      
      
      wet_orgn_out <- wet_orgn *  yy  
      
      wet_no3_out  <- wet_no3  *  yy   
      
      wet_psed_out <- wet_psed *  yy
      
      wet_psol_out <- wet_psol *  yy
      
      wet_no2_out  <- wet_no2  *  yy
      
      wet_nh4_out  <- wet_nh4  *  yy
      
      
      wet_orgn <- wet_orgn * (1. - yy)  # remaining organic N in wetland [kg]
      
      wet_no3  <- wet_no3  * (1. - yy)    # remaining NO3 in wetland [kg]
      
      wet_psed <- wet_psed * (1. - yy)  # remaining sediment P in wetland [kg]
      
      wet_psol <- wet_psol * (1. - yy)  # remaining soluble P in wetland [kg]
      
      wet_no2  <- wet_no2  * (1. - yy)   # remaining NO2 in wetland [kg]
      
      wet_nh4  <- wet_nh4  * (1. - yy)   # remaining NH4 in wetland [kg]
      
      
    }  
	  
      
    }
    
    
  }  
  
 
  
  output <- c(wet_orgn,wet_no3,wet_psed,wet_psol,wet_no2,wet_nh4,
            
            wet_orgn_out,wet_no3_out,wet_psed_out, wet_psol_out, wet_no2_out,
            
            wet_nh4_out)
  
  
  
  
  return(output)
  
  
  
} # function
