################################################################################

# Routing sediment through riparian wetland

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

# References: 

# 1. SWAT source codes

# 2. SWAT 2009 theory


################################################################################


Wetland_rip_sed<- function(wet_vol,wet_voli, S_wet, sedyld, sanyld, silyld,
                           
                           clayld,sagyld, lagyld,varoute_2, varoute_3,
                           
                            routingParams){ 
  
  
  
  
  # wet_vol: Water stored in the wetland at end of day [m3].
  
  # wet_voli: Water volume before release [m3].
  
  # S_wet: Water flux between wetland and channel [m3].
  
  # sedyld: Sediment from upland area [ton].
  
  # sanyld: Sand from upland area [ton].
  
  # silyld: Silt from upland area [ton].
  
  # clayld: Clay from upland area [ton].
  
  # sagyld: Small aggregates from upland area [ton].
  
  # lagyld: Large aggregates from upland area [ton].
  
  # varoute_2: Water from upstream watershed to channel [m3].
  
  # varoute_3: Sediment from upstream watershed to channel [ton].
  
  # routingParams: List of parameters for routing.
  
  
  
  wet_fr <- routingParams$wet_fr        # Drainage area fraction [0-1].
  
  vol <- routingParams$wet_vol          # Water stored in the wetland at begining of day [m3].
  
  wet_sed <- routingParams$wet_sed      # Sediment concentration in wetland [ton/m3].
  
  wet_san <- routingParams$wet_san      # Sand concentration in wetland [ton/m3].
  
  wet_sil <- routingParams$wet_sil      # Silt concentration in wetland [ton/m3].
  
  wet_cla <- routingParams$wet_cla      # Clay concentration in wetland [ton/m3].
  
  wet_sag <- routingParams$wet_sag      # Small aggregates concentration in wetland [ton/m3].
  
  wet_lag <- routingParams$wet_lag      # Large aggregates concentration in wetland [ton/m3].
  
  wet_nsed <- routingParams$wet_nsed    # Normal sediment concentration in wetland [mg/l].
  
  wet_nsed <- wet_nsed * 1e-6           # Convert to [ton/m3].
  
  sed_stl <- routingParams$sed_stl      # Fraction of sediment remaining suspended after settling for one day [-].
  
  rchwtr <- routingParams$rchstor       # Water stored in channel at begining of day [m3].
  
  sedst <-  routingParams$sedst         # Sediment stored in channel at begining of day [m3].
  
  sed_ch <- 0                           # Sediment concentration in channel at begining of day [ton/m3].
  
  wetsedo <- 0                          # Sediment exchange between wetland and channel [ton]. 
  
  wetsano <- 0                          # Sand exchange between wetland and channel [ton].
  
  wetsilo <- 0                          # Silt exchange between wetland and channel [ton].
  
  wetclao <- 0                          # Clay exchange between wetland and channel [ton].
  
  wetsago <- 0                          # Samall aggregates exchange between wetland and channel [ton]. 
  
  wetlago <- 0                          # Large aggregates exchange between wetland and channel [ton].
  
  
  if (wet_fr > 0.) {  
    
    # store initial values ----------------------------------------------------
    
    
    sed <- 0.  
    
    san <- 0.
    
    sil <- 0.
    
    cla <- 0.
    
    sag <- 0.
    
    lag <- 0.
    
    inised <- 0.
    
    finsed <- 0.
    
    setsed <- 0.
    
    remsetsed <- 0.
    
    
    
    sed <- wet_sed  
    
    san <- wet_san
    
    sil <- wet_sil
    
    cla <- wet_cla
    
    sag <- wet_sag
    
    lag <- wet_lag
    
    
    
    wetsedi <- sedyld
    
    wetsani <- sanyld 
    
    wetsili <- silyld 
    
    wetclai <- clayld 
    
    wetsagi <- sagyld 
    
    wetlagi <- lagyld 
    
    
    
    if (wet_voli < 0.001) { # r2
      
      
      wet_sed <- 0.
      
      wet_san <- 0.
      
      wet_sil <- 0.
      
      wet_cla <- 0.
      
      wet_sag <- 0.
      
      wet_lag <- 0.
      
      
      
      
    } else {
	
	
	
	if(S_wet < 0) {  # channel >> wetland
        
        
        sed_ch <- (varoute_3 + sedst) / (rchwtr + varoute_2) # sediment concentration in channel at begining of day [ton/m3].
        
        
        wetsedo <- sed_ch  * S_wet  # Sediment exchange [ton].
        
        wetsano <- 0
        
        wetsilo <- 0
        
        wetclao <- 0
        
        wetsago <- 0
        
        wetlago <- 0
		
		
		}
      
      
      
      #  compute new sediment concentration -----------------------------------
      
      wet_sed <- (sed * vol + wetsedi - wetsedo) / wet_voli   # Sediment concentration at end of day [ton/m3].
      
      wet_san <- (san * vol + wetsani - wetsano) / wet_voli
      
      wet_sil <- (sil * vol + wetsili - wetsilo) / wet_voli
      
      wet_cla <- (cla * vol + wetclai - wetclao) / wet_voli
      
      wet_sag <- (sag * vol + wetsagi - wetclao) / wet_voli
      
      wet_lag <- (lag * vol + wetlagi - wetlago) / wet_voli
     
      
      
      # compute sediment settling -----------------------------------------------
      
      
      if (sed_stl < 1.e-6) {sed_stl <- 0.0}
      
      # sed_stl: fraction of sediment remaining suspended in impoundment after 
      
      # settling for one day (kg/kg)
      
      inised <- wet_sed 
      
      if (wet_sed > wet_nsed) { 
        
        wet_sed <- (wet_sed - wet_nsed) * sed_stl + wet_nsed
        
        
        
      } 
      
      finsed <- wet_sed
      
      setsed <- inised - finsed
      
      setsed <- max(0.,setsed)
      
      
      if (wet_lag >= setsed) {
        
        wet_lag <- wet_lag - setsed
        
        remsetsed <- 0.
        
      } else { #1
        
        remsetsed <- setsed - wet_lag
        
        wet_lag <- 0.
        
        if (wet_san >= remsetsed) {
          
          wet_san <- wet_san - remsetsed
          
          remsetsed <- 0.
          
        } else { #2
          
          remsetsed <- remsetsed - wet_san
          
          wet_san <- 0.
          
          if (wet_sag >= remsetsed) {
            
            wet_sag <- wet_sag - remsetsed
            
            remsetsed <- 0.
            
            
          } else{  #3
            
            remsetsed <- remsetsed - wet_sag
            
            wet_sag <- 0.
            
            if (wet_sil >= remsetsed) {
              
              wet_sil <- wet_sil - remsetsed
              
              remsetsed <- 0.
              
              
            } else { #4
              
              
              remsetsed <- remsetsed - wet_sil
              
              wet_sil <- 0.
              
              if (wet_cla >= remsetsed) {
                
                wet_cla <- wet_cla - remsetsed
                
                remsetsed = 0.
                
              } else { #5
                
                remsetsed <- remsetsed - wet_cla
                wet_cla <- 0.
                
                
              } #5
              
              
              
            } #4
            
            
            
          } #3
          
          
          
        } #2
        
        
        
      } #1
      
      
	   # compute sediment leaving wetland ----------------------------------------
    
    
    if(S_wet > 0) {  # wetland >> channel
      
      
      
      wetsedo <- wet_sed * S_wet     # sediment exchange [ton].
      
      wetsano <- wet_san * S_wet
      
      wetsilo <- wet_sil * S_wet
      
      wetclao <- wet_cla * S_wet
      
      wetsago <- wet_sag * S_wet
      
      wetlago <- wet_lag * S_wet
      
      
    } 
	  
	  
	  # Sediment concentration is insignificant
    
    
    if (wet_sed < 1e-6 ) {wet_sed <-0} 
    
    if (wet_san < 1e-6) {wet_san <-0}
    
    if (wet_sil < 1e-6) {wet_sil <-0}
    
    if (wet_cla < 1e-6) {wet_cla <-0}
    
    if (wet_sag < 1e-6) {wet_sag <-0}
    
    if (wet_lag < 1e-6) {wet_lag <-0}
	 
	  
      
    }
     
    
    
  }  
  
  
  
  
  output <- c(wet_sed,wet_san, wet_sil, wet_cla, wet_sag, wet_lag, wetsedo,
            
            wetsano, wetsilo, wetclao, wetsago, wetlago)
  
  
  
  
  return(output)
  
  
  
} # function
