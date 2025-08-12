################################################################################
# This function calculates water exchange between riparian wetland and channel

# based on Liu algorithm. 

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

# References: 

# SWAT source codes - https://swat.tamu.edu/software/swat/

# SWAT 2009 theory

# Liu, Y., Yang, W., & Wang, X. (2008). Development of a SWAT extension module ...

# to simulate riparian wetland hydrologic processes at a watershed scale. ...

# Hydrological Processes: An International Journal, 22(16), 2901-2915.

################################################################################  

source(".../functions/Wetland_rip_wat.R")  # call wetland water routing  
     
source(".../functions/VS.R") # call Variable Storage Routing or Muskingum METHOD


Qman<- function(x1,x2,x3,x4){ # internal function
    
    
    # convert to discharge
    
    # x1: area
    
    # x2: hydraulic radius 
    
    # x3: Manning roughness
    
    # x4: slope
    
    r_qman <- 0.
    
    r_qman <- x1 * x2 ^ 0.6666 * sqrt(x4) / x3
    
    return(r_qman)
    
  }


Liu <- function(varoute_2,pet_day,flwin,flwout,watyld,routingParams,day){
  
  
  
  
  # varoute_2: Water enters channel from upstream watershed [m3].
  
  # pet_day: Potential evaporation [mm]. 
  
  # watyld: Water enters wetland from upland area [m3].
  
  # S_wet: Water exchange between wetland and channel [m3].
 
 
  #----- parameters------------------------------------------------
   
  
  wet_nvol <- routingParams$wet_nvol         # Wetland normal volume [m3].
  
  wet_nsa <- routingParams$wet_nsa           # Surface area of wetlands at normal water level [ha].
  
  wet_mxsa <- routingParams$wet_mxsa         # Surface area of wetlands at maximum water level [ha].
  
  wet_mxvol <- routingParams$wet_mxvol       # Volume of water stored in wetlands when filled to maximum water level [m3].
  
  ch_d<-routingParams$ch_d                   # Channel bankfull depth [m].
  
  ch_l2<-routingParams$ch_l2                 # Length of channel [km].
  
  phi_1<-routingParams$phi_1                 # Bankfull channel area [m2].
  
  phi_6 <- routingParams$phi_6               # Channel bed width [m2].
  
  ch_n2 <- routingParams$ch_n2               # Channel Manning roughness.
  
  ch_s2 <- routingParams$ch_s2               # Channel slope [-].

  eps <- routingParams$eps                   # Acceptable error in convergence [m].

  chside <- routingParams$chside             # Channel side slope [-].
    
  
  wet_nsa <-  wet_nsa * 10000 # covert to m2
  
  wet_mxsa <- wet_mxsa * 10000 # convert to m2
  
  
  
  # calculate shape parameters for surface area equation
  
 # wetdif <- 0.
  
 # wetdif <- wet_mxvol - wet_nvol
  
 # if ((wet_mxsa - wet_nsa) > 0. & wetdif > 0.) {
  
 # lnvol <- 0.
  
 # lnvol <- log10(wet_mxvol) - log10(wet_nvol)
  
#  if (lnvol > 1.e-4) {
  
 # bw2 <- (log10(wet_mxsa) - log10(wet_nsa)) / lnvol
  
 # } else {
    
  #  bw2 <- (log10(wet_mxsa) - log10(wet_nsa)) / 0.001
  
 # }
  
 # if (bw2 > 0.9) {bw2 <- .9}
  
 # bw1 <- (wet_mxsa / wet_mxvol) ^ bw2
  
 # } else {
    
  #  bw2 <- .9
  
  #bw1 <- (wet_nsa / wet_nvol) ^ .9
  
 # } 
  
  
 
  
  #wet_d <- bw1 ^ (-1) * wet_nvol ^ (1-bw2)         # water depth in wetland at normal storage level [m]
  
  wet_d <- wet_nvol / wet_nsa
  
  
    c <- 0.
    
    c <- chside
    
    p <- phi_6 + 2. * ch_d * sqrt(1. + c * c)
    
    rh <- phi_1 / p
    
    maxrt <- Qman(phi_1, rh, ch_n2, ch_s2)        # Maximum flow capacity of the channel at bank full [m3/s]
	
	
	
   vol_bf <- maxrt * 86400                        # Bankfull volume [m3].



   S_wet <- 0                                     # Water flux between wetland and channel [m3]

    
   wetout<- Wetland_rip_wat(watyld=watyld, S_wet=S_wet, routingParams=routingParams) 
   
   
   rchout <- VS(varoute_2=varoute_2,pet_day=pet_day,day=day,flwin=flwin,
                       
                       flwout=flwout,S_wet=S_wet,routingParams=routingParams)
					   
							   
  
   v_ch <- rchout[5] * 86400 

   
   v_wet <- wetout[1]                            # water stored in wetland [m3].
   
   
   wetout0 <- wetout
   
   
   rchout0 <- rchout
   
   

   
   if(wetout[2] <= wet_d &  rchout[7] <= ch_d){  # Condition 0: no flux between wetland and channel.
     
     liucon <- 0
     
     S_wet <- 0
	 
	 
     return(c(rchout0,wetout0,S_wet))
     
	 

   }else{
     
     
     if(wetout[2] > wet_d &  rchout[7] <= ch_d) { # condition 1: wetland >> channel
	 
       
       
      S_wet <- v_wet - wet_nvol
       
       
      wetout <- Wetland_rip_wat(watyld=watyld, S_wet=S_wet, routingParams=routingParams) 
       
       
      rchout <- VS(varoute_2=varoute_2,pet_day=pet_day,day=day,flwin=flwin,
                       
                       flwout=flwout,S_wet=S_wet,routingParams=routingParams)
       
       
       if(rchout[7] <= ch_d){
         
              
         output<-c(rchout,wetout,S_wet)
         
         return(output)
         
         
       }else{
	  
						   
						   
    H_wet <- wetout0[2] - wet_d	                   # Water level in wetland [m].
       
    H_ch  <- rchout0[7] - ch_d	                   # Water level in channel [m].
						   
						   
	
	range <- c(0, (v_wet - wet_nvol))
	
		
					   
	 
	 # bisection algorithm
	 
	 
	 
	 while(abs(H_wet - H_ch) > eps){
	 
	 
	 
	 c <- (range[1] + range [2]) / 2
	 
	 
	 S_wet <- c
	 
	 
	wetout<- Wetland_rip_wat(watyld=watyld, S_wet=S_wet, routingParams=routingParams) 
       
       
       rchout <- VS(varoute_2=varoute_2,pet_day=pet_day,day=day,flwin=flwin,
                       
                       flwout=flwout,S_wet=S_wet, routingParams=routingParams)
						   
						   
     H_wet <- wetout[2] - wet_d	 
       
     H_ch  <- rchout[7] - ch_d


# determine sign of midpoint	
	
	
	
	if(H_wet > H_ch){
	
	 range[1] <- c
	
	}else{
	
	range[2] <- c
	
	}
	
	
	if(abs(range[1]-range[2]) < 0.01) {
	
	# print('Warning!')
	
	break
	
	
	}
	
	 
	 } # while
	 
	
	 
	 
	 output<-c(rchout,wetout,S_wet)
         
         
         return(output)
	   
	
	  
	   }
       
       
       
       }  # condition 1
     
     
     if(wetout[2] <= wet_d &  rchout[7] > ch_d) { # condition 2: channel >> wetland
	 
	       
       S_wet <- vol_bf - v_ch 

       if(S_wet > 0) {S_wet <- 0}
	   
	   
       
       wetout<- Wetland_rip_wat(watyld=watyld, S_wet=S_wet, routingParams=routingParams) 
       
       
       rchout <- VS(varoute_2=varoute_2,pet_day=pet_day,day=day,flwin=flwin,
                       
                       flwout=flwout,S_wet=S_wet,routingParams=routingParams)
       
       
       if(wetout[2] <= wet_d ){
         
         
         output<-c(rchout,wetout,S_wet)
         
         
         return(output)
         
         
       }else{
	   
						   
						   
    H_wet <- wetout0[2] - wet_d	 
       
    H_ch  <-  rchout0[7] - ch_d	   
						   
						  

		
	range <- c((vol_bf - v_ch), 0)

	if(range[1] > 0){range[1]<-0}	
		
	 
	 # bisection algorithm
	 
	 
	 
	 while(abs(H_wet - H_ch) > eps){
	 
	 
	 c <- (range[1] + range [2]) / 2
	 

	 S_wet <- c
	 
	 
	   wetout<- Wetland_rip_wat(watyld=watyld, S_wet=S_wet, routingParams=routingParams) 
       
       
       rchout <- VS(varoute_2=varoute_2,pet_day=pet_day,day=day,flwin=flwin,
                       
                       flwout=flwout,S_wet=S_wet,routingParams=routingParams)
						   
						   
    H_wet <- wetout[2] - wet_d	 
       
    H_ch  <- rchout[7] - ch_d


# determine sign of midpoint	
	
	
	
	if(H_wet > H_ch){
	
	 range[1] <- c
	
	}else{
	
	range[2] <- c
	
	}
	
	
	if(abs(range[1]-range[2]) < 0.01 ) {
	
	# print('Warning!')
	
	break
	
	
	}
	
	 
	 } # while
	 
	
	 
	 
	 output<-c(rchout,wetout,S_wet)
         
         
         return(output)
	   
	   
	   }
       
       
       }  # condition 2
     
     if(wetout[2] > wet_d & rchout[7] > ch_d) { # condition 3

		   
    H_wet <- wetout0[2] - wet_d	 
       
    H_ch <- rchout0[7] - ch_d   
						   
						   

	if(H_wet > H_ch) {

	range <- c(0, (v_wet - wet_nvol))
	
	}else{
	
	range <- c((vol_bf - v_ch), 0)

        if(range[1] > 0){range[1] <- 0}

	}
				   
	 
	 # bisection algorithm
	 
	 
	 
	 while(abs(H_wet - H_ch) > eps){
	 
	 
	 
	 
	 c <- (range[1] + range [2]) / 2
	 
	
	 S_wet <- c
	 
	 
	 wetout<- Wetland_rip_wat(watyld=watyld, S_wet=S_wet, routingParams=routingParams) 
       
       
      rchout <- VS(varoute_2=varoute_2,pet_day=pet_day,day=day,flwin=flwin,
                       
                       flwout=flwout,S_wet=S_wet,routingParams=routingParams)
						   
						   
    H_wet <- wetout[2] - wet_d	 
       
    H_ch <- rchout[7] - ch_d


# determine sign of midpoint	
	
	
	
	if(H_wet > H_ch){
	
	 range[1] <- c
	
	}else{
	
	range[2] <- c
	
	}
	
	
	if(abs(range[1]-range[2]) <0.01) {
	
	# print('Warning!')
	
	break
	
	
	}
	
	 
	 } # while
	 
 
	 output<-c(rchout,wetout,S_wet)
         
         
         return(output)

	
	 } # end of condition 3
     
     
     
   }
   

   
   
} # end of function

    
    
    
    
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
  
  
  
  
  
  
  

