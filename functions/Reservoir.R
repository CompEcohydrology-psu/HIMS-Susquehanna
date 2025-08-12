################################################################################

# Flow and Sediment Routing in Controlled and Uncontrolled Reservoirs 

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

# References: 

# SWAT source code

# SWAT 2009 theory

# Turner, S. W., Doering, K., & Voisin, N. (2020). Data-driven reservoir ...

# simulation in a large-scale hydrological and water resource model... 

# Water Resources Research, 56(10), e2020WR027902.

# Turner, S. W., Steyaert, J. C., Condon, L., & Voisin, N. (2021). Water ...

# storage and release policies for all large reservoirs of conterminous ... 

# United States. Journal of Hydrology, 603, 126843.

################################################################################


Res<-function(varoute_2,pet_day,percp_day,varoute_3,varoute_23,varoute_24,
              
              varoute_25,varoute_26,varoute_27,varoute_28,routingParams, week){
			  
			  
			 
  
  
  # varoute_2: inflow to reservoir [m3]
  
  # pet_day: potential evaporation [mm] 
  
  # percp_day: precipitation [mm]
  
  # varoute_3: sediment inflow to reservoir [ton]
  
  # routingParams: routing parameters
  
  # week: week of operation within hydrological/water year oct >> sep
  
  # control:  uncontrolled spillway [0] and controlled spillway [1]
  
  
  
# read parameters:
  

  res_evol <- routingParams$res_evol
  
  res_pvol <- routingParams$res_pvol
  
  res_esa <- routingParams$res_esa
  
  res_psa <- routingParams$res_psa
  
  res_rr <- routingParams$res_rr
  
  res_nsed <- routingParams$res_nsed  * 1.e-6             # mg/L => ton/m^3
  
  res_vol <-routingParams$res_vol
  
  evrsv <- routingParams$evrsv
  
  res_k <- routingParams$res_k
  
  res_sed <- routingParams$res_sed
  
  res_san <- routingParams$res_san
  
  res_sil <- routingParams$res_sil
  
  res_cla <- routingParams$res_cla
  
  res_sag <- routingParams$res_sag
  
  res_lag <- routingParams$res_lag
  
  res_gra <- routingParams$res_gra

  br1<- routingParams$br1
 
  br2<- routingParams$br2
  
  ndtargr <- routingParams$ndtargr
  
  sed_stlr <- routingParams$sed_stlr
  
  res_d50 <- routingParams$res_d50


 control <- routingParams$control
  
  
  if(control==1){  # controlled spillway parameters
  
  
  hi_mu <- routingParams$hi_mu
    
  hi_alpha <- routingParams$hi_alpha
    
  hi_beta <- routingParams$hi_beta
    
  hi_min <- routingParams$hi_min
    
  hi_max <- routingParams$hi_max
    
    
  lo_mu <- routingParams$lo_mu
    
  lo_alpha <- routingParams$lo_alpha
    
  lo_beta <- routingParams$lo_beta
    
  lo_min <- routingParams$lo_min
    
  lo_max <- routingParams$lo_max
  
  Release_alpha1 <- routingParams$Release_alpha1
  
  Release_alpha2 <- routingParams$Release_alpha2
  
  Release_beta1 <- routingParams$Release_beta1
  
  Release_beta2 <- routingParams$Release_beta2
  
  Release_c <- routingParams$Release_c
  
  Release_max <- routingParams$Release_max
  
  Release_min <- routingParams$Release_min
  
  Release_p1 <- routingParams$Release_p1
  
  Release_p2 <- routingParams$Release_p2
  
  I_bar <- routingParams$I_bar
  
    
  t <- week
  
  w <- 1/52  # frequency of operation
  
  
  if(I_bar <= 0) {control <-0 }  # no flow 
  
  
  }
  
  
  
  


  res_d50mm <- res_d50 / 1000.             # micrometers to millimeters
     
     
  velsetlr <- 24. * 411. * res_d50mm ^ 2.  # fall velocity of sediment
  
  sed_stlr <- exp(-.184 * res_d50)

  
  # incoming sediments:
  
  ressedi <- varoute_3
    
  ressani <- varoute_23
  
  ressili <- varoute_24
  
  resclai <- varoute_25
    
  ressagi <- varoute_26
  
  reslagi <- varoute_27
    
  resgrai <- varoute_28
    
  
  
  # store initial values
  
  # vol  volume of water stored in reservoir at beginning of day [m3]
  
  # sed  concentration of sediment in reservoir at beginning of day (kg/L)
  
  # or [ton/m3]
  
  vol <- 0
  
  sed <- 0
  
  inised <- 0.
  
  finsed <- 0.
  
  setsed <- 0.
  
  remsetsed <- 0.
  
  trapres <- 0.
  
  vol <- res_vol  # res_vol: reservoir volume [m3]
  
  sed <- res_sed  # res_sed: amount of sediment in reservoir
  
 
  
  #    Storage by particle sizes
  
  san <- res_san
  
  sil <- res_sil
  
  cla <- res_cla
  
  sag <- res_sag
  
  lag <- res_lag
  
  gra <- res_gra
  
  
  
  # calculate surface area for day
  
  ressa <- br1 * res_vol ^ br2
  
  
  # calculate water balance for day

  resev <- 0
  
  #resev <- 10. * evrsv * pet_day * ressa  # evaporation from reservoir on 
  
  #day [m3]
  
  # lake evaporation coefficient

   ressep <- 0
  
  #ressep <- res_k * ressa * 240. # seepage from reservoir on day [m3]
  
  # res_k: hydraulic conductivity of the reservoir bottom [mm/hr]

  respcp <- 0
  
 # respcp <- percp_day * ressa * 10. # precipitation on reservoir 
  
 
  
  # for day [m3]
  
  # sub_subp: precipitation for day in subbasin [mm]
  
  
  resflwi <- varoute_2   #water entering reservoir on day [m3]
  
 
 
  
  # new water volume for day
  
 
  
  res_vol <- res_vol + respcp + resflwi - resev - ressep
  
  
   
  
  
  
  
 # if reservoir volume is greater than zero
  
  # determine reservoir outflow 
  
  # Uncontrolled reservoir
  
 
  vvr <- 0 
  
  resflwo <- 0
  

 # uncontrolled spillway
  
  if(control==0){
  
  vvr <- 0.
  
  if (res_vol > res_pvol & res_vol <=res_evol) {
  
  vvr <- res_vol - res_pvol
  
  if (res_rr > vvr ) {   # res_rr=0: missing information in NID!!!
  
  resflwo <- resflwo + vvr
  
  }else{
    
    resflwo <- resflwo + res_rr
	
	if(res_rr==0) {resflwo <- resflwo + vvr}
  
  }
  }
  
  
  

if(res_vol > res_evol){

vvr <- res_evol - res_pvol

if (res_rr > vvr ) {   # res_rr=0: missing information in NID!!!
  
  resflwo <- (res_vol -res_evol) + vvr
  
  }else{
    
    resflwo <- (res_vol -res_evol) + res_rr
	
	if(res_rr==0) {resflwo <- (res_vol -res_evol) + vvr}
  
  }

}
  
}






#------------- Controlled Reservoirs -----------
  
  if(control==1){
    
    I <- resflwi * 7  # weekly flow
  
  
hi_hmc <-  hi_mu + hi_alpha * sin(2 * pi * w * t) + hi_beta * cos(2 * pi * w * t)
  
  
 if ( hi_min <= hi_hmc & hi_hmc <= hi_max ) {hi_S <- hi_hmc }
  
 if ( hi_hmc <= hi_min ) {hi_S <- hi_min}
  
 if ( hi_hmc >= hi_max) {hi_S <- hi_max}
  
  
  hi_S <- as.numeric(hi_S)
  
  
lo_hmc <-  lo_mu + lo_alpha * sin(2 * pi * w * t) + lo_beta * cos(2 * pi * w * t)


if ( lo_min <= lo_hmc & lo_hmc <= lo_max ) {lo_S <- lo_hmc }

if ( lo_hmc <= lo_min ) {lo_S <- lo_min}

if ( lo_hmc >= lo_max) {lo_S <- lo_max}


lo_S <- as.numeric(lo_S)


R_hat <- Release_alpha1 * sin(2 * pi * w * t) + Release_alpha2 * sin(4 * pi * w * t) +
  
  Release_beta1 * cos(2 * pi * w * t) + Release_beta2 * cos(4 * pi * w * t) 


S <- (vol / res_evol) * 100  




#Release_max <- I_bar * Release_max +  I_bar

#Release_min <- I_bar * Release_min +  I_bar


if( S >= lo_S & S <= hi_S ) {
  
  A <- (S - lo_S) / (hi_S-lo_S)  # fractional position of storage within normal range [0-1]
 
  I_hat <- (I - I_bar) / I_bar
  
  epsilon <- Release_c + Release_p1 * A + Release_p2 * I_hat
  
  
  
  R_dot <- I_bar * (R_hat+epsilon) + I_bar
  
 # if(R_dot > Release_max) {R_dot <- Release_max}
 
 
  
  }

if(S > hi_S) {
  
  R_dot <- res_evol * ((S - hi_S)/100) + I
  

  
 # if(R_dot >Release_max) {R_dot <- Release_max}
  
  
}

if(S < lo_S) {
  
  R_dot <- Release_min 
  
  
}


c1 <- min(R_dot,I+vol)

c2 <- I + vol - res_evol

resflwo <- max(c1, c2)

resflwo <- resflwo / 7  # convert weekly release to daily


if(res_rr > 0){

Release_max <- res_rr

if(resflwo > Release_max) {resflwo <- Release_max}

}


}


  
  # if reservoir volume is zero
  
  if (res_vol < 0.001) {
  
  # if volume deficit in reservoir exists, reduce seepage so
    
  # that reservoir volume is zero
    
  ressep <- ressep + res_vol
  
  res_vol <- 0.
  
  # if seepage is less than volume deficit, take remainder
  
  # from evaporation
  
  if (ressep < 0.) {
    
  resev <- resev + ressep
  
  ressep <- 0.
  
  }
  
  res_sed <- 0.
  
  # added by my own
  
  res_sil <- 0.
  
  res_cla <- 0.
  
  res_sag <- 0.
  
  res_lag <- 0.
  
  res_gra <- 0.
  
  ressedo <-0.
  
  ressano <-0.
  
  ressilo <-0.
  
  resclao <- 0.
  
  ressago <- 0.
  
  reslago <- 0.
  
  resgrao <- 0.
  
  

  
  }else{  #1
      
    
    # subtract outflow from reservoir storage
	
	
	# res_voli <- res_vol
    
    
    res_vol <- res_vol - resflwo
    
    if (res_vol < 0.) {
    
    resflwo <- resflwo + res_vol
    
    res_vol <- 0.
    
    }
 
    
    
    
    # compute new sediment concentration in reservoir
    
    
    if (ressedi < 1.e-6) {ressedi <- 0.0}      
    
    
    if (ressa == 0.) {ressa <- 1.e-6}    
    
    
    velofl <- (resflwo / ressa) / 10000.  # m3/d / ha * 10000. = m/d
    
    
    
    #	  velsetl = 1.35      !! for clay particle m/d
    
    if (velofl > 1.e-6) {
    
    trapres <- velsetlr / velofl
    
    if (trapres > 1.) {trapres <- 1.}  
    
    susp <- 1. - trapres
    
    }else{
      
      susp <- 1.
    
    }
    
    if (res_vol > 0.) {              
      
    res_sed <- (ressedi * susp + sed * vol) / res_vol # amount of sediment in
    
    # reservoir [ton/m3]
    
    # ressedi: sediment entering reservoir during time step
    
    res_san <- (ressani + san * vol) / res_vol
    
    res_sil <- (ressili + sil * vol) / res_vol
    
    res_cla <- (resclai + cla * vol) / res_vol
    
    res_sag <- (ressagi + sag * vol) / res_vol
    
    res_lag <- (reslagi + lag * vol) / res_vol
    
    res_gra <- (resgrai + gra * vol) / res_vol
    
    res_sed <- max(1.e-6,res_sed)
    
    res_san <- max(1.e-6,res_san)
    
    res_sil <- max(1.e-6,res_sil)
    
    res_cla <- max(1.e-6,res_cla)
    
    res_sag <- max(1.e-6,res_sag)
    
    res_lag <- max(1.e-6,res_lag)
    
    res_gra <- max(1.e-6,res_gra)
    
    }else{
      
    res_sed <- 1.e-6   
      
    res_san <- 1.e-6 
    
    res_sil <- 1.e-6 
    
    res_cla <- 1.e-6 
    
    res_sag <- 1.e-6 
    
    res_lag <- 1.e-6 
    
    res_gra <- 1.e-6 
    
    }
    
    
   
    
    # compute change in sediment concentration due to settling
    
    if (res_sed < 1.e-6) {res_sed <- 0.0}    
    
    if (res_sed > res_nsed) { #2
    
    inised <- res_sed
    
    res_sed <- (res_sed - res_nsed) * sed_stlr + res_nsed
    
    finsed <- res_sed
    
    setsed <- inised - finsed
    
    if (res_gra >= setsed) {
    
    res_gra <- res_gra - setsed
    
    remsetsed <- 0.
    
    }else{
      
      remsetsed <- setsed - res_gra
      
    res_gra <- 0.
    
    if (res_lag >= remsetsed) {
      
    res_lag <- res_lag - remsetsed
    
    remsetsed <- 0.
    
    }else{
      
      remsetsed <- remsetsed - res_lag
      
    res_lag <- 0.
    
    if (res_san >= remsetsed) {
    
    res_san <- res_san - remsetsed
    
    remsetsed <- 0.
    
    }else{
      
      remsetsed <- remsetsed - res_san
      
    res_san <- 0.
    
    if (res_sag >= remsetsed) {
    
    res_sag <- res_sag - remsetsed
    
    remsetsed <- 0.
    
    }else{
      
      remsetsed <- remsetsed - res_sag
      
    res_sag <- 0.
    
    if (res_sil >= remsetsed) {
    
    res_sil <- res_sil - remsetsed
    
    remsetsed <- 0.
    
    }else{
      
      remsetsed <- remsetsed - res_sil
      
    res_sil <- 0.
    
    if (res_cla >= remsetsed) {
      
    res_cla <- res_cla - remsetsed
    
    remsetsed <- 0.
    
    }else{
      
      remsetsed <- remsetsed - res_cla
      
    res_cla <- 0.
    
    }
    }
    }
    }
    }
    }
    
    } #2
    
    # compute sediment leaving reservoir
    
    ressedo <- res_sed * resflwo   # [ton]
    
    ressano <- res_san * resflwo
    
    ressilo <- res_sil * resflwo
    
    resclao <- res_cla * resflwo
    
    ressago <- res_sag * resflwo
    
    reslago <- res_lag * resflwo
    
    resgrao <- res_gra * resflwo
    
    # net change in amount of sediment in reservoir for day
    
    ressedc <- vol * sed + ressedi - ressedo - res_sed * res_vol
  
    
    
  }  #1
  
 
  #update surface area for day
  
  ressa <- br1 * res_vol ^ br2
  
  
  
  output <- c(resflwo,res_vol,ressa,res_sed,res_san,res_sil,
              
              res_cla,res_sag,res_lag,res_gra,
              
              ressedo,ressano,ressilo,resclao,ressago,reslago, resgrao,
              
              varoute_2,resev,respcp, ressedi,ressani,ressili,resclai,
              
              ressagi, reslagi,  resgrai)
  
  
  return(output)
  
  
  #resflwo: water leaving reservoir on day [m3]
  
  #res_sed: sediment concentration in reservoir [mg/L]
  
  # ressedo: sediment realease in ton
  
} # end of function



