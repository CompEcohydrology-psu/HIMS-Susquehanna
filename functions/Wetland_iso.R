################################################################################

# Routing flow through non-riparian wetland (isolated) wetland

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

# References: 

# SWAT source codes

# SWAT 2009 theory

################################################################################


Wetland_iso<- function(watyld, sedyld, sanyld, silyld, clayld,
                   
                  sagyld,lagyld, psolyld, psedyld, orgnyld, no3yld, param, i_mo){ # function
  
  
  # param: parameters
  
  # wet_fr: fraction of flow goes to wetland [0-1]
  
  # wetsep: seepage from wetland or recharge from HRUs at the place of wetland to groundwater [m3] [hru no 10]
  
  # watyld: water contributing to channel from subbasin (NHD catchment) [m3] = hydrology.mat[1]
  
  # sedyld: sediment contributing to channel from subbasin (NHD catchment) [ton] = sediment.mat[1]
  
  # orgnyld: organic Nitrogen [kg] = contaminant.mat[1]
  
  # psedyld: sediment Phosphorous [kg] = contaminant.mat[2]
  
  # no3yld: no3 [kg] = contaminant.mat[3]
  
  # psolyld: soluble Phosphorus [kg] = contaminant.mat[4]
  
  # i_mo: Month
  
  
  
  wet_fr    <- param$wet_fr
  
  wet_vol   <- param$wet_vol
  
  wet_sed   <- param$wet_sed      # sediment concentration in wetland
  
  wet_san   <- param$wet_san
  
  wet_sil   <- param$wet_sil
  
  wet_cla   <- param$wet_cla
  
  wet_sag   <- param$wet_sag
  
  wet_lag   <- param$wet_lag
  
  wet_psol  <- param$wet_psol
  
  wet_psed  <- param$wet_psed
  
  wet_orgn  <- param$wet_orgn
  
  wet_no3   <- param$wet_no3

  wet_nvol  <- param$wet_nvol            # [m3]
  
  wet_mxvol <- param$wet_mxvol           # [m3]

  wet_nsa   <- param$wet_nsa             # [hectare]

  wet_mxsa  <- param$wet_mxsa            # [hectare]
  
  wet_nsed  <- param$wet_nsed * 1e-6     # [ton/m3]
    
  wet_k     <- param$wet_k               # hydraulic conductivity [mm/hr]

  sed_stl   <- param$sed_stl             # fraction of sediment remaining suspended in impoundment after settling for one day [-]

  
  psetlw <- c(param$psetlw / 365,param$psetlw / 365)  # m/year >> m/day
  
  
  nsetlw <- c(param$nsetlw / 365,param$nsetlw / 365) # m/year >> m/day

 
  ipnd1<-1
  
  ipnd2<-1




 # calculate shape parameters for surface area equation
  
  #wetdif <- 0.
  
  #wetdif <- wet_mxvol - wet_nvol
  
 # if ((wet_mxsa - wet_nsa) > 0. & wetdif > 0.) {
    
   # lnvol <- 0.
    
   # lnvol <- log10(wet_mxvol) - log10(wet_nvol)
    
   # if (lnvol > 1.e-4) {
      
    #  bw2 <- (log10(wet_mxsa) - log10(wet_nsa)) / lnvol
      
   # } else {
      
    #  bw2 <- (log10(wet_mxsa) - log10(wet_nsa)) / 0.001
      
   # }
    
   # if (bw2 > 0.9) {bw2 <- .9}
    
   # bw1 <- (wet_mxsa / wet_mxvol) ^ bw2
    
  #} else {
    
   # bw2 <- .9
    
  #  bw1 <- (wet_nsa / wet_nvol) ^ .9
    
 # } 
  

  

if (wet_fr > 0.) {  # r1

# store initial values ----------------------------------------------------

vol <- 0.  # volume of wetland at beginning of day [m3]

sed <- 0.  # sediment concentration in wetland at beginning of day [kg/L]

san <- 0.

sil <- 0.

cla <- 0.

sag <- 0.

lag <- 0.

inised <- 0.

finsed <- 0.

setsed <- 0.

remsetsed <- 0.



vol <- wet_vol  # volume of water in wetlands [m3]

sed <- wet_sed  # sediment concentration in wetland water [kg/L]

san <- wet_san

sil <- wet_sil

cla <- wet_cla

sag <- wet_sag

lag <- wet_lag





# calculate water balance for day -----------------------------------------

# recharge, precipitation and evaporation are accounted in the landscape process

 wetsa <- 0. # surface area of wetland on current day [ha]

 # wetsa <- bw1 * wet_vol ^ bw2
  
  wetsa <- wet_nsa  # [ha]

# wetev <- 10. * evwet * pet_day * wetsa

 wetsep <- wet_k * wetsa * 240.  # seepage from wetland on day [m3] <caluclated based on existing Hrus>

# wetpcp <- percp_day * wetsa * 10.




# calculate water flowing into wetland ---------------------------


wetflwi <- watyld * wet_fr 


# wet_fr: fraction of subbasin (NHD catchment) area that drains into wetland [0-1]


wati <- watyld 


wat <- watyld * (1. - wet_fr) 


wetloss <- wati - wat


watyld <- watyld - wetloss # partition that goes directly to channel and not entering wetland [m3]



# sediment loading to wetland from HRU ------------------------------------

wetsedi <- sedyld * wet_fr 
  

# wetsedi: sediment loading to wetland for day [ton]

wetsani <- sanyld *  wet_fr

wetsili <- silyld *  wet_fr

wetclai <- clayld *  wet_fr

wetsagi <- sagyld *  wet_fr

wetlagi <- lagyld *  wet_fr



sedyld <- sedyld - sedyld * wet_fr # partition of sediment that goes directly to channel and not entering wetland [m3]

sanyld <- sanyld - sanyld * wet_fr

silyld <- silyld - silyld * wet_fr

clayld <- clayld - clayld * wet_fr

sagyld <- sagyld - sagyld * wet_fr

lagyld <- lagyld - lagyld * wet_fr



# --compute nitrogen and phosphorus levels in wetland at beginning of day ----

# equation 29.1.1 in SWAT manual


wet_psol <- wet_psol + psolyld * wet_fr  # soluble P [kg]

# wet_psol: amount of soluble P originating from surface runoff in wetland at beginning of day [kg]

# psolyld: HRU 19 and 20 load for subbasin (NHD catchment) [kg]

wet_psed <- wet_psed + psedyld * wet_fr  # sediment P

# wet_psed: amount of mineral P attached to sediment originating from surface runoff in wetland at

#beginning of day [kg] <HRU 14 and 15 > 

# psedyld: HRU 14 and 15 load for subbasin (NHD catchment) [kg]


wet_orgn <- wet_orgn + orgnyld * wet_fr  # organic N

# wet_orgn: amount of organic N originating from surface runoff in wetland at beginning of day

# orgnyld: HRU 13 load for subbasin (NHD catchment) [kg]

wet_no3 <- wet_no3 + no3yld * wet_fr  # NO3

# wet_no3: amount of nitrate in wetland at beginning of day [kg]

# surno3yld: HRU 16, 17, and 18 load for subbasin (NHD catchment) [kg]





# remove nutrients entering wetlands from HRU loadings --------------------

xx <- 0.

xx <- 1. - wet_fr

orgnyld <- orgnyld * xx  # this partition directly goes to stream without passing through wetland

no3yld <- no3yld * xx

psedyld <- psedyld * xx

psolyld <- psolyld * xx

# chlayld <- chlayld * xx


# new water volume for day ------------------------------------------------

wetev <- 0  # accounted in landscape

wetpcp <- 0 # accounted in landscape

wet_vol <- wet_vol - wetsep - wetev + wetpcp + wetflwi



if (wet_vol < 0.001) { # r2

# check for volume deficit in wetland

# reduce seepage so that the wetland volume is zero

wetsep <- wetsep + wet_vol

wet_vol <- 0.

# if seepage is less than the volume deficit, take the 

# remainder from evaporation

if (wetsep < 0.) {

wetev <- wetev + wetsep

wetsep <- 0.

}

wet_sed <- 0.

wet_san <- 0.

wet_sil <- 0.

wet_cla <- 0.

wet_sag <- 0.

wet_lag <- 0.

wet_psol <- 0.

wet_psed <- 0.

wet_orgn <- 0.

wet_no3 <- 0.

wet_chla <- 0.

wet_seci <- 0.


} else {
  
  

#  compute new sediment concentration -----------------------------------

  wet_sed <- (sed * vol + wetsedi) / wet_vol
  
  wet_san <- (san * vol + wetsani) / wet_vol
  
  wet_sil <- (sil * vol + wetsili) / wet_vol
  
  wet_cla <- (cla * vol + wetclai) / wet_vol
  
  wet_sag <- (sag * vol + wetsagi) / wet_vol
  
  wet_lag <- (lag * vol + wetlagi) / wet_vol
  
  

# compute outflow if wetland water volume > 0 -----------------------------

  
  if (wet_vol <= wet_nvol) {
  
  wetflwo <- 0.
  
  } else {
    
    if (wet_vol <= wet_mxvol) {
  
  wetflwo <- (wet_vol - wet_nvol) 
  
  wet_vol <- wet_vol - wetflwo
  
  } else {
    
  wetflwo <- wet_vol - wet_mxvol
  
  wet_vol <- wet_mxvol
  
  }
  
}
  

  watyld <- watyld + wetflwo # total water goes to channel [m3]
  
  
  

# compute sediment settling -----------------------------------------------

  
  if (sed_stl < 1.e-6) {sed_stl <- 0.0}
  
  # sed_stl: fraction of sediment remaining suspended in impoundment after settling for one day (kg/kg)
  
  inised <- wet_sed 
  
  if (wet_sed > wet_nsed) {
  
  wet_sed <- (wet_sed - wet_nsed) * sed_stl + wet_nsed
  
  # wet_nsed: normal sediment concentration in wetland water [kg/L]
  
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


  wetsedo <- wet_sed * wetflwo
  
  wetsano <- wet_san * wetflwo
  
  wetsilo <- wet_sil * wetflwo
  
  wetclao <- wet_cla * wetflwo
  
  wetsago <- wet_sag * wetflwo
  
  wetlago <- wet_lag * wetflwo
  
  
  sedyld <- sedyld + wetsedo # total amount of sediment contribute to channel [ton]
  
  sanyld <- sanyld + wetsano
  
  silyld <- silyld + wetsilo
  
  clayld <- clayld + wetclao
  
  sagyld <- sagyld + wetsago
  
  lagyld <- lagyld + wetlago
  
    
  
  # net change in amount of sediment in wetland for day
  
  wetsedc <- vol * sed + wetsedi - wetsedo - wet_sed * wet_vol
  
 

# determine settling rate for nutrients -----------------------------------

  
  if (i_mo >= ipnd1 & i_mo <= ipnd2) {
    
  iseas <- 1
  
  } else {
    
    iseas <- 2
    
  }
  
  phosk <- 0.
  
  nitrok <- 0.
  
  phosk <- psetlw[iseas] * wetsa * 10000. / wet_vol
  
  phosk <- min(phosk, 1.)
  
  nitrok <- nsetlw[iseas] * wetsa * 10000. / wet_vol
  
  nitrok <- min(nitrok, 1.)
  
  

# remove nutrients by settling --------------------------------------------

  
  wet_psol <- wet_psol * (1. - phosk)
  
  wet_psed <- wet_psed * (1. - phosk)
  
  wet_orgn <- wet_orgn * (1. - nitrok)
  
  wet_no3 <- wet_no3 * (1. - nitrok)
  
  
  
  
  if (wet_orgn < 1.e-6) {wet_orgn <- 0.0}
  
  if (wet_no3 < 1.e-6) {wet_no3 <- 0.0}
  
  if (wet_psed < 1.e-6) {wet_psed <- 0.0}
  
  if (wet_psol < 1.e-6) {wet_psol <- 0.0}
  
  
  tpco <- 0.
  
  tpco <- 1.e6 * (wet_psol + wet_psed) / (wet_vol + wetflwo) 
                    
                      
 # chlaw <- 1  # chlorophyll-a production coefficient for wetland
  
 # chlaco <- 0.
  
 # wet_chla <- 0.
  
#  wet_seci <- 0.
  
  
#  if (tpco > 1.e-4) {
  
 # equation 29.1.6 in SWAT manual
  
 # chlaco <- chlaw * 0.551 * (tpco ^ 0.76)
  
 #  wet_chla <- chlaco * (wet_vol + wetflwo) * 1.e-6
  
 # }
  
 # if (chlaco > 1.e-4) {
  
  # equation 29.1.8 in SWAT manual
  
  # wet_seci <- secciw * 6.35 * (chlaco ^ (-0.473))
  
 # }
  
  

# compute nutrients leaving wetland ---------------------------------------

  
  yy <- 0.
  
  yy <- wetflwo / (wet_vol + wetflwo)
  
  orgnyld <- orgnyld + wet_orgn * yy   # total organic nitrogen contributes to channel
  
  no3yld <- no3yld + wet_no3 * yy 
  
  psedyld <- psedyld + wet_psed * yy 
  
  psolyld <- psolyld + wet_psol * yy


  
  

  # tip: total amount goes to channel

# update nutrient pools in wetlands ---------------------------------------

  
 
  wet_orgn <- wet_orgn * (1. - yy)  # remaining organic N in wetland [kg]
  
  wet_no3 <- wet_no3 * (1. - yy)
  
  wet_psed <- wet_psed * (1. - yy)
  
  wet_psol <- wet_psol * (1. - yy)
  
  # wet_chla <- wet_chla * (1. - yy)
  
  
  
} # r2
  
}  # r1
  
  



if (watyld < 0.) {watyld <- 0.}

if (sedyld < 0.) {

sedyld <- 0.0

sanyld <- 0.0

silyld <- 0.0

clayld <- 0.0

sagyld <- 0.0

lagyld <- 0.0

}
  
  
  
  output<-c( watyld, sedyld, sanyld, silyld, clayld, sagyld, lagyld, orgnyld, no3yld, psedyld, psolyld,
             
             wet_vol, wet_sed,wet_san, wet_sil, wet_cla, wet_sag, wet_lag, wet_psol,
             
             wet_psed, wet_orgn, wet_no3)
            
  
  return(output)
  


} # function
