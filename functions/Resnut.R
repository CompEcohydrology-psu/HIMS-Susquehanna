################################################################################

# Routing nutrients through reservoir

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

# References: 

# SWAT source code - https://swat.tamu.edu/software/swat/

# SWAT 2009 theory

################################################################################



Resnut<-function(resflwo,res_vol,ressa,tmpav,varoute_4,varoute_5,varoute_6,
                 
                 varoute_7,varoute_14,varoute_15,i_mo,routingParams){
  

  
  theta <- function(r20,thk,tmp){ # internal function
    
    r_theta <- 0.
    
    r_theta <- r20 * thk ^ (tmp - 20.)
    
    
    
    
    
    return(r_theta)
    
  }
  
  
  
  # read parameters:
  
  
  ires1 <- routingParams$ires1
  
  ires2 <- routingParams$ires2
  
  psetlr1 <- routingParams$psetlr1       #  m/year  
  
  psetlr2<-routingParams$psetlr2         #  m/year
  
  nsetlr1 <- routingParams$nsetlr1       #  m/year
  
  nsetlr2 <- routingParams$nsetlr2       #  m/year
  
  theta_n <- routingParams$theta_n
  
  theta_p <- routingParams$theta_p
  
  con_pirr <- routingParams$con_pirr
  
  con_nirr <- routingParams$con_nirr
  
  chlar <- routingParams$chlar
  
  seccir <- routingParams$seccir        # water clarity coefficient for reservoir  [-]
  
  res_solp  <- routingParams$res_solp   # amount of soluble P in reservoir        [kg]
  
  res_orgp <- routingParams$res_orgp    # amount of organic P in reservoir        [kg]
  
  res_no3 <- routingParams$res_no3      # amount of nitrate in reservoir          [kg]
  
  res_no2 <- routingParams$res_no2      # amount of nitrite in reservoir          [kg]
  
  res_nh3 <- routingParams$res_nh3      # amount of ammonia in reservoir          [kg]
  
  res_orgn <- routingParams$res_orgn    # amount of organic N in reservoir        [kg]
  
  res_chla <- routingParams$res_chla    # amount of chlorophyll-a in reservoir    [kg]
  
  
  psetlr <- c()
  
  nsetlr <-c()
  
  ires_nut <- 0
  
  
  psetlr[1] <- psetlr1 / 365   # >> m/day

  psetlr[2] <- psetlr2 / 365   # >> m/day

  nsetlr[1] <- nsetlr1 / 365   # >> m/day

  nsetlr[2] <- nsetlr2 / 365   # >> m/day
  
  
  
  # if reservoir volume less than 1 m^3, set all nutrient levels to
  
  # zero and perform no nutrient calculations
  
  if (res_vol < 1.) {
    
  res_orgn <- 0.
  
  res_orgp <- 0.
  
  res_no3 <- 0.
  
  res_nh3 <- 0.  # amount of ammonia in reservoir (kg)
  
  res_no2 <- 0.
  
  res_solp <- 0.
  
  res_chla <- 0.
  
  res_seci <- 0.
  
  resorgno <- 0.
  
  resorgpo <- 0.
  
  ressolpo <- 0.
  
  reschlao <- 0.
  
  resnh3o <- 0.
  
  resno2o <- 0.
  
  resno3o <- 0.
  
    output<-c(res_orgn,res_orgp,res_no3,res_nh3, res_no2, res_solp,res_chla,
            
            resorgno,  resorgpo, ressolpo,  reschlao, resnh3o,  resno2o,
            
            resno3o,varoute_4,varoute_5,varoute_6,varoute_7,varoute_14,
            
            varoute_15)
  
  return(output)

  }
 
  
  
  
 
  
  
  # if reservoir volume greater than 1 m^3, perform nutrient calculations
  
  if (i_mo >= ires1 & i_mo <= ires2) {
    
  iseas <- 1
  
  }else{
    
  iseas <- 2
  
  }
  
  # add incoming nutrients to those in reservoir
  
  # equation 29.1.1 in SWAT manual
  
  res_orgn <- res_orgn + varoute_4
  
  res_orgp <- res_orgp + varoute_5
  
  res_no3 <- res_no3 + varoute_6
  
  res_nh3 <- res_nh3 + varoute_14
  
  res_no2 <- res_no2 + varoute_15
  
  res_solp <- res_solp + varoute_7

  

  
  
  conc_p <- (res_orgp + res_solp) / res_vol
  
  conc_n <- (res_orgn + res_no3 + res_nh3 + res_no2) / res_vol
  
  conc_n <- res_no3 / res_vol
  
  
  # settling rate/mean depth
  
  # part of equation 29.1.3 in SWAT manual
  
  # ires_nut = 1 new equations 0 = old equations (Ikenberry)
  
  if (ires_nut == 1) {
    
  phosk <- ressa * 10000. * (conc_p - con_pirr) *
    
                        theta(psetlr[iseas], theta_p, tmpav)
  
  nitrok <- ressa * 10000. * (conc_n - con_nirr) * 
    
                        theta(nsetlr[iseas], theta_n, tmpav)
  
  }else{
    
    phosk <- psetlr[iseas] * ressa * 10000. / (res_vol + resflwo)
  
  nitrok <- nsetlr[iseas] * ressa * 10000. /  (res_vol + resflwo)
  
  }
  
  
  nitrok <- max(nitrok, 0.)
  
  phosk <- max(phosk, 0.)
  
  nitrok <- min(nitrok, 1.)
  
  phosk <- min(phosk, 1.)
  
  
  # remove nutrients from reservoir by settling
  
  # other part of equation 29.1.3 in SWAT manual
  
  res_solp <- res_solp * (1. - phosk)
  
  res_orgp <- res_orgp * (1. - phosk)
  
  res_orgn <- res_orgn * (1. - nitrok)
  
  res_no3 <- res_no3 * (1. - nitrok)
  
  res_nh3 <- res_nh3 * (1. - nitrok)
  
  res_no2 <- res_no2 * (1. - nitrok)
  
  
  # calculate chlorophyll-a and water clarity
  
  tpco <- 0.
  
  chlaco <- 0.
  
  res_chla <- 0.
  
  res_seci <- 0.


  
  tpco <- 1.e+6 * (res_solp + res_orgp) /(res_vol + resflwo)
  
  if (tpco > 1.e-4) {
  
  # equation 29.1.6 in SWAT manual
  
  chlaco <- chlar * 0.551 * (tpco ^ 0.76)
  
  res_chla <- chlaco * (res_vol + resflwo) * 1.e-6
  
  }
  
 # if (chlaco > 1.e-4) {
  
  # equation 29.1.8 in SWAT manual
  
 # res_seci <- seccir * 6.35 * (chlaco ^ (-0.473))
  
#}
  
  
  # calculate amount of nutrients leaving reservoir
  
  if (res_no3 < 1.e-4) {res_no3 <- 0.0}
  
  if (res_orgn < 1.e-4) {res_orgn <- 0.0}
  
  if (res_orgp < 1.e-4) {res_orgp <- 0.0}
  
  if (res_solp < 1.e-4) {res_solp <- 0.0}
  
  if (res_chla < 1.e-4) {res_chla <- 0.0}
  
  if (res_nh3 < 1.e-4) {res_nh3 <- 0.0}
  
  if (res_no2 < 1.e-4) {res_no2 <- 0.0}
  
  resno3o <- res_no3 * resflwo / (res_vol + resflwo) #amount of nitrate leaving 
  
  #reservoir on day [kg]
  
  resorgno <- res_orgn * resflwo / (res_vol + resflwo)
  
  resorgpo <- res_orgp * resflwo / (res_vol + resflwo)
  
  ressolpo <- res_solp * resflwo / (res_vol + resflwo)
  
  reschlao <- res_chla * resflwo / (res_vol + resflwo)
  
  resnh3o <- res_nh3 * resflwo / (res_vol + resflwo)
  
  resno2o <- res_no2 * resflwo / (res_vol + resflwo)
  
  res_orgn <- res_orgn - resorgno #amount of organic N in reservoir [kg]
  
  res_orgp <- res_orgp - resorgpo
  
  res_no3 <- res_no3 - resno3o
  
  res_nh3 <- res_nh3 - resnh3o
  
  res_no2 <- res_no2 - resno2o
  
  res_solp <- res_solp - ressolpo
  
  res_chla <- res_chla - reschlao
  
  
  output<-c(res_orgn,res_orgp,res_no3,res_nh3, res_no2, res_solp,res_chla,
            
            resorgno,  resorgpo, ressolpo,  reschlao, resnh3o,  resno2o,
            
            resno3o,varoute_4,varoute_5,varoute_6,varoute_7,varoute_14,
            
            varoute_15)
  
  
  return(output)
} # end of function








