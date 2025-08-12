################################################################################

# This function routes nutrients in channel

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

# References: 

# SWAT source codes

# SWAT 2009 theory

################################################################################



Watqual<-function( varoute_2,rchwtr,rchdep,tmpav,hru_ra,dayl,

                           varoute_4,varoute_5,varoute_6, varoute_7,varoute_13,
						   
                           varoute_14,varoute_15,varoute_16,varoute_17,i_mo,
						   
                           rttime,S_wet, wet_orgn_out,wet_no3_out,wet_psed_out,
						   
                          wet_psol_out, wet_no2_out, wet_nh4_out,
						  
						  routingParams){
  

  


  Theta <- function(r20,thk,tmp){ # internal function
    
   # r20  reaction rate at 20 degrees centigrade
    
   # tmp: temperature on current day
    
    r_theta <- 0.
    
    r_theta <- r20 * thk ^ (tmp - 20.)
    
    return(r_theta)
    
  }
  

  
  dcoef <- 3.
  
  
  # read parameters:
  
  
  bc1 <- routingParams$bc1
  
  bc2 <- routingParams$bc2
  
  bc3 <- routingParams$bc3
  
  bc4 <- routingParams$bc4
  
  ai0 <- routingParams$ai0
  
  ai1 <- routingParams$ai1
  
  ai2 <- routingParams$ai2
  
  ai3 <- routingParams$ai3
  
  ai4 <- routingParams$ai4
  
  ai5 <- routingParams$ai5
  
  ai6 <- routingParams$ai6
  
  lambda0 <- routingParams$lambda0
  
  lambda1 <- routingParams$lambda1
  
  lambda2 <- routingParams$lambda2
  
  k_n <- routingParams$k_n
  
  p_n <- routingParams$p_n
  
  k_p <- routingParams$k_p
  
  k_l <- routingParams$k_l
  
  mumax <- routingParams$mumax
  
  rhoq <- routingParams$rhoq
  
  rs1 <- routingParams$rs1
  
  rs2 <- routingParams$rs2
  
  rs3 <- routingParams$rs3
  
  rs4 <- routingParams$rs4
  
  rs5 <- routingParams$rs5
  
  rk1 <- routingParams$rk1
  
  rk2 <- routingParams$rk2
  
  rk3 <- routingParams$rk3
  
  rk4 <- routingParams$rk4
  
  igropt <- routingParams$igropt
  
  tfact <- routingParams$tfact
  
  algae <- routingParams$algae 
  
  organicn <- routingParams$organicn
  
  ammonian <- routingParams$ammonian
  
  nitriten <- routingParams$nitriten # nitrite concentration in reach [mg/L]
  
  nitraten <- routingParams$nitraten # nitrate concentration in reach [mg/L]
  
  organicp <- routingParams$organicp # organic phosphorus concentration in reach
  
  disolvp <- routingParams$disolvp
  
  rch_cbod <- routingParams$rch_cbod
  
  rch_dox <- routingParams$rch_dox
  
  tmp_win1 <- routingParams$tmp_win1
  
  tmp_win2 <- routingParams$tmp_win2
  
  tmp_spr1 <- routingParams$tmp_spr1
  
  tmp_spr2 <- routingParams$tmp_spr2
  
  tmp_sum1 <- routingParams$tmp_sum1
  
  tmp_sum2 <- routingParams$tmp_sum2
  
  tmp_fal1 <- routingParams$tmp_fal1
  
  tmp_fal2 <- routingParams$tmp_fal2
  
 

  
  # Temperature adjustment factors:
  
  thgra <- 1.047
  
  thrho <- 1.047
  
  thrs1 <- 1.024
  
  thrs2 <- 1.074
  
  thrs3 <- 1.074
  
  thrs4 <- 1.024
  
  thrs5 <- 1.024
  
  thbc1 <- 1.083
  
  thbc2 <- 1.047
  
  thbc3 <- 1.047
  
  thbc4 <- 1.047
  
  thrk1 <- 1.047
  
  thrk2 <- 1.024
  
  thrk3 <- 1.024
  
  thrk4 <- 1.060
  

  
  # initialize water flowing into reach
  
   wtrin <- 0.
   
   wtrin<- varoute_2 + S_wet  # [m3]

  #  S_wet: Surface flux between wetland and channel [m3]
  
  
  
  if (wtrin > 1.e-4) { #1
    
  # wtrin: water flowing into reach on day [m3]
    
  # concentrations
    
  # initialize inflow concentrations
  
    chlin <- 0.   # chlorophyll-a concentration in inflow [mg/L]
    
    algin <- 0.   # algal biomass concentration in inflow [mg/L]
    
    orgnin <- 0.  # organic N concentration in inflow [mg/L]
    
    ammoin <- 0.  # ammonium N concentration in inflow [mg/L]
    
    nitritin <- 0. # nitrite concentration in inflow [mg/L]
    
    nitratin <- 0. # nitrate concentration in inflow [mg/L]
    
    orgpin <- 0.   # organic P concentration in inflow [mg/L]
    
    dispin <- 0.  # soluble P concentration in inflow [mg/L]
    
    cbodin <- 0.  # carbonaceous biological oxygen demand [mg/L]
    
    disoxin <- 0. # dissolved oxygen concentration in inflow [mg/L]
    
    cinn <- 0. # effective available nitrogen concentration [mg/L]
  
  
    if (wtrin > 0.001) {
    
    chlin <- 1000. * varoute_13  / wtrin   # convert to [mg/L]
    
    #varoute_13:  chlorophyll-a [kg]
    
    
    algin <- 1000. * chlin / ai0        # QUAL2E equation III-1
    
    #ai0:   ratio of chlorophyll-a to algal biomass [ug chla/mg alg]

    orgnin <- 1000. * (varoute_4 + wet_orgn_out)  / wtrin   # convert to mg/L
    
    ammoin <- 1000. * (varoute_14 + wet_nh4_out )  / wtrin
    
    nitritin <- 1000. * (varoute_15 +  wet_no2_out )  / wtrin
    
    nitratin <- 1000. * (varoute_6 + wet_no3_out )   / wtrin
    
    orgpin <- 1000. * (varoute_5 + wet_psed_out)  / wtrin
    
    dispin <- 1000. * (varoute_7 + wet_psol_out )  / wtrin
    
    cbodin <- 1000. * varoute_16  / wtrin
    
    disoxin <- 1000. * varoute_17  / wtrin
 
  }
  
  
 
    
    
    # initialize concentration of nutrient in reach
    
    wtrtot <- 0.  # inflow + storage water [m3]
    
    algcon <- 0. # initial algal biomass concentration in reach [mg/L]
    
    orgncon <- 0. # initial organic N concentration in reach [mg/L]
    
    nh3con <- 0. # initial ammonia concentration in reach [mg/L]
    
    no2con <- 0. # initial nitrite concentration in reach [mg/L]
    
    no3con <- 0. # initial nitrate concentration in reach [mg/L]
    
    orgpcon <- 0. # initial organic P concentration in reach [mg/L]
    
    solpcon <- 0. # initial soluble P concentration in reach [mg/L]
    
    cbodcon <- 0. # initial carbonaceous biological oxygen demand [mg/L]
    
    o2con <- 0. # initial dissolved oxygen concentration in reach [mg/L]
    
    rch_cbod <- max(1.e-6,rch_cbod)
    
    wtrtot <- wtrin + rchwtr
    
    #rchwtr: water stored in reach at beginning of day [m3]
    
    algcon <- (algin * wtrin + algae * rchwtr) / wtrtot
    
    #algae: algaealgal biomass concentration in reach [mg/L]
    
    orgncon <- (orgnin * wtrin + organicn * rchwtr) / wtrtot
    
    #organicn: organic nitrogen concentration in reach [mg/L]
    
    nh3con <- (ammoin * wtrin + ammonian * rchwtr) / wtrtot
    
    #ammonian: ammonia concentration in reach [mg/L]
    
    no2con <- (nitritin * wtrin + nitriten * rchwtr) / wtrtot
    
    #nitriten: nitrite concentration in reach [mg/L]
    
    no3con <- (nitratin * wtrin + nitraten * rchwtr) / wtrtot
    
    #nitraten: nitrate concentration in reach [mg/L]
    
    orgpcon <- (orgpin * wtrin + organicp * rchwtr) / wtrtot
    
    #organicp: organic phosphorus concentration in reach [mg/L]
    
    solpcon <- (dispin * wtrin + disolvp * rchwtr) / wtrtot
    
    #disolvp: dissolved phosphorus concentration in reach [mg/L]
    
    cbodcon <- (cbodin * wtrin + rch_cbod * rchwtr) / wtrtot
    
    #rch_cbod: carbonaceous biochemical oxygen demand in reach [mg/L]
    
    o2con <- (disoxin * wtrin + rch_dox * rchwtr) / wtrtot
    
    #rch_dox: o2 condissolved oxygen concentration in reach


    if (algcon < 1.e-6) {algcon <- 0.0}

    if (orgncon < 1.e-6) {orgncon <- 0.0}
    
    if (nh3con < 1.e-6) {nh3con <- 0.0}
    
    if (no2con < 1.e-6) {no2con <- 0.0}
    
    if (no3con < 1.e-6) {no3con <- 0.0}
    
    if (orgpcon < 1.e-6) {orgpcon <- 0.0}
    
    if (solpcon < 1.e-6) {solpcon <- 0.0}
    
    if (cbodcon < 1.e-6) {cbodcon <- 0.0}
    
    if (o2con < 1.e-6) {o2con <- 0.0}
    
    
	
    
    # calculate temperature in stream
    
    # Stefan and Preudhomme. 1993.  Stream temperature estimation 
    
    # from air temperature.  Water Res. Bull. p. 27-45
    
    # SWAT manual equation 2.3.13
    
    # check for month to input water temperature parms
    
    if (i_mo == 12 | i_mo == 1 | i_mo == 2) {
    
    wtmp <- tmp_win1 + tmp_win2 * tmpav
    
  } 
    
    if (i_mo == 3 | i_mo == 4 | i_mo == 5) {
      
    wtmp <- tmp_spr1 + tmp_spr2 * tmpav
    
    }  
    
    if (i_mo == 6 | i_mo == 7 | i_mo == 8) {
    
    wtmp <- tmp_sum1 + tmp_sum2 * tmpav
    
  }  
    
    if (i_mo == 9 | i_mo == 10 | i_mo == 11) {
      
    wtmp <- tmp_fal1 + tmp_fal2 * tmpav
    
    }
    
    water_temp<-wtmp # added by my own
    
 
    
    if (wtmp <= 0.) {wtmp <- 0.1}
    
    # calculate effective concentration of available nitrogen
    
    # QUAL2E equation III-15
    
    cinn <- nh3con + no3con  #effective available nitrogen concentration [mg/L]
    
    # calculate saturation concentration for dissolved oxygen
    
    # QUAL2E section 3.6.1 equation III-29
    
    ww <- 0.
    
    xx <- 0.
    
    yy <- 0.
    
    zz <- 0.
    
    
    ww <- -139.34410 + (1.575701e05 / (wtmp + 273.15))
    
    xx <- 6.642308e07 / ((wtmp + 273.15)^2)
    
    yy <- 1.243800e10 / ((wtmp + 273.15)^3)
    
    zz <- 8.621949e11 / ((wtmp + 273.15)^4)
    
    soxy <- exp(ww - xx + yy - zz)
    
    if (soxy < 1.e-6) {soxy <- 0.} 
    
    # end initialize concentrations
    
    # O2 impact calculations
    
    # calculate nitrification rate correction factor for low
    
    # oxygen QUAL2E equation III-21
    
    cordo <- 0.
    
    if (o2con <= 0.001) {o2con <- 0.001}
    
    if (o2con > 30.) {o2con <- 30.}
    
    cordo <- 1.0 - exp(-0.6 * o2con)
    
    # modify ammonia and nitrite oxidation rates to account for
    
    # low oxygen
    
    bc1mod <- 0. 
    
    bc2mod <- 0.
    
    bc1mod <- bc1 * cordo
    
    bc2mod <- bc2 * cordo
    
    # bc1: rate constant for biological oxidation of NH3 to NO2 in reach 
    
    # at 20 deg C [1/day]
    
    # rate constant for biological oxidation of NO2 to NO3 in reach
    
    # at 20 deg C [1/day]
    
    
    # end O2 impact calculations
    
    
    # calculate flow duration
    
    tday <- 0.
    
    tday <- rttime / 24.0  # convert to day
    
    # rttime: reach travel time [hr]
    
    if (tday > 1.0) {tday <- 1.0}
    
    #     tday = 1.0
    
    # algal growth
    
    # calculate light extinction coefficient 
    
    # (algal self shading) QUAL2E equation III-12
    
    if (ai0 * algcon > 1.e-6) {
      
      
    lambda <- lambda0 + (lambda1 * ai0 * algcon) + lambda2 *
      
      (ai0 * algcon) ^ (.66667)
    
    
    # lambda: light extinction coefficient [1/m]
    
    #lambda0: non-algal portion of the light extinction coefficient [1/m]
    
    #lambda1: linear algal self-shading coefficient  1/(m*ug chla/L)
    
    #lambda2: linear algal self-shading coefficient (1/m)(ug chla/L)**(-2/3)
    
    }else{
      
      lambda <- lambda0
      
    }
    
    if (lambda > lambda0) {lambda <- lambda0}
	
	
	
    
    
    # calculate algal growth limitation factors for nitrogen
    
    # and phosphorus QUAL2E equations III-13 & III-14
    
    fnn <- 0. # algal growth limitation factor for nitrogen [-]
    
    fpp <- 0. # algal growth limitation factor for phosphorus [-]
    
    fnn <- cinn / (cinn + k_n)
    
    # k_n: michaelis-menton half-saturation constant for nitrogen [mg/L]
    
    fpp <- solpcon / (solpcon + k_p)
    
    # k_p: michaelis-menton half saturation constant for phosphorus [mg/L]
    
    # calculate daylight average, photosynthetically active,
    
    # light intensity QUAL2E equation III-8
    
    # Light Averaging Option # 2
    
    algi <- 0.
    
    if (dayl > 0.) {
      
      # dayl: day length for current day [hours]
      
    algi <- hru_ra * tfact / dayl
    
   # tfact: fraction of solar radiation computed in the temperature heat balance
    
   # that isphotosynthetically active
    
   # hru_ra: solar radiation for the day in HRU [MJ/m^2] 
    
    }else{
      
      algi <- 0.
    
    }
    
    
    # calculate growth attenuation factor for light, based on
    
    # daylight average light intensity QUAL2E equation III-7b
    
    fl_1 <- 0.
    
    fll <- 0.
    
    fl_1 <- (1. / (lambda * rchdep)) *                            
             log((k_l + algi) / (k_l + algi * (exp(-lambda * rchdep))))
    
    
    #k_l: half saturation coefficient for light MJ/(m2*hr)
    
    fll <- 0.92 * (dayl / 24.) * fl_1
    
    
    
    # calculcate local algal growth rate
    
    #    igropt   Qual2E option for calculating the local
    #                           specific growth rate of algae
    #                           1: multiplicative:
      #                         u = mumax * fll * fnn * fpp
    #                           2: limiting nutrient
    #                           u = mumax * fll * Min(fnn, fpp)
    #                           3: harmonic mean
    #                           u = mumax * fll * 2. / ((1/fnn)+(1/fpp))
    
    
 
    
    
    gra <- 0.
    
    if(igropt==1){
      
      # multiplicative QUAL2E equation III-3a
      
      gra <- mumax * fll * fnn * fpp
      
      #mumax: maximum specific algal growth rate at 20 deg C
      
    }
    
    if(igropt==2){
      
      # limiting nutrient QUAL2E equation III-3b
      
      gra <- mumax * fll * min(fnn, fpp)
      
      
    }
    
    
    if(igropt==3){
      
      # harmonic mean QUAL2E equation III-3c
      
      if (fnn > 1.e-6 & fpp > 1.e-6) {
        
      gra <- mumax * fll * 2. / ((1. / fnn) + (1. / fpp))
      
      }else{
        
        gra <- 0.
        
      }
      
      
    }
    
    

    
    # calculate algal biomass concentration at end of day
    
    # (phytoplanktonic algae)
    
    # QUAL2E equation III-2
    
    algae <- 0.
    
    algae <- algcon + (Theta(gra,thgra,wtmp) * algcon -       
              Theta(rhoq,thrho,wtmp) * algcon - Theta(rs1,thrs1,wtmp) 
                                          / rchdep * algcon) * tday
										 									  
    
    # thgra: temperature adjustment factor for local algal growth rate [-]
    
    # thrho: temperature adjustment factor for local algal [-]
    
    # thrs1: temperature adjustment factor for local algal [-]
    
    # rhoq: algal respiration rate at 20 deg C
    
    #rs1: local algal settling rate in reach at 20 deg [m/day]
    
    if (algae < 1.e-6) {algae <- 0.}
    
    # JGA added to set algae limit *****
      
      if (algae > 5000.) {algae <- 5000.}
    
    if (algae > (dcoef * algcon)) {algae <- dcoef * algcon}
    
    
    # calculate chlorophyll-a concentration at end of day
    
    # QUAL2E equation III-1
    
    chlora <- 0.
    
    chlora <- algae * ai0 / 1000.
    
    # end algal growth 
    
    # oxygen calculations
    
    # calculate carbonaceous biological oxygen demand at end
    
    # of day QUAL2E section 3.5 equation III-26
    
    yy <- 0.
    
    zz <- 0.
    
    yy <- Theta(rk1,thrk1,wtmp) * cbodcon
    
    # rk1: CBOD deoxygenation rate coefficient in reach at 20 deg C [1/day]
    
    # thrk1: temperature adjustment factor for local CBOD deoxygenation [-]
    
    zz <- Theta(rk3,thrk3,wtmp) * cbodcon
    
    #rk3: rate of loss of CBOD due to settling in reach at 20 deg C
    
    #thrk3: temperature adjustment factor for loss of CBOD due to settling
    
    rch_cbod <- 0.
    
    rch_cbod <- cbodcon - (yy + zz) * tday
    
    
    
    # deoxygenation rate
    
    coef <- exp(-Theta(rk1,thrk1,wtmp) * tday)
    
    cbodrch <- coef * cbodcon
    
    #cbod rate loss due to settling
    
    coef <- exp(-Theta(rk3,thrk3,wtmp) * tday)
    
    cbodrch <- coef * cbodrch
    
    rch_cbod <- cbodrch
    
    
    if (rch_cbod < 1.e-6) {rch_cbod <- 0.}
    
    if (rch_cbod > (dcoef * cbodcon)) {rch_cbod <- dcoef * cbodcon}
    
    
    
    # calculate dissolved oxygen concentration if reach at 
    
    # end of day QUAL2E section 3.6 equation III-28
    
    uu <- 0.
    
    vv <- 0.
    
    ww <- 0.
    
    xx <- 0.
    
    yy <- 0.
    
    zz <- 0.
    
    rhoq <- 1.0
    
    rk2 <- 1.0
    
    uu <- Theta(rk2,thrk2,wtmp) * (soxy - o2con)
    
    #thrk2: temperature adjustment factor for local oxygen reaeration rate [-]
    
    
    vv <- (ai3 * Theta(gra,thgra,wtmp) - ai4 *                      
                      Theta(rhoq,thrho,wtmp)) * algcon
    
    # ai3: the rate of oxygen production per unit of algal photosynthesis [-]
    
    # ai4: the rate of oxygen uptake per unit of algae respiration [-]
    
    ww <- Theta(rk1,thrk1,wtmp) * cbodcon
    
    xx <- Theta(rk4,thrk4,wtmp) / (rchdep * 1000.)
    
    yy <- ai5 * Theta(bc1mod,thbc1,wtmp) * nh3con
    
    zz <- ai6 * Theta(bc2mod,thbc2,wtmp) * no2con
    
    # ai5: the rate of oxygen uptake per unit of NH3 nitrogen oxidation [mg/L]
    
    # ai6: the rate of oxygen uptake per unit of NO2 nitrogen oxidation [mg/L]
    
    #thbc1: temperature adjustment factor for local biological 
    
    #oxidation of NH3 to NO2 [-]
    
    #thbc2: temperature adjustment factor for local biological oxidation
    
    # of NO2 to NO3 [-]
    
    
    
    rch_dox <- o2con + (uu + vv - ww - xx - yy - zz) * tday
    
    rch_dox <- min(0.1, rch_dox)
    
    
    #algea O2 production minus respiration
    
    #if (vv > 0.) then
    
    doxrch <- soxy
    
    #else
    
      #  coef = exp(-0.03 * vv)
    
    #  doxrch = coef * soxy
    
    #end if
    
    
    #cbod deoxygenation
    
    coef <- exp(-0.1 * ww)
    
    doxrch <- coef * doxrch
    
    #benthic sediment oxidation
    
    coef <- 1. - (Theta(rk4,thrk4,wtmp) / 100.)
    
    doxrch <- coef * doxrch
    
    #ammonia oxydation
    
    coef <- exp(-0.05 * yy)
    
    doxrch <- coef * doxrch
    
    #nitrite oxydation
    
    coef <- exp(-0.05 * zz)
    
    doxrch <- coef * doxrch
    
    #reaeration
    
    uu <- Theta(rk2,thrk2,wtmp) / 100. * (soxy - doxrch)
    
    rch_dox <- doxrch + uu
    
    
    if (rch_dox < 1.e-6) {rch_dox <- 0.}
    
    if (rch_dox > soxy) {rch_dox <- soxy}
    
    # if (rch_dox(jrch) > dcoef * o2con) rch_dox(jrch)= dcoef * o2con
    
    # end oxygen calculations
    
   
    
     #######################
     # nitrogen calculations
     #######################
    
    # calculate organic N concentration at end of day
    
    # QUAL2E section 3.3.1 equation III-16
    
    xx <- 0.
    
    yy <- 0.
    
    zz <- 0.
    
    xx <- ai1 * Theta(rhoq,thrho,wtmp) * algcon
    
    yy <- Theta(bc3,thbc3,wtmp) * orgncon
    
    # bc3: rate constant for hydrolysis of organic N to ammonia in 
    
    # reach at 20 deg C [1/day]
    
    # thbc3: temperature adjustment factor for local  [-]
    
    zz <- Theta(rs4,thrs4,wtmp) * orgncon
    
    
    # rs4: rate coefficient for organic nitrogen settling in 
    
    #reach at 20 deg C [1/day]
    
    # thrs4: temperature adjustment factor for local organic N settling
    
    #rate [none]
    
    
    #        red_fac = orgncon / 4.
    
    #        if (red_fac > 0.75) red_fac = 0.75
    
    #        zz = zz + red_fac
    
    organicn <- 0.
    
    organicn <- orgncon + (xx - yy - zz) * tday
    
    if (organicn < 1.e-6) {organicn <- 0.}
    
    if (organicn > (dcoef * orgncon)) {organicn <- dcoef * orgncon}
    
    
    
    # calculate fraction of algal nitrogen uptake from ammonia
    
    # pool QUAL2E equation III-18
    
    f1 <- 0.
    
    f1 <- p_n * nh3con / (p_n * nh3con + (1. - p_n) * no3con +  1.e-6)
    
    #p_n: algal preference factor for ammonia
    
    
    ##### calculate ammonia nitrogen concentration at end of day
    
    # QUAL2E section 3.3.2 equation III-17
    
    ww <- 0.
    
    xx <- 0.
    
    yy <- 0.
    
    zz <- 0.
    
    ww <- Theta(bc3,thbc3,wtmp) * orgncon
    
    xx <- Theta(bc1mod,thbc1,wtmp) * nh3con
    
    yy <- Theta(rs3,thrs3,wtmp) / (rchdep * 1000.)
    
    #rs3: benthos source rate for ammonia nitrogen
    
    #in((m**2)*day)reach at 20 deg C
    
    #thrs3: temperature adjustment factor for local benthos 
    
    #source rate for ammonia nitrogen
    
    
    zz <- f1 * ai1 * algcon * Theta(gra,thgra,wtmp)
    
    #ai1: fraction of algal biomass that is nitrogen
    
    
    ammonian <- 0.
    
    ammonian <- nh3con + (ww - xx + yy - zz) * tday
    
    if (ammonian < 1.e-6) {ammonian <- 0.}
    
    if (ammonian > (dcoef * nh3con) & nh3con > 0.){ammonian <- dcoef * nh3con}  
   
    
    ##### calculate concentration of nitrite at end of day
    
    # QUAL2E section 3.3.3 equation III-19
    
    yy <- 0.
    
    zz <- 0.
    
    yy <- Theta(bc1mod,thbc1,wtmp) * nh3con
    
    zz <- Theta(bc2mod,thbc2,wtmp) * no2con
    
    nitriten <- 0.
    
    nitriten <- no2con + (yy - zz) * tday
    
    if (nitriten < 1.e-6) {nitriten <- 0.}
    
    if (nitriten > (dcoef * no2con) & no2con > 0.) {nitriten <- dcoef * no2con}
    
    
    
    ##### calculate nitrate concentration at end of day
    
    # QUAL2E section 3.3.4 equation III-20
    
    yy <- 0.
    
    zz <- 0.
    
    yy <- Theta(bc2mod,thbc2,wtmp) * no2con
    
    zz <- (1. - f1) * ai1 * algcon * Theta(gra,thgra,wtmp)
    
    nitraten <- 0.
    
    nitraten <- no3con + (yy - zz) * tday
    
    if (nitraten > dcoef * no3con) {nitraten <- dcoef * no3con}
    
    if (nitraten < 1.e-6) {nitraten <- 0.}
    
    ##### end nitrogen calculations
    
    ################################
    # phosphorus calculations
    ################################
    
    
    # calculate organic phosphorus concentration at end of
    
    # day QUAL2E section 3.3.6 equation III-24
    
    xx <- 0.
    
    yy <- 0.
    
    zz <- 0.
    
    xx <- ai2 * Theta(rhoq,thrho,wtmp) * algcon
    
    #ai2: fraction of algal biomass that is phosphorus
    
    yy <- Theta(bc4,thbc4,wtmp) * orgpcon
    
    zz <- Theta(rs5,thrs5,wtmp) * orgpcon
    
    #bc4: rate constant for the decay of organic P to dissolved P 
    
    #in reach at 20 deg C
    
    #thbc4: temperature adjustment factor for local
    
    #rs5: organic phosphorus settling rate in reach at 20 deg C [1/day]
    
    #thrs5: temperature adjustment factor for local organic P settling rate
    
  
    organicp <- 0.
    
    organicp <- orgpcon + (xx - yy - zz) * tday
    
    if (organicp < 1.e-6) {organicp <- 0.}
    
    if (organicp > dcoef * orgpcon) {organicp <- dcoef * orgpcon}
    
    
    ##### calculate dissolved phosphorus concentration at end
    
    # of day QUAL2E section 3.4.2 equation III-25
    
    xx <- 0.
    
    yy <- 0.
    
    zz <- 0.
    
    xx <- Theta(bc4,thbc4,wtmp) * orgpcon
    
    yy <- Theta(rs2,thrs2,wtmp) / (rchdep * 1000.)
    
    #rs2: benthos source rate for dissolved phosphorus ((m**2)*day)
    
    #in reach at 20 deg C
    
    #thrs2: temperature adjustment factor for local [-]
    
    zz <- ai2 * Theta(gra,thgra,wtmp) * algcon
    
    disolvp <- 0.
    
    disolvp <- solpcon + (xx + yy - zz) * tday
    
    if (disolvp < 1.e-6) {disolvp <- 0.}
    
    if (disolvp > dcoef * solpcon) {disolvp <- dcoef * solpcon} 
    
    # end phosphorus calculations
    
    
    Total_N<-0
    
    Total_P<-0
    

  
  } else{
  
  
    # all water quality variables set to zero when no flow
    
  algin <- 0.0
  
  chlin <- 0.0
  
  orgnin <- 0.0
  
  ammoin <- 0.0
  
  nitritin <- 0.0
  
  nitratin <- 0.0
  
  orgpin <- 0.0
  
  dispin <- 0.0
  
  cbodin <- 0.0
  
  disoxin <- 0.0
  
  algae <- 0.0
  
  chlora <- 0.0
  
  organicn <- 0.0
  
  ammonian <- 0.0
  
  nitriten <- 0.0
  
  nitraten <- 0.0
  
  organicp <- 0.0
  
  disolvp <- 0.0
  
  rch_cbod <- 0.0
  
  rch_dox <- 0.0
  
  soxy <- 0.0
  
  orgncon <- 0.0
  

  if (i_mo == 12 | i_mo == 1 | i_mo == 2) {
    
    wtmp <- tmp_win1 + tmp_win2 * tmpav
    
  } 
  
  if (i_mo == 3 | i_mo == 4 | i_mo == 5) {
    
    wtmp <- tmp_spr1 + tmp_spr2 * tmpav
    
  }  
  
  if (i_mo == 6 | i_mo == 7 | i_mo == 8) {
    
    wtmp <- tmp_sum1 + tmp_sum2 * tmpav
    
  }  
  
  if (i_mo == 9 | i_mo == 10 | i_mo == 11) {
    
    wtmp <- tmp_fal1 + tmp_fal2 * tmpav
    
  }
  
  water_temp<-wtmp # added by my own
  
  
  
  
  }
  
  
   
   
   output<-c(algae,chlora, organicn, ammonian, nitriten, nitraten,  organicp,
             disolvp, rch_cbod,rch_dox,water_temp,chlin, orgnin, ammoin, nitritin,
             nitratin,orgpin,dispin,cbodin,disoxin,soxy)
   
return(output)

 # output is concentration in mg/L  
   
  # Total N (org N + no3 + no2 + nh4 outs) kg
  # Total P (org P + sol p outs)  kg
   
   
   
} # end of function

