################################################################################

# Routing Water, Sediment, and Nutrients Through Ponds

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# References: 

# SWAT source codes

# SWAT 2009 theory

################################################################################



Pond <- function(watyld, sedyld, sanyld, silyld, clayld,
                   
                   sagyld,lagyld, psolyld, psedyld, orgnyld, no3yld, param, i_mo){ # function
  
  
  # param: parameters
  
  # pnd_fr: fraction of flow goes to pond [0-1]
  
  # pndsep: seepage from pond [m3] 
  
  # watyld: water contributing to channel from subbasin (NHD catchment) [m3] 
  
  # sedyld: sediment contributing to channel from subbasin (NHD catchment) [ton] 
  
  # orgnyld: organic Nitrogen [kg] 
  
  # psedyld: sediment Phosphorous [kg] 
  
  # no3yld: no3 [kg] 
  
  # psolyld: soluble Phosphorus [kg]
  
  # i_mo: Month
  
  
  
  pnd_fr   <- param$pnd_fr
  
  pnd_vol  <- param$pnd_vol
  
  pnd_sed  <- param$pnd_sed # sediment concentration in pond
  
  pnd_san  <- param$pnd_san
  
  pnd_sil  <- param$pnd_sil
  
  pnd_cla  <- param$pnd_cla
  
  pnd_sag  <- param$pnd_sag
  
  pnd_lag  <- param$pnd_lag
  
  pnd_psol <- param$pnd_psol
  
  pnd_psed <- param$pnd_psed
  
  pnd_orgn <- param$pnd_orgn
  
  pnd_no3  <- param$pnd_no3
  
  pnd_pvol <- param$pnd_pvol
  
  pnd_evol <- param$pnd_evol

  pnd_psa  <- param$pnd_psa

  pnd_esa  <- param$pnd_esa
  
  pnd_nsed <- param$pnd_nsed * 1e-6    # normal sediment concentration in
  
  #  pond [kg/L]  - 1e-6: covert mg/L to kg/L or [ton/m3]

  
  pnd_d50  <- param$pnd_d50    #[micrometer]

  sed_stl  <- param$sed_stl
    
  pnd_k <- param$pnd_k # hydraulic conductivity of soil at the place of pndland [mm/hr]
  
  psetlp <- c(param$psetlp / 365,param$psetlp / 365)  # m/year >> m/day
  
  nsetlp <- c(param$nsetlp / 365,param$nsetlp / 365)  # m/year >> m/day
  
  
  ipnd1<-1
  
  ipnd2<-1
  
    

   # calculate shape parameters for surface area equation

         #   pe_sa <- 0.

          #  pp_sa <- 0.

          #  pp_vo <- 0.

          #  pe_vo <- 0.

          #  pp_vo <- pnd_pvol

           # pe_vo <- pnd_evol

          #  pe_sa <- pnd_esa

          #  pp_sa <- pnd_psa

           # if ((pe_sa - pp_sa) > 0. & (pe_vo - pp_vo) > 0.) {

            #  lnvol <- 0.  
      
            #  lnvol <- log10(pe_vo) - log10(pp_vo)

            #  if (lnvol > 1.e-4) {

            #    bp2 <- (log10(pe_sa) - log10(pp_sa)) / lnvol

             # }else{

             #   bp2 <- (log10(pe_sa) - log10(pp_sa)) / 0.001

             # }

             # if (bp2 > .9) {

              #  bp2 <- .9

              #  bp1 <- (pnd_psa / pnd_pvol) ^ .9

            #  }else{

              #  bp1 <- (pnd_esa / pnd_evol) ^ bp2

            #  }

          #  }else{

            #  bp2 <- .9

            #  bp1 <- (pnd_psa / pnd_pvol) ^ .9

           # }



  
  if (pnd_fr > 0.) {  # r1
    
    # store initial values ----------------------------------------------------
    
    vol <- 0.  # volume of pndland at beginning of day [m3]
    
    sed <- 0.  # sediment concentration in pndland at beginning of day [kg/L]
    
    san <- 0.
    
    sil <- 0.
    
    cla <- 0.
    
    sag <- 0.
    
    lag <- 0.
    
    inised <- 0.
    
    finsed <- 0.
    
    setsed <- 0.
    
    remsetsed <- 0.
    
    
    
    vol <- pnd_vol  # volume of water in pndlands [m3]
    
    sed <- pnd_sed  # sediment concentration in pndland water [kg/L]
    
    san <- pnd_san
    
    sil <- pnd_sil
    
    cla <- pnd_cla
    
    sag <- pnd_sag
    
    lag <- pnd_lag
    
    
    
    
    
    # calculate water balance for day -----------------------------------------
    
    # recharge, precipitation and evaporation are accounted in the landscape process
    
     pndsa <- 0. # surface area of pndland on current day [ha]
    
    # pndsa <- bp1 * pnd_vol ^ bp2

     pndsa <- pnd_psa # [ha]
    
    # pndev <- 10. * evpnd * pet_day * pndsa
    
    pndsep <- pnd_k * pndsa * 240.  # seepage from pndland on day [m3] 
    
    # pndpcp <- percp_day * pndsa * 10.
    
    
    
    
    # calculate water flowing into pndland ---------------------------
    
    
    pndflwi <- watyld * pnd_fr 
    
    
    # pnd_fr: fraction of subbasin (NHD catchment) area that drains into pndland [0-1]
    
    
    wati <- watyld 
    
    
    wat <- watyld * (1. - pnd_fr) 
    
    
    pndloss <- wati - wat
    
    
    watyld <- watyld - pndloss # partition that goes directly to channel and not entering pond [m3]
    
    
    
    # sediment loading to pond  ------------------------------------
    
    pndsedi <- sedyld * pnd_fr # [ton]
    
    
    # pndsedi: sediment loading to pndland for day [ton]
    
    pndsani <- sanyld *  pnd_fr
    
    pndsili <- silyld *  pnd_fr
    
    pndclai <- clayld *  pnd_fr
    
    pndsagi <- sagyld *  pnd_fr
    
    pndlagi <- lagyld *  pnd_fr
    
    
    
    sedyld <- sedyld - sedyld * pnd_fr # partition of sediment that goes directly to channel and not entering pndland [m3]
    
    sanyld <- sanyld - sanyld * pnd_fr
    
    silyld <- silyld - silyld * pnd_fr
    
    clayld <- clayld - clayld * pnd_fr
    
    sagyld <- sagyld - sagyld * pnd_fr
    
    lagyld <- lagyld - lagyld * pnd_fr
    
    
    
    # --compute nitrogen and phosphorus levels in pond at beginning of day ----
    
    
    pnd_psol <- pnd_psol + psolyld * pnd_fr  # soluble P [kg]
    
    # pnd_psol: amount of soluble P in pond at beginning of day [kg]
    
    # psolyld: HRU 19 and 20 load for subbasin (NHD catchment) [kg]
    
    pnd_psed <- pnd_psed + psedyld * pnd_fr  # sediment P
    
    # pnd_psed: amount of  P attached to sediment in pond at
    
    #beginning of day [kg] <HRU 14 and 15 > 
    
    # psedyld: HRU 14 and 15 load for subbasin (NHD catchment) [kg]
    
    
    pnd_orgn <- pnd_orgn + orgnyld * pnd_fr  # organic N
    
    # pnd_orgn: amount of organic N in pond at beginning of day
    
    # orgnyld: HRU 13 load for subbasin (NHD catchment) [kg]
    
    pnd_no3 <- pnd_no3 + no3yld * pnd_fr  # NO3
    
    # pnd_no3: amount of nitrate in pond at beginning of day [kg]
    
    # no3yld: HRU 16, 17, and 18 load for subbasin (NHD catchment) [kg]
    
    
    
    
    
    
    # remove nutrients entering pond from HRU loadings --------------------
    
    xx <- 0.
    
    xx <- 1. - pnd_fr
    
    orgnyld <- orgnyld * xx  # this partition directly goes to stream without passing through pond
    
    no3yld <- no3yld * xx
    
    psedyld <- psedyld * xx
    
    psolyld <- psolyld * xx
    
    
    # new water volume for day ------------------------------------------------
    
    pndev <- 0  # accounted in landscape
    
    pndpcp <- 0 # accounted in landscape
    
    pnd_vol <- pnd_vol - pndsep - pndev + pndpcp + pndflwi
    
    
    
    if (pnd_vol <= 0) { # r2
      
      # check for volume deficit in pndland
      
      # reduce seepage so that the pndland volume is zero
      
      pndsep <- pndsep + pnd_vol
      
      pnd_vol <- 0.
      
      # if seepage is less than the volume deficit, take the 
      
      # remainder from evaporation
      
      if (pndsep < 0.) {
        
        pndev <- pndev + pndsep
        
        pndsep <- 0.
        
      }
      
      pnd_sed <- 0.
      
      pnd_san <- 0.
      
      pnd_sil <- 0.
      
      pnd_cla <- 0.
      
      pnd_sag <- 0.
      
      pnd_lag <- 0.
      
      pnd_psol <- 0.
      
      pnd_psed <- 0.
      
      pnd_orgn <- 0.
      
      pnd_no3 <- 0.
      
      pnd_chla <- 0.
      
      pnd_seci <- 0.
      
      
    } else {
      
      
      
      # compute outflow if pond water volume > 0 -----------------------------
      
      
      if (pnd_vol <= pnd_pvol) {
        
        pndflwo <- 0.
        
      } else {
        
        if (pnd_vol <= pnd_evol) {
          
          pndflwo <- (pnd_vol - pnd_pvol)           
          
        } else {
          
          pndflwo <- pnd_vol - pnd_evol
          
          
        }
        
      }
      
      
      watyld <- watyld + pndflwo # total water goes to channel [m3]
      
  
      


      
      # compute new sediment concentration
      
     
     
      pnd_d50mm <- pnd_d50 / 1000.        # micrometers to millimeters # readpnd.f
      
      velsetlp <- 24. * 411. * pnd_d50mm ^ 2.  # readpnd.f
      
     # velsetlpnd:  fall velocity of sediment
      
      
      if (pndsedi < 1.e-6) {pndsedi <- 0.}
      
      if (pndsa == 0.) {pndsa <- 0.001}    # MJW added line of code 040811
      
      velofl <- (pndflwo / pndsa) / 10000.
      
      if (velofl > 1.e-6) {
      
      trappnd <- velsetlp / velofl
      
      if (trappnd > 1.) {trappnd <- 1.}
      
      susp <- 1. - trappnd
      
      } else {
        
        susp <- 1.
      
      }
      
      
      
      
      
      if (pnd_vol > 0.1) {
      
      pnd_sed <- (sed * vol + susp * pndsedi) / pnd_vol
      
      pnd_san <- (san * vol + pndsani) / pnd_vol
      
      pnd_sil <- (sil * vol + pndsili) / pnd_vol
      
      pnd_cla <- (cla * vol + pndclai) / pnd_vol
      
      pnd_sag <- (sag * vol + pndsagi) / pnd_vol
      
      pnd_lag <- (lag * vol + pndlagi) / pnd_vol
      
    } 
      
      
      
      # compute final pond volume
      
      pnd_vol <- pnd_vol - pndflwo
      
      if (pnd_vol < 0.) {
      
      pndflwo <- pndflwo + pnd_vol
      
      pnd_vol <- 0.
      
      }
      
      
     
      
      # compute change in sediment concentration due to settling
      
      if (sed_stl < 1.e-6) {sed_stl <- 0.0}
      
      if (pnd_sed > pnd_nsed) { # r1
      
      inised <- pnd_sed
      
      pnd_sed <- (pnd_sed - pnd_nsed) * sed_stl + pnd_nsed
      
      finsed <- pnd_sed
      
      setsed <- inised - finsed
      
      if (pnd_lag >= setsed) {
      
      pnd_lag <- pnd_lag - setsed
      
      remsetsed <- 0.
      
     } else { # r2
        
        remsetsed <- setsed - pnd_lag
      
      pnd_lag <- 0.
      
      if (pnd_san >= remsetsed) {
      
      pnd_san <- pnd_san - remsetsed
      
      remsetsed <- 0.
      
     } else { # r3
        
        remsetsed <- remsetsed - pnd_san
      
      pnd_san <- 0.
      
      if (pnd_sag >= remsetsed) {
      
      pnd_sag <- pnd_sag - remsetsed
      
      remsetsed <- 0.
      
      } else { # r4
        
        remsetsed <- remsetsed - pnd_sag
      
      pnd_sag <- 0.
      
      if (pnd_sil >= remsetsed) {
      
      pnd_sil <- pnd_sil - remsetsed
      
      remsetsed <- 0.
      
     } else { # r5
        
        remsetsed <- remsetsed - pnd_sil
      
      pnd_sil <- 0.
      
      if (pnd_cla >= remsetsed) {
      
      pnd_cla <- pnd_cla - remsetsed
      
      remsetsed <- 0.
      
     } else {  # r6
        
        remsetsed <- remsetsed - pnd_cla
      
      pnd_cla <- 0.
      
      
    } # r6
      
    } # r5
      
    } # r4
      
    } # r3
      
    } # r2
      
    }  # r1
      
      
      

      
      # compute sediment leaving pond ----------------------------------------
      
      
      pndsedo <- pnd_sed * pndflwo
      
      pndsano <- pnd_san * pndflwo
      
      pndsilo <- pnd_sil * pndflwo
      
      pndclao <- pnd_cla * pndflwo
      
      pndsago <- pnd_sag * pndflwo
      
      pndlago <- pnd_lag * pndflwo
      
      
      sedyld <- sedyld + pndsedo # total amount of sediment contribute to channel [ton]
      
      sanyld <- sanyld + pndsano
      
      silyld <- silyld + pndsilo
      
      clayld <- clayld + pndclao
      
      sagyld <- sagyld + pndsago
      
      lagyld <- lagyld + pndlago
      
      
      
    
      # net change in amount of sediment in pndland for day
      
      pndsedc <- vol * sed + pndsedi - pndsedo - pnd_sed * pnd_vol
      
      
      
    
      
      
      # determine settling rate for nutrients -----------------------------------
      
      
      if (i_mo >= ipnd1 & i_mo <= ipnd2) {
        
        iseas <- 1
        
      } else {
        
        iseas <- 2
        
      }
      
      phosk <- 0.
      
      nitrok <- 0.
      
      phosk <- psetlp[iseas] * pndsa * 10000. / pnd_vol
      
      phosk <- min(phosk, 1.)
      
      nitrok <- nsetlp[iseas] * pndsa * 10000. / pnd_vol
      
      nitrok <- min(nitrok, 1.)
      
      
      
      # remove nutrients by settling --------------------------------------------
      
      
      pnd_psol <- pnd_psol * (1. - phosk)
      
      pnd_psed <- pnd_psed * (1. - phosk)
      
      pnd_orgn <- pnd_orgn * (1. - nitrok)
      
      pnd_no3 <- pnd_no3 * (1. - nitrok)
      
      
      
      if (pnd_orgn < 1.e-6) {pnd_orgn <- 0.0}
      
      if (pnd_no3 < 1.e-6) {pnd_no3 <- 0.0}
      
      if (pnd_psed < 1.e-6) {pnd_psed <- 0.0}
      
      if (pnd_psol < 1.e-6) {pnd_psol <- 0.0}
      
      
      
      # compute nutrients leaving pond ---------------------------------------
      
      
      yy <- 0.
      
      yy <- pndflwo / (pnd_vol + pndflwo)
      
      orgnyld <- orgnyld + pnd_orgn * yy   # total organic nitrogen contributes to channel
      
      no3yld <- no3yld + pnd_no3 * yy 
      
      psedyld <- psedyld + pnd_psed * yy 
      
      psolyld <- psolyld + pnd_psol * yy 
      
      
      # update nutrient pools in pond ---------------------------------------
      
      
      
      pnd_orgn <- pnd_orgn * (1. - yy)  # remaining organic N in pond [kg]
      
      pnd_no3 <- pnd_no3 * (1. - yy)
      
      pnd_psed <- pnd_psed * (1. - yy)
      
      pnd_psol <- pnd_psol * (1. - yy)
      
      # pnd_chla <- pnd_chla * (1. - yy)
      
      
      
      
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
      
      
      
      output<-c(watyld, sedyld, sanyld, silyld, clayld, sagyld, lagyld, orgnyld, no3yld, psedyld, psolyld,
                 
                 pnd_vol, pnd_sed,pnd_san, pnd_sil, pnd_cla, pnd_sag, pnd_lag, pnd_psol,
                 
                 pnd_psed, pnd_orgn, pnd_no3)
      
      
      
      
      return(output)
      
      
      
    } # function
    
    