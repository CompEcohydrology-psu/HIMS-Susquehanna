################################################################################

# Thios function read parameters from a SWAT model

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

################################################################################


Paramread <- function(nsub,dir,res_option,res_dir){

# set library pasth here ...

library(readr)

  # dir: Nabsed folder directory
    
  
  Qman <- function(x1,x2,x3,x4){ # internal function
    
    
    # convert to discharge
    
    # x1: area
    
    # x2: hydraulic radius 
    
    # x3: Manning roughness
    
    # x4: slope
    
    r_qman <- 0.
    
    r_qman <- x1 * x2 ^ 0.6666 * sqrt(x4) / x3
    
    return(r_qman)
    
  }

  
  
  # channel and reservoir routing parameters
  
  
  
path<-dir  # directory of Nabsed folder / all SWAT files
  

nsub.a <- nsub + 1  


pattern <- paste(substr('000000000',1,5-nchar(nsub)),nsub,'0000.rte',sep="")


rte<-list.files(path=path,pattern=pattern,full.names = TRUE)


bsn <- list.files(path=path,pattern='.bsn',full.names = TRUE)


wwq <- list.files(path=path,pattern='.wwq',full.names = TRUE)

pattern <- paste(substr('000000000',1,5-nchar(nsub)),nsub,'0000.swq',sep="")

swq <- list.files(path=path,pattern=pattern,full.names = TRUE)

pattern <- paste(substr('000000000',1,5-nchar(nsub)),nsub,'0001.gw',sep="")

gw<-list.files(path=path,pattern=pattern,full.names = TRUE)

pattern <- paste(substr('000000000',1,5-nchar(nsub)),nsub,'0000.sub',sep="")

sub<-list.files(path=path,pattern=pattern,full.names = TRUE)

if(res_option==1){

pattern <- paste(substr('000000000',1,5-nchar(nsub)),nsub,'0000.res',sep="")

res<-list.files(path=res_dir,pattern=pattern,full.names = TRUE)

pattern <- paste(substr('000000000',1,5-nchar(nsub)),nsub,'0000.lwq',sep="")

lwq <- list.files(path=res_dir,pattern=pattern,full.names = TRUE)

}

 
fileConn <- file(rte)

tx<-readLines(fileConn)


  ch_w2 <- parse_number(substr( tx[2],1,17))  # channel bankfull top width
  
  ch_d <- parse_number(substr( tx[3],1,17))   # channel bankfull depth
  
  ch_s2 <- parse_number(substr( tx[4],1,17))  # channel slope
  
  ch_l2 <- parse_number(substr( tx[5],1,17))  # channel length [km]
  
  ch_n2 <- parse_number(substr( tx[6],1,17))  # Manning roughness
  
  ch_k2 <- parse_number(substr( tx[7],1,17))  # Manning roughness Effective 
  
  ch_cov1 <-parse_number(substr( tx[8],1,17))
  
  ch_cov2 <-parse_number(substr( tx[9],1,17))
  
  ch_wdr <- parse_number(substr( tx[10],1,17)) # Channel width:depth ratio
  
  alpha_bnk <- parse_number(substr( tx[11],1,17)) # Baseflow alpha factor for
  
  chside <- parse_number(substr( tx[15],1,17)) # side slope
  
  ch_bnk_bd <- parse_number(substr( tx[16],1,17))
  
  ch_bed_bd <- parse_number(substr( tx[17],1,17))
  
  ch_bnk_kd <-  parse_number(substr( tx[18],1,17))
    
  ch_bed_kd <- parse_number(substr( tx[19],1,17))
  
  ch_bnk_d50<- parse_number(substr( tx[20],1,17)) 
  
  ch_bed_d50<- parse_number(substr( tx[21],1,17)) 
  
  tc_bnk <- parse_number(substr( tx[22],1,17)) 
  
  tc_bed <- parse_number(substr( tx[23],1,17)) 
  
  close(fileConn)
  

    
    
    fileConn <- file(bsn)
    
    tx<-readLines(fileConn)
    
    msk_co1 <- parse_number(substr( tx[59],1,17))  # calib coef
    
    msk_co2 <-  parse_number(substr( tx[60],1,17))   # calib coef
    
    msk_x <- parse_number(substr( tx[61],1,17))  # Muskingum X parameter
   
    evrch <- parse_number(substr( tx[66],1,17))  # Reach evaporation adjustment
    
    #factor
    
    prf <- parse_number(substr( tx[22],1,17)) #Peak rate adjustment factor for 
    
    #sediment routing
    
    spcon <- parse_number(substr( tx[23],1,17)) #Linear parameter for 
    
    #calculating the maximum amount of sediment
    
    spexp <- parse_number(substr( tx[24],1,17)) # Exponent parameter for
    
    #calculating sediment reentrained in channel sediment routing
      
    close(fileConn)
      
      
    fileConn <- file(wwq)
    
    tx<-readLines(fileConn)
    
    igropt <- parse_number(substr( tx[3],1,17))
    
    ai0 <- parse_number(substr( tx[4],1,17))

    ai1 <- parse_number(substr( tx[5],1,17))
    
    ai2 <- parse_number(substr( tx[6],1,17))
    
    ai3 <- parse_number(substr( tx[7],1,17))
    
    ai4 <- parse_number(substr( tx[8],1,17))
    
    ai5 <- parse_number(substr( tx[9],1,17))
    
    ai6 <- parse_number(substr( tx[10],1,17))  
    
    mumax <- parse_number(substr( tx[11],1,17))
    
    rhoq <- parse_number(substr( tx[12],1,17))
    
    tfact <- parse_number(substr( tx[13],1,17))
    
    k_l <- parse_number(substr( tx[14],1,17))    #Half-saturation coefficient
    
    # for light [kJ/(m2·min)]
    
    k_n <- parse_number(substr( tx[15],1,17))  # Michaelis-Menton 
    
    # half-saturation constant for nitrogen [mg N/lL]
    
    k_p <- parse_number(substr( tx[16],1,17))  # Michaelis-Menton 
    
    # half-saturation constant for phosphorus [mg P/l]
    
    lambda0 <- parse_number(substr( tx[17],1,17)) # Non-algal portion of the
    
    # light extinction coefficient [m-1]
    
    lambda1 <-  parse_number(substr( tx[18],1,17)) # Linear algal
    
    # self-shading coefficient [m-1·(µg chla/l)-1)]
    
    lambda2 <-  parse_number(substr( tx[19],1,17)) #Nonlinear algal
    
    # self-shading coefficient [m-1·(µg chla/l)-2]
    
    p_n <- parse_number(substr( tx[20],1,17)) # Algal preference factor for ammonia
    
    chla_subco <- parse_number(substr( tx[21],1,17))
    
    close(fileConn)
    
    
    fileConn <- file(swq)
    
    tx <- readLines(fileConn)
    
    rs1 <- parse_number(substr( tx[3],1,17))
    
    rs2 <- parse_number(substr( tx[4],1,17))
    
    rs3 <- parse_number(substr( tx[5],1,17))
    
    rs4 <- parse_number(substr( tx[6],1,17))
    
    rs5 <- parse_number(substr( tx[7],1,17))
    
    rs6 <- parse_number(substr( tx[8],1,17))
    
    rs7 <- parse_number(substr( tx[9],1,17))
    
    rk1 <- parse_number(substr( tx[10],1,17))
    
    rk2 <- parse_number(substr( tx[11],1,17))
    
    rk3 <- parse_number(substr( tx[12],1,17))
    
    rk4 <- parse_number(substr( tx[13],1,17))
    
    rk5 <- parse_number(substr( tx[14],1,17))
    
    rk6 <- parse_number(substr( tx[15],1,17))
    
    bc1 <- parse_number(substr( tx[16],1,17))
    
    bc2 <- parse_number(substr( tx[17],1,17))
    
    bc3 <- parse_number(substr( tx[18],1,17))
    
    bc4 <- parse_number(substr( tx[19],1,17))
    
    chpst_rea <- parse_number(substr( tx[21],1,17))
      
    chpst_vol <- parse_number(substr( tx[22],1,17))
      
    chpst_koc <- parse_number(substr( tx[23],1,17))
      
    chpst_stl <- parse_number(substr( tx[24],1,17))
      
    chpst_rsp <- parse_number(substr( tx[25],1,17))
      
    chpst_mix <- parse_number(substr( tx[26],1,17))
        
    sedpst_conc <-parse_number(substr( tx[27],1,17))
      
    sedpst_rea <-parse_number(substr( tx[28],1,17))
    
    sedpst_bry <-parse_number(substr( tx[29],1,17))
    
    sedpst_act <-parse_number(substr( tx[30],1,17))
    
    close(fileConn)
    
    
    fileConn <- file(gw)
    
    tx <- readLines(fileConn)
    
    alpha_bf <- parse_number(substr( tx[5],1,17))
    
    gw_revap <- parse_number(substr( tx[7],1,17))
    
    close(fileConn)
    
    
    fileConn <- file(sub)
    
    tx <- readLines(fileConn)
    
    lat <- parse_number(substr( tx[5],1,17)) # latitude
    
    close(fileConn)
    
    
    if(res_option==1){

    
    # read reservoir file

     fileConn <- file(res)
    
     tx <- readLines(fileConn)
     
     mores <-parse_number(substr( tx[3],1,17))
    
     iyres <- parse_number(substr( tx[4],1,17))
    
     res_esa <- parse_number(substr( tx[5],1,17))
    
     res_evol <- parse_number(substr( tx[6],1,17))
    
     res_psa <- parse_number(substr( tx[7],1,17))
    
     res_pvol <- parse_number(substr( tx[8],1,17))
    
     res_vol <- parse_number(substr( tx[9],1,17))
    
     res_sed <- parse_number(substr( tx[10],1,17))
    
     res_nsed <- parse_number(substr( tx[11],1,17))
    
     res_d50 <- parse_number(substr( tx[12],1,17))
    
     res_k <- parse_number(substr( tx[13],1,17))
    
     res_rr <- parse_number(substr( tx[23],1,17))
     
     ndtargr <- parse_number(substr( tx[27],1,17))
    
     evrsv <- parse_number(substr( tx[38],1,17))
    
     close(fileConn)
    

     fileConn <- file(lwq)
    
     tx <- readLines(fileConn)
    
     ires1 <- parse_number(substr( tx[3],1,17))
    
     ires2 <- parse_number(substr( tx[4],1,17))
    
     psetlr1 <- parse_number(substr( tx[5],1,17))
    
     psetlr2 <- parse_number(substr( tx[6],1,17))
    
     nsetlr1 <- parse_number(substr( tx[7],1,17))
    
     nsetlr2 <- parse_number(substr( tx[8],1,17))
    
     chlar <- parse_number(substr( tx[9],1,17))
    
     seccir <- parse_number(substr( tx[10],1,17))
    
     res_orgp <- parse_number(substr( tx[11],1,17))
    
     res_solp <- parse_number(substr( tx[12],1,17))
    
     res_orgn <- parse_number(substr( tx[13],1,17))
    
     res_no3 <- parse_number(substr( tx[14],1,17))
    
     res_nh3 <- parse_number(substr( tx[15],1,17))
    
     res_no2 <- parse_number(substr( tx[16],1,17)) # (mg/L)
    
    
     close(fileConn)

  }

    ###############  parameter checker ##########
    
    
    if(alpha_bnk <= 0 ) {alpha_bnk <- alpha_bf}
    
    ch_revap <- gw_revap
    
    
    
    
 # flow
    
    if (ch_s2 <= 0.) {ch_s2 <- .0001}
    
    if (ch_n2 <= 0.01) {ch_n2 <- .01}
    
    if (ch_n2 >= 0.70) {ch_n2 <- 0.70}
    
    if (ch_l2 <= 0.) {ch_l2 <- .0010}
    
    if (ch_wdr <= 0.) {ch_wdr <- 3.5}
    
    if (chside <= 1.e-6) {chside <- 2.0}
    
    
    
    aa <- 1.
    
    b <- 0.
    
    d <- 0.
    
    #   If side slope is not set in .rte file then assume this default
    
    #   If it is main reach default side slope to 2:1 if it is a waterway default
    
    # to 8:1
    
    if (chside <= 1.e-6) {
      
      chsslope <- 2.
      
    }else{
      
      chsslope <- chside
      
    }
    
    fps <- 4.
    
    d <- ch_d
    
    b <- ch_w2 - 2. * d * chsslope
    
    
    #    check if bottom width (b) is < 0
    
    if (b <= 0.) {
      
      b <- 0.
      
      chsslope <- 0.
      
      b <- .5 * ch_w2
      
      chsslope <- (ch_w2 - b) / (2. * d)
      
    }
    
    phi_6 <- b
    
    phi_7 <- d
    
    
    #    compute flow and travel time at bankfull depth
    
    p <- 0.
    
    a <- 0.
    
    rh <- 0.
    
    tt2 <- 0.
    
    p <- b + 2. * d * sqrt(chsslope * chsslope + 1.)
    
    a <- b * d + chsslope * d * d
    
    rh <- a / p
    
    phi_1 <- a
    
    phi_5 <- Qman(a, rh, ch_n2, ch_s2)
    
    phi_8 <- Qman(aa, rh, ch_n2, ch_s2)
    
    phi_9 <- phi_8 * 5. / 3.
    
    phi_10 <- ch_l2 / phi_9 / 3.6
    
    tt2 <- ch_l2 * a / phi_5
    
    
    #    compute flow and travel time at 0.1 bankfull depth
    
    a <- 0.
    
    d <- 0.
    
    p <- 0.
    
    rh <- 0.
    
    qq1 <- 0.
    
    tt1 <- 0.
    
    d <- 0.1 * ch_d
    
    p <- b + 2. * d * sqrt(chsslope * chsslope + 1.)
    
    a <- b * d + chsslope * d * d
    
    rh <- a / p
    
    qq1 <- Qman(a, rh, ch_n2, ch_s2)
    
    tt1 <- ch_l2 * a / qq1
    
    phi_11 <- Qman(aa, rh, ch_n2, ch_s2)
    
    phi_12 <- phi_11 * 5. / 3.
    
    phi_13 <- ch_l2 / phi_12 / 3.6
    
    
    
    # sediment
    
    if (tc_bnk <= 0.) {tc_bnk <- 0.} # Critical shear stress (N.m^2)
    
    if (tc_bed <= 0.) {tc_bed <- 0.} # Critical shear stress (N.m^2)
    
  
   
    
    
    
    
    
    ch_eqn <-0 # Physic-based Bagnold
    
    if (ch_eqn <= 0) {
    
    if (ch_cov1 <= 0.0) {ch_cov1 <- 0.0}
    
    if (ch_cov2 <= 0.0) {ch_cov2 <- 0.0}
    
    if (ch_cov1 >= 1.0) {ch_cov1 <- 1.0}
    
    if (ch_cov2 >= 1.0) {ch_cov2 <- 1.0}
    
    }else{
      
      if (ch_cov1 <= 0.0) {ch_cov1 <- 1.0}
    
    if (ch_cov2 <= 0.0) {ch_cov2 <- 1.0}
    
    if (ch_cov1 >= 25.) {ch_cov1 <- 25.}
    
    if (ch_cov2 >= 25.) {ch_cov2 <- 25.}
    
}
    
    
   
    
    
    if(ch_bnk_d50 <= 1.e-6) {ch_bnk_d50 <- 50} # Bank material is assumed
    
    #to be silt type partcile if not given
      
      if (ch_bnk_d50 > 10000) {ch_bnk_d50 <- 10000.}
      
    
    bnksize <- ch_bnk_d50/1000. # convert to milimeter
    
    # Channel sediment particle size distribution
    
    #    Clayey bank
    
    if (bnksize <= 0.005) {
      
      ch_bnk_cla <- 0.65
      
      ch_bnk_sil <- 0.15
      
      ch_bnk_san <- 0.15
      
      ch_bnk_gra <- 0.05
      
    }
    
    #    Silty bank
    
    if (bnksize > 0.005 & bnksize <= 0.05) {
      
      ch_bnk_sil <- 0.65
      
      ch_bnk_cla <- 0.15
      
      ch_bnk_san <- 0.15
      
      ch_bnk_gra <- 0.05
      
    }
    
    #    Sandy bank
    
    if (bnksize > 0.05 & bnksize <= 2.) {
      
      ch_bnk_san <- 0.65
      
      ch_bnk_sil <- 0.15
      
      ch_bnk_cla <- 0.15
      
      ch_bnk_gra <- 0.05
      
    }
    
    #    Gravel bank
    
    if (bnksize > 2.) {
      
      ch_bnk_gra <- 0.65
      
      ch_bnk_san <- 0.15
      
      ch_bnk_sil <- 0.15
      
      ch_bnk_cla <- 0.05
      
    }
    
    
    # Channel sediment particle size distribution
    
    if(ch_bed_d50 <= 1.e-6) {ch_bed_d50 <- 500} # Bed material is assumed 
    
    #to be sand type partcile if not given.
    
    if (ch_bed_d50 > 10000) {ch_bed_d50 <- 10000.}
    
    #    Clayey bed
    
    bedsize <- ch_bed_d50 / 1000.  # Units conversion Micrometer to Millimeters
    
    if (bedsize <= 0.005) {
      
      ch_bed_cla <- 0.65
      
      ch_bed_sil <- 0.15
      
      ch_bed_san <- 0.15
      
      ch_bed_gra <- 0.05
      
    }
    
    #    Silty bed
    
    if (bedsize > 0.005 & bedsize <= 0.05) {
      
      ch_bed_sil <- 0.65
      
      ch_bed_cla <- 0.15
      
      ch_bed_san <- 0.15
      
      ch_bed_gra <- 0.05
      
    }
    
    #    Sandy bed
    
    if (bedsize > 0.05 & bedsize <= 2.) {
      
      ch_bed_san <- 0.65
      
      ch_bed_sil <- 0.15
      
      ch_bed_cla <- 0.15
      
      ch_bed_gra <- 0.05
      
    }
    
    #    Gravel bed
    
    if (bedsize > 2.) {
      
      ch_bed_gra <- 0.65
      
      ch_bed_san <- 0.15
      
      ch_bed_sil <- 0.15
      
      ch_bed_cla <- 0.05
      
    }
    
    
    #    Bulk density of channel bank sediment 
    
    if (ch_bnk_bd <= 1.e-6) {ch_bnk_bd <- 1.40}   # Silty loam bank
    
    #    Bulk density of channel bed sediment
    
    if (ch_bed_bd <= 1.e-6) {ch_bed_bd <- 1.50}  # Sandy loam bed
    
    
    #    An estimate of Critical shear stress if it is not given (N/m^2)
    
    #	Critical shear stress based on silt and clay %
    
    #	Critical Shear Stress based on Julian and Torres (2005)
    
    #    Units of critical shear stress (N/m^2)
    
    
    SC <- 0.
    
    if  (tc_bnk <= 1.e-6) {
      
      SC <- (ch_bnk_sil + ch_bnk_cla) * 100.
      
      tc_bnk <- (0.1 + (0.1779*SC) + (0.0028*(SC)^2)          
                 - ((2.34e-05)*(SC)^3)) * ch_cov1
    }
    
    if  (tc_bed <= 1.e-6) {
      
      SC <- (ch_bed_sil + ch_bed_cla) * 100.
      
      tc_bed <- (0.1 + (0.1779*SC) + (0.0028*(SC)^2)          
                 - ((2.34e-05)*(SC)^3)) * ch_cov2
    }
    
    
    #  An estimate of channel bank erodibility coefficient from jet test if it is
    
    # not available
    
    #  Units of kd is (cm^3/N/s)
    
    #  Base on Hanson and Simon, 2001
    
    if (ch_bnk_kd <= 1.e-6) {
      
      if (tc_bnk <= 1.e-6) {
        
        ch_bnk_kd <- 0.2
        
      }else{
        
        ch_bnk_kd <- 0.2 / sqrt(tc_bnk)
        
      }
      
    }
    
    
    #  An estimate of channel bed erodibility coefficient from jet test if it is
    
    # not available
    
    #  Units of kd is (cm^3/N/s)
    
    #  Base on Hanson and Simon, 2001
    
    if (ch_bed_kd <= 1.e-6) {
      
      if (tc_bed <= 1.e-6) {
        
        ch_bed_kd <- 0.2
        
      }else{
        
        ch_bed_kd <- 0.2 / sqrt(tc_bed)
        
      }
      
    }
    
    

    
    #    set default values for undefined parameters
    
   
    
    igropt <- 2
    
    if (ai0 <= 0.) {ai0 <- 50.}
    
    if (ai1 <= 0.) {ai1 <- 0.08}
    
    if (ai2 <= 0.) {ai2 <- 0.015}
    
    if (ai3 <= 0.) {ai3 <- 1.60}
    
    if (ai4 <= 0.) {ai4 <- 2.0}
    
    if (ai5 <= 0.) {ai5 <- 3.5}
    
    if (ai6 <= 0.) {ai6 <- 1.07}
    
    if (mumax <= 0.) {mumax <- 2.0}
    
    if (rhoq <= 0.) {rhoq <- 2.5}      # previous 0.3
    
    if (tfact <= 0.) {tfact <- 0.3}
    
    if (k_l <= 0.) {k_l <- 0.75}
    
    if (k_n <= 0.) {k_n <- 0.02}
    
    if (k_p <= 0.) {k_p <- 0.025}
    
    if (lambda0 <= 0.) {lambda0 <- 1.0}
    
    if (lambda1 <= 0.) {lambda1 <- 0.03}
    
    if (lambda2 <= 0.) {lambda2 <- 0.054}
    
    if (p_n <= 0.) {p_n <- 0.5}
    
    if (chla_subco <= 0.) {chla_subco <- 40.0} 
    
    
    k_l <- k_l * 1.e-3 * 60. # convert unit
    
    
    
    #    set default values for undefined parameters
    
    if (rs1 <= 0.) {rs1 <- 1.0}
    
    if (rs2 <= 0.) {rs2 <- 0.05}
    
    if (rs3 <= 0.) {rs3 <- 0.5}
    
    if (rs4 <= 0.) {rs4 <- 0.05}
    
    if (rs5 <= 0.) {rs5 <- 0.05}
    
    if (rs6 <= 0.) {rs6 <- 2.5}
    
    if (rs7 <= 0.) {rs7 <- 2.5}
    
    if (rk1 <= 0.) {rk1 <- 1.71}
    
    if (rk2 <= 0.) {rk2 <- 1.0}    # previous 50.0
    
    if (rk4 <= 0.) {rk4 <- 2.0}
    
    if (rk5 <= 0.) {rk5 <- 2.0}
    
    if (rk6 <= 0.) {rk6 <- 1.71}
    
    if (bc1 <= 0.) {bc1 <- 0.55} 
    
    if (bc2 <= 0.) {bc2 <- 1.1}
    
    if (bc3 <= 0.) {bc3 <- 0.21}
    
    if (bc4 <= 0.) {bc4 <- 0.35}
    
    
    
    
    
    if(res_option==1){

    
   #    set default values for reservoir
    
   
    
    if (ndtargr <= 0) {ndtargr <- 15}
    
    if (res_d50 <= 0) {res_d50 <- 10.}
    
    if (res_pvol + res_evol > 0.) {
    
    if (res_pvol <= 0) {res_pvol <- 0.9 * res_evol}
    
    }else{
      
      if (res_pvol <= 0) {res_pvol <- 60000.0}
    
    }
    
    if (res_evol <= 0.0) {res_evol <- 1.11 * res_pvol}  
    
    if (res_psa <= 0.0) {res_psa <- 0.08 * res_pvol}
    
    if (res_esa <= 0.0) {res_esa <- 1.5 * res_psa} 
    
    if (res_vol < 0.0) {res_vol <- 0.0}
    
    if (evrsv <= 0.) {evrsv <- 0.6}
    

    
    #   convert units for reservoirs
    
     res_evol <- res_evol * 10000.                # 10**4 m**3 => m**3
     
     res_pvol <- res_pvol * 10000.          # 10**4 m**3 => m**3
     
     res_vol <- res_vol * 10000.            # 10**4 m**3 => m**3
     
     res_rr <- res_rr * 86400.              # m**3/s => m**3/day
     
     res_sed <- res_sed * 1.e-6              # mg/L => ton/m^3
     
     res_d50mm <- res_d50 / 1000.                  # micrometers to millimeters
     
     res_san <- res_sed * 0. 
     
     res_sil <- res_sed * 1. 
     
     res_cla <- res_sed * 0. 
     
     res_sag <- res_sed * 0. 
     
     res_lag <- res_sed * 0. 
     
     res_gra <- 0.
     
         
     velsetlr <- 24. * 411. * res_d50mm ^ 2. # fall velocity of sediment
  
    
    
    #    calculate shape parameters for surface area equation
    
    resdif <- 0.

    resdif <- res_evol - res_pvol

    if ((res_esa - res_psa) > 0. & resdif > 0.) {

      lnvol <- 0.

      lnvol <- log10(res_evol) - log10(res_pvol)

      if (lnvol > 1.e-4) {

        br2 <- (log10(res_esa) - log10(res_psa)) / lnvol

      }else{

        br2 <- (log10(res_esa) - log10(res_psa)) / 0.001

      }

      if (br2 > 0.9) {

        br2 <- 0.9

        br1 <- res_psa / (res_pvol ^ 0.9)

      }else{

        br1 <- res_esa /(res_evol ^ br2)

      }

    } else{

      br2 <- 0.9

      br1 <- res_psa /(res_pvol ^ 0.9)

    }


    # calculate sediment settling rate
  
    sed_stlr <- exp(-.184 * res_d50)
    
    
    
    
   # set default values for parameters (reservoir water quality)
    
    if (chlar <= 1.e-6) {chlar <- 1.}
    
    if (seccir <= 1.e-6) {seccir <- 1.}
    
   # if (theta_n <= 0.) {theta_n <- 1.08}
    
   # if (theta_p <= 0.) {theta_p <- 1.08}
    
   # if (con_nirr <= 0.) {con_nirr <- 0.0}
    
   # if (con_pirr <= 0.) {con_pirr <- 0.0} 
    
    
    
       
    
   
    pars<-list(ch_w2=ch_w2,ch_d=ch_d,ch_s2=ch_s2,ch_l2=ch_l2,ch_n2=ch_n2,
               
               chside=chside,phi_1=phi_1, phi_6=phi_6, phi_10=phi_10,
               
               phi_13=phi_13,ch_k2=ch_k2,ch_cov1=ch_cov1,ch_cov2=ch_cov2, 
               
               ch_wdr= ch_wdr,msk_co1=msk_co1,msk_co2=msk_co2,
               
               msk_x=msk_x,evrch=evrch,alpha_bnk=alpha_bnk, ch_bnk_bd=ch_bnk_bd,
               
               ch_bed_bd=ch_bed_bd,tc_bnk=tc_bnk,tc_bed=tc_bed,ch_bnk_kd=ch_bnk_kd,
               
               ch_bed_kd=ch_bed_kd,ch_bnk_d50=ch_bnk_d50,ch_bed_d50=ch_bed_d50,
               
               prf=prf,spcon=spcon,spexp=spexp, ai0=ai0, ai1=ai1, ai2=ai2,
               
               ai3=ai3, ai4=ai4, ai5=ai5, ai6=ai6, mumax=mumax, rhoq=rhoq,
               
               tfact= tfact,k_l= k_l, k_n=k_n, k_p=k_p, lambda0=lambda0,
               
               lambda1=lambda1, lambda2=lambda2, p_n=p_n, 
               
               chla_subco=chla_subco, rs1= rs1, rs2= rs2, rs3= rs3, rs4= rs4,
               
               rs5= rs5,rs6= rs6,rs7= rs7,rk1=rk1,rk2=rk2,rk3=rk3,rk4=rk4,
               
               rk5=rk5,rk6=rk6,bc1=bc1,bc2=bc2,bc3=bc3,bc4=bc4,
               
               chpst_rea=chpst_rea,chpst_vol=chpst_vol,chpst_koc=chpst_koc,
               
               chpst_stl=chpst_stl,chpst_rsp=chpst_rsp, chpst_mix=chpst_mix,
               
               sedpst_conc=sedpst_conc,sedpst_rea=sedpst_rea, 
               
               sedpst_bry=sedpst_bry,sedpst_act=sedpst_act,
               
               bankst=0,rchstor=0,ch_revap=ch_revap,depch=0,depsanch=0,
               
               depsilch=0,depclach=0,depsagch=0,deplagch=0,
               
               depgrach=0,depfp=0,depsilfp=0,depclafp=0,ch_bnk_cla=ch_bnk_cla,
               
               ch_bnk_sil=ch_bnk_sil,ch_bnk_san=ch_bnk_san,ch_bnk_gra=ch_bnk_gra,
               
               ch_bed_cla=ch_bed_cla,depprch=0,depprfp=0,
               
               ch_bed_sil=ch_bed_sil,ch_bed_san=ch_bed_san,ch_bed_gra=ch_bed_gra,
               
               igropt=igropt,tfact=tfact,sedst=0,sanst=0,silst=0,clast=0,sagst=0,
               
               lagst=0,grast=0,algae=0,organicn=0, ammonian=0, 
               
               nitriten=0,nitraten=0, organicp=0, disolvp=0,
               
               rch_cbod=0, rch_dox=0,
               
               mores=mores,iyres=iyres,
               
               res_evol= res_evol,res_pvol=res_pvol,res_esa= res_esa,
               
               res_psa=res_psa,res_rr=res_rr,res_nsed=res_nsed,   
               
               res_vol=res_vol,evrsv=evrsv,res_k=res_k, br1=br1,br2=br2, ndtargr=ndtargr,
               
               sed_stlr=sed_stlr, velsetlr= velsetlr,
               
               
               res_sed=0, res_san=0,
               
               res_sil=0,res_cla=0, res_sag=0, res_lag=0,res_gra=0,
               
               ires1=ires1,ires2=ires2, psetlr1=psetlr1, psetlr2=psetlr2, nsetlr1=nsetlr1,
               
               nsetlr2=  nsetlr2, theta_n=1.08,theta_p=1.08, con_pirr=0,
               
               con_nirr=0, chlar=chlar, seccir=seccir,res_solp=0, res_orgp=0, res_no3=0, 
               
               res_no2=0, res_nh3=0, res_orgn=0,
               
               tmp_win1=5,tmp_win2=0.75,tmp_spr1=5,tmp_spr2=0.75,tmp_sum1=5,
               
               tmp_sum2=0.75,tmp_fal1=5,tmp_fal2=0.75, lat=lat)

               
           }else{ # end of if for reservoir option ==1    


 pars<-list(ch_w2=ch_w2,ch_d=ch_d,ch_s2=ch_s2,ch_l2=ch_l2,ch_n2=ch_n2,
               
               chside=chside,phi_1=phi_1, phi_6=phi_6, phi_10=phi_10,
               
               phi_13=phi_13,ch_k2=ch_k2,ch_cov1=ch_cov1,ch_cov2=ch_cov2, 
               
               ch_wdr= ch_wdr,msk_co1=msk_co1,msk_co2=msk_co2,
               
               msk_x=msk_x,evrch=evrch,alpha_bnk=alpha_bnk, ch_bnk_bd=ch_bnk_bd,
               
               ch_bed_bd=ch_bed_bd,tc_bnk=tc_bnk,tc_bed=tc_bed,ch_bnk_kd=ch_bnk_kd,
               
               ch_bed_kd=ch_bed_kd,ch_bnk_d50=ch_bnk_d50,ch_bed_d50=ch_bed_d50,
               
               prf=prf,spcon=spcon,spexp=spexp, ai0=ai0, ai1=ai1, ai2=ai2,
               
               ai3=ai3, ai4=ai4, ai5=ai5, ai6=ai6, mumax=mumax, rhoq=rhoq,
               
               tfact= tfact,k_l= k_l, k_n=k_n, k_p=k_p, lambda0=lambda0,
               
               lambda1=lambda1, lambda2=lambda2, p_n=p_n, 
               
               chla_subco=chla_subco, rs1= rs1, rs2= rs2, rs3= rs3, rs4= rs4,
               
               rs5= rs5,rs6= rs6,rs7= rs7,rk1=rk1,rk2=rk2,rk3=rk3,rk4=rk4,
               
               rk5=rk5,rk6=rk6,bc1=bc1,bc2=bc2,bc3=bc3,bc4=bc4,
               
               chpst_rea=chpst_rea,chpst_vol=chpst_vol,chpst_koc=chpst_koc,
               
               chpst_stl=chpst_stl,chpst_rsp=chpst_rsp, chpst_mix=chpst_mix,
               
               sedpst_conc=sedpst_conc,sedpst_rea=sedpst_rea, 
               
               sedpst_bry=sedpst_bry,sedpst_act=sedpst_act,
               
               bankst=0,rchstor=0,ch_revap=ch_revap,depch=0,depsanch=0,
               
               depsilch=0,depclach=0,depsagch=0,deplagch=0,
               
               depgrach=0,depfp=0,depsilfp=0,depclafp=0,ch_bnk_cla=ch_bnk_cla,
               
               ch_bnk_sil=ch_bnk_sil,ch_bnk_san=ch_bnk_san,ch_bnk_gra=ch_bnk_gra,
               
               ch_bed_cla=ch_bed_cla,depprch=0,depprfp=0,
               
               ch_bed_sil=ch_bed_sil,ch_bed_san=ch_bed_san,ch_bed_gra=ch_bed_gra,
               
               igropt=igropt,tfact=tfact,sedst=0,sanst=0,silst=0,clast=0,sagst=0,
               
               lagst=0,grast=0,algae=0,organicn=0, ammonian=0, 
               
               nitriten=0,nitraten=0, organicp=0, disolvp=0,
               
               rch_cbod=0, rch_dox=0,
               
               mores=6,iyres=0,
               
               res_evol= 0,res_pvol=0,res_esa=0,
               
               res_psa=0,res_rr=0,res_nsed=0,   
               
               res_vol=0,evrsv=0.6,res_k=0, br1=0,br2=0, ndtargr=10,
               
               sed_stlr=0, velsetlr= 0,
               
               
               res_sed=0, res_san=0,
               
               res_sil=0,res_cla=0, res_sag=0, res_lag=0,res_gra=0,
               
               ires1=1,ires2=1, psetlr1=10, psetlr2=10, nsetlr1=5.5,
               
               nsetlr2= 5.5, theta_n=1.08,theta_p=1.08, con_pirr=0,
               
               con_nirr=0, chlar=1, seccir=1,res_solp=0, res_orgp=0, res_no3=0, 
               
               res_no2=0, res_nh3=0, res_orgn=0,
               
               tmp_win1=5,tmp_win2=0.75,tmp_spr1=5,tmp_spr2=0.75,tmp_sum1=5,
               
               tmp_sum2=0.75,tmp_fal1=5,tmp_fal2=0.75, lat=lat)



}


    
    
    return(pars)
    
}






