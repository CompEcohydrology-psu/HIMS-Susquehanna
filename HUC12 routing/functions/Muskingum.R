################################################################################

# Channel Flow Routing with the Muskingum Method

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

# References: 

# SWAT source codes

# SWAT 2009 theory

################################################################################


Muskingum<-function(varoute_2,pet_day,routingParams,day,flwin,flwout){
  
  
  
  
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
  
  
#init
  

  
  
  qdbank <- 0.
  rcharea <- 0.
  rchdep <- 0.
  
  revapday<-0
  
  rtevp <- 0.
  rttime <- 0.
  rttlc <- 0.
  rtwtr <- 0.
  
  sdti <- 0.
  #### parameters
  
  phi_1<-routingParams$phi_1        # bankfull channel area (m2)
  
  phi_6<-routingParams$phi_6        # channel bed width (m)
  
  msk_co1<-routingParams$msk_co1   # calibration coefficient to control impact
  
  #of the storage time constant for the reach at bankfull depth  upon
  
  #the storage time constant for the reach used in the Muskingum flow method.
  
  msk_co2<-routingParams$msk_co2    # calibration coefficient to control impact
  
  #of the storage time constant for the reach at 0.1 bankfull depth  upon
  
  #the storage time constant for the reach used in the Muskingum flow method.
  
  msk_x<-routingParams$msk_x        # calibration parameter for Muskingum  
  
  # routing method ranging between 0 - 0.3 
  
  # tip: if msk_x is set to 0, then Muskingum method declines to 
  
  #variable storage method
  
  ch_l2<-routingParams$ch_l2        # length of channel (km)
  
  chside<-routingParams$chside      # channel side slope [SWAT default =2]
  
  ch_d<-routingParams$ch_d          # bankfull depth (m)
  
  ch_n2<-routingParams$ch_n2        # Manning roughness
  
  ch_s2<-routingParams$ch_s2        # channel slope
  
  ch_k2<-routingParams$ch_k2        # effective hydraulic conductivity (mm/hr)
  
  rchstor<-routingParams$rchstor    # water stored in reach (m3)
  
  evrch<-routingParams$evrch        # Reach evaporation adjustment factor (-)
  
  ch_wdr <- routingParams$ch_wdr    # Channel width:depth ratio
  
  ch_w2 <- routingParams$ch_w2            #Main channel width
  
  alpha_bnk <- routingParams$alpha_bnk # Baseflow alpha factor for bank 
  
  # storage [days]
  
  bankst <- routingParams$bankst # water stored in bank
  
  ch_revap <- routingParams$ch_revap
  
  phi_10 <- routingParams$phi_10
  
  phi_13 <- routingParams$phi_13
  
  
  ######################################
  
  
  qinday <- 0
  
  qoutday <- 0
  
  det <- 24.
  
  sum_rttlc <- 0.0
  
  sum_rtevp <- 0.0
  
  
  
  # Water entering reach on day
  
  wtrin <- 0.
  
  wtrin <- varoute_2 #[m3]
  

  
 # Compute storage time constant for reach (msk_co1 + msk_co2 = 1.)
    
  msk1 <- msk_co1 / (msk_co1 + msk_co2)
  
  msk2 <- msk_co2 / (msk_co1 + msk_co2)
  
  msk_co1 <- msk1
  
  msk_co2 <- msk2
  

  xkm <- 0.
  
  xkm <- phi_10 * msk_co1 + phi_13 * msk_co2
  
  # Muskingum numerical stability -Jaehak Jeong, 2011
  
  # Check numerical stability
  
  detmax <- 2.* xkm * (1.- msk_x)
  
  detmin <- 2.* xkm * msk_x
  
  
  
  
  # Discretize time interval to meet the stability criterion 
  
  if (det>detmax) {
  
  if (det/2. <= detmax) {
  
  det <- 12
  
  nn <- 2
  
  
  }else if(det/4. <= detmax) {
  
  det <- 6 
  
  nn <- 4
  
  }else{
    
    det <- 1
    
    nn <- 24
  
  }
  
  }else{
    
    det <- 24
    
    nn <- 1
  
  }
  
  
  # Inflow during a sub time interval    
  
  wtrin <- wtrin / nn
  
  # Iterate for the day    
  
  
  for(ii in 1:nn){
    
    # calculate volume of water in reach
    
 
    
    vol <- 0.
    
    vol <- wtrin + rchstor
    
    # Find average flowrate in a sub time interval
    
    volrt <- vol / (86400. / nn)
    
    # Find maximum flow capacity of the channel at bank full
    
    c <- 0.
    
    c <- chside
    
    p <- phi_6 + 2. * ch_d * sqrt(1. + c * c)
    
    rh <- phi_1 / p
    
    maxrt <- Qman(phi_1, rh, ch_n2, ch_s2)
    
    sdti <- 0.
    
    rchdep <- 0.
    
    p <- 0.
    
    rh <- 0.
    
    vc <- 0.
    
    
    # If average flowrate is greater than  the channel capacity at bank full
    
    # then simulate flood plain flow else simulate the regular channel flow
    
    if (volrt > maxrt) {
      
    rcharea <- phi_1
    
    rchdep <- ch_d
    
    p <- phi_6 + 2. * ch_d * sqrt(1. + c * c)
    
    rh <- phi_1 / p
    
    sdti <- maxrt
    
    adddep <- 0
    
    # find the crossectional area and depth for volrt
    
    # by iteration method at 1cm interval depth
    
    # find the depth until the discharge rate is equal to volrt
    
    while (sdti < volrt){
      
    adddep <- adddep + 0.01
    
    addarea <- rcharea + ((ch_w2 * 5) + 4 * adddep) * adddep
    
    addp <- p + (ch_w2 * 4) + 2. * adddep * sqrt(1. + 4 * 4)
    
    rh <- addarea / addp
    
    sdti <- Qman(addarea, rh, ch_n2, ch_s2)
    
    }
    
    rcharea <- addarea
    
    rchdep <- ch_d + adddep
    
    p <- addp
    
    sdti <- volrt
    
    }else{
      
      
      # find the crossectional area and depth for volrt
      
      # by iteration method at 1cm interval depth
      
      # find the depth until the discharge rate is equal to volrt
      
      while (sdti < volrt){
        
      rchdep <- rchdep + 0.01
      
      rcharea <- (phi_6 + c * rchdep) * rchdep
      
      p <- phi_6 + 2. * rchdep * sqrt(1. + c * c)
      
      rh <- rcharea / p
      
      sdti <- Qman(rcharea, rh, ch_n2, ch_s2)
      
    }
      
      sdti <- volrt
    }
      
    # calculate top width of channel at water level
    
    topw <- 0.
    
    if (rchdep <= ch_d) {
    
    topw <- phi_6 + 2. * rchdep * c
    
    }else{
      
      topw <- 5 * ch_w2 + 2. * (rchdep - ch_d) * 4.
    
    }
      
      
    
  
    
    
    if (sdti > 0) { #1
    
    # calculate velocity and travel time
      
    vc <- sdti / rcharea
    
    vel_chan <- vc
    
    rttime <- ch_l2 * 1000. / (3600. * vc)
    
    # Compute coefficients
    
    yy <- 0.
    
    c1 <- 0.
    
    c2 <- 0.
    
    c3 <- 0.
    
    c4 <- 0.
    
    yy <- 2. * xkm * (1. - msk_x) + det
    
    c1 <- (det - 2. * xkm * msk_x) / yy
    
    c2 <- (det + 2. * xkm * msk_x) / yy
    
    c3 <- (2. * xkm * (1. - msk_x) - det) / yy
    
    
    # Compute water leaving reach on day
    
    if (ii == 1 & day==1) {
    
    flwin <- rchstor
    
    flwout <- rchstor
    
    }
    
    rtwtr <- c1 * wtrin + c2 * flwin + c3 * flwout
    
    if (rtwtr < 0.) {rtwtr <- 0.}
    
    rtwtr <- min(rtwtr, (wtrin + rchstor))
    
    
    # calculate amount of water in channel at end of day
    
    rchstor <- rchstor + wtrin - rtwtr
    
    # Add if statement to keep rchstor from becoming negative
    
    if (rchstor < 0.0) {rchstor <- 0.0}
    
    # transmission and evaporation losses are proportionally taken from the 
    
    # channel storage and from volume flowing out
    
    # calculate transmission losses
    
    rttlc <- 0.
    
    if (rtwtr > 0.) {
    
    #  Total time in hours to clear the water
    
    rttlc <- det * ch_k2 * ch_l2 * p
    
    rttlc2 <- rttlc * rchstor / (rtwtr + rchstor)
    
    if (rchstor <= rttlc2) {
      
    rttlc2 <- min(rttlc2, rchstor)
    
    rchstor <- rchstor - rttlc2
    
    rttlc1 <- rttlc - rttlc2
    
    if (rtwtr <= rttlc1) {
      
    rttlc1 <- min(rttlc1, rtwtr)
    
    rtwtr <- rtwtr - rttlc1
    
    }else{
      
      rtwtr <- rtwtr - rttlc1
      
    }
      
    }else{
      
      rchstor <- rchstor - rttlc2
      
    rttlc1 <- rttlc - rttlc2
    
    if (rtwtr <= rttlc1) {
    
    rttlc1 <- min(rttlc1, rtwtr)
    
    rtwtr <- rtwtr - rttlc1
    
    }else{
      
      rtwtr <- rtwtr - rttlc1
    
    }
    }
    
    rttlc <- rttlc1 + rttlc2
    
    sum_rttlc <- sum_rttlc + rttlc
    
    }
    
    
    
    

    
    # calculate evaporation
    
    rtevp <- 0.
    
    if (rtwtr > 0.) { #2
    
    aaa <- evrch * pet_day / (1000.0 * nn)
    
    
    if (rchdep <= ch_d) {
      
    rtevp <- aaa * ch_l2 * 1000. * topw
    
    }else{
      
      if (aaa <=  (rchdep - ch_d)) {
    
    rtevp <- aaa * ch_l2 * 1000. * topw
    
    }else{
      
      rtevp <- (rchdep - ch_d) 
      
    rtevp <- rtevp + (aaa - (rchdep - ch_d)) 
    
    topw <- phi_6 + 2. * ch_d * c       
    
    rtevp <- rtevp * ch_l2 * 1000. * topw
    }
    }
    
    rtevp2 <- rtevp * rchstor / (rtwtr + rchstor)
    
    if (rchstor <= rtevp2) {
    
    rtevp2 <- min(rtevp2, rchstor)
    
    rchstor <- rchstor - rtevp2
    
    rtevp1 <- rtevp - rtevp2
    
    if (rtwtr <= rtevp1) {
      
    rtevp1 <- min(rtevp1, rtwtr)
    
    rtwtr <- rtwtr - rtevp1
    
    }else{
      
      rtwtr <- rtwtr - rtevp1
    }
    
    }else{
      
      rchstor <- rchstor - rtevp2
      
    rtevp1 <- rtevp - rtevp2
    
    if (rtwtr <= rtevp1) {
      
    rtevp1 <- min(rtevp1, rtwtr)
    
    rtwtr <- rtwtr - rtevp1
    
    }else{
      
      rtwtr <- rtwtr - rtevp1
      
    }
    
    }
    
    rtevp <- rtevp1 + rtevp2
    
    
    } #2
    
    
    sum_rtevp <- sum_rtevp + rtevp
    
    # define flow parameters for current iteration
    
    
    
    flwin <- 0.
    
    flwout <- 0.
    
    flwin <- wtrin
    
    flwout <- rtwtr
    
    # define flow parameters for current day
    
    qinday <- qinday + wtrin
    
    qoutday <- qoutday + rtwtr 
    
    
    
    
    # total outflow for the day
    
    rtwtr <- qoutday
    
    
    
    }else{
      
      rtwtr <- 0.
      
      sdti <- 0.
      
      rchstor <- 0.
      
      vel_chan <- 0.
      
      flwin <- 0.
      
      flwout <- 0.
    
      
    }
    
    
    
    
    
  } # end of loop
  
  
  # precipitation on reach is not calculated because area of HRUs 
  # in subbasin sums up to entire subbasin area (including channel
      # area) so precipitation is accounted for in subbasin loop
  
  #      volinprev(jrch) = wtrin
  #	qoutprev(jrch) = rtwtr
  
  rttlc <- sum_rttlc
  
  rtevp <- sum_rtevp
  

  if (rtwtr < 0.) {rtwtr <- 0.}
  
  if (rchstor < 0.) {rchstor <- 0.}
  
  if (rchstor < 10.) { 
    
  rtwtr <- rtwtr + rchstor
  
  rchstor <- 0.
  
  }
  
  
  # add transmission losses to bank storage/deep aquifer in subbasin
 
  
  alpha_bnke <- exp(-alpha_bnk)

  trnsrch <-0 # TRNSRCH: reach transmission loss partitioning to deep aquifer
  
  if (rttlc > 0.) {
    
    bankst <- bankst + rttlc * (1. - trnsrch)
    
  }
  
  # compute revap from bank storage
  
  revapday <- ch_revap * pet_day * ch_l2 * ch_w2
  
  revapday <- min(revapday,bankst)
  
  bankst <- bankst - revapday
  
  # compute contribution of water in bank storage to streamflow
  
  qdbank <- bankst * (1. - alpha_bnke)
  
  bankst <- bankst - qdbank
  
  rtwtr <- rtwtr + qdbank
  

  
  output<-c(flwin,flwout,rchstor,rtwtr,sdti,rcharea,rchdep, qinday,bankst,
            
            rtevp,rttlc,rttime,vel_chan,varoute_2)
  
  
  return(output)
  
} # end of function






