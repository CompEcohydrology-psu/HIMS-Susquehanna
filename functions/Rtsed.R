################################################################################

# Sediment routing in channel

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

# References: 

# SWAT source code - https://swat.tamu.edu/software/swat/

# SWAT 2009 theory

################################################################################


Rtsed<-function(varoute_3,rtwtr,sdti,rchstor,rcharea,rchdep,wetsedo,
                routingParams){


# Assuming 'parm' module is already handled in R environment

# Initialize variables


qdin <- 0.0

sedin <- 0.0

vc <- 0.0

cyin <- 0.0

cych <- 0.0

depnet <- 0.0

deg1 <- 0.0

deg2 <- 0.0

dep <- 0.0

deg <- 0.0

sedrch <- 0.0

outfract <-0.0


# reading parameters [hydrology]

phi_1 <- routingParams$phi_1     # bankfull channel area [m2]  

phi_6 <- routingParams$phi_6     # channel bed width [m]  

ch_l2 <- routingParams$ch_l2     # length of channel [km]

chside <- routingParams$chside   # channel side slope (SWAT default =2) [-]

ch_d <- routingParams$ch_d       # bankfull depth [m]

ch_n2 <- routingParams$ch_n2     # Manning roughness [-]

ch_s2 <- routingParams$ch_s2     # channel slope [-]

ch_w2 <- routingParams$ch_w2 # top with at bankfull depth


# reading parameters [sediment]

prf <- routingParams$prf #Reach peak rate adjustment factor for sediment [-]

spcon <- routingParams$spcon #linear parameter for calculating sediment

spexp <- routingParams$spexp # power parameter for calculating sediment

ch_bed_bd <- routingParams$ch_bed_bd 

ch_bnk_bd <- routingParams$ch_bnk_bd

ch_bnk_d50 <- routingParams$ch_bnk_d50 # Median particle size diameter of 

#channel bank sediment (μm)

ch_bed_d50 <- routingParams$ch_bed_d50 # Median particle size diameter of 

#channel bank sediment (μm)

ch_bnk_kd <- routingParams$ch_bnk_kd #channel bank erodibility coefficient

#[cm^3/N/s]

ch_bed_kd <- routingParams$ch_bed_kd #channel bed erodibility coefficient

#[cm^3/N/s]

tc_bnk <- routingParams$tc_bnk  # critical shear stress for bank [N/m2]

tc_bed <- routingParams$tc_bed  # critical shear stress bed [N/m2]


ch_erodmo <- routingParams$ch_cov1 #cover factor for channel bank [-]

ch_cov2 <- routingParams$ch_cov2 # cover factor for channel bed [-]

depch <- routingParams$depch     # initial deposited sediment in channel [ton]


sedst <- routingParams$sedst



if (rtwtr > 0 && rchdep > 0) {
  
  qdin <- rtwtr + rchstor
  
  if (qdin > 0.01) {
    
    sedin <- varoute_3  + sedst + wetsedo
    
    sedinorg <- sedin
    
    peakr <- prf * sdti
    
    # Calculate flow velocity
    
    vc <- ifelse(rchdep < 0.010, 0.01, peakr / rcharea)
    
    vc <- min(vc, 5)
    
    tbase <- ch_l2 * 1000 / (3600 * 24 * vc)
    
    tbase <- min(tbase, 1)
    
    # Sediment transport
    
    cyin <- sedin / qdin
    
    cych <- spcon * vc ^ spexp
    
    depnet <- qdin * (cych - cyin)
    
    depnet <- ifelse(abs(depnet) < 1.0e-6, 0, depnet)
    
    if (depnet > 1.0e-6) {
      
      deg <- depnet
      
      if (deg >= depch) {
        
        deg1 <- depch
        
        deg2 <- (deg - deg1) * ch_erodmo * ch_cov2
        
      } else {
        
        deg1 <- deg
        
        deg2 <- 0
        
      }
      
      dep <- 0
      
    } else {
      
      dep <- -depnet
      
      deg <- 0
      
      deg1 <- 0
      
      deg2 <- 0
    }
    
    depch <- depch + dep - deg1
    
    depch <- max(depch, 1.0e-6)
    
    sedin <- sedin + deg1 + deg2 - dep
    
    sedin <- max(sedin, 1.0e-6)
    
    outfract <- rtwtr / qdin
    
    outfract <- min(outfract, 1.0)
    
    sedrch <- sedin * outfract
    
    sedrch <- max(sedrch, 1.0e-6)
    
    sedst <- sedin - sedrch
    
    sedst <- max(sedst, 1.0e-6)
    
    # Additional calculations and mass balance tests...
    
    # Compute changes in channel dimensions
    # Assuming `ideg` is defined elsewhere in the R environment
   # if (ideg == 1) {
      # ... calculations for channel dimensions ...
   # }
    
  } else {
    
    sedrch <- 0
    
    sedst <- sedin
    
    sedrch <- 0.
    
    rch_san <- 0.
    
    rch_sil <- 0.
    
    rch_cla <- 0.
    
    rch_sag <- 0.
    
    rch_lag <- 0.
    
    rch_gra <- 0.
    
    
    
  }
}

output<-c(sedst,sedrch,depch,varoute_3)


return(output)


}
