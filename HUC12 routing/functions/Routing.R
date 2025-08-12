################################################################################

# Master Routing Function for Flow, Nutrients, and Sediment

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

################################################################################
# varoute_2:  Water (m3)                varoute_4:  Organic N (kg)

# varoute_3: Sediment (ton)             varoute_5:  Organic P (kg)

# varoute_23:  Sand  (ton)              varoute_6:  NO3 (kg)

# varoute_24:  Silt (ton)               varoute_7: Mineral P (kg)

# varoute_25: Clay (ton)                varoute_13: chlorophyll-a (kg)

# varoute_26: Small aggregates (ton)    varoute_14: NH4 (kg)

# varoute_27: Large aggregates (ton)    varoute_15: N02 (kg)

# varoute_28: Gravel (ton)              varoute_16: CBOD (kg)

# varoute_17: Dissolved oxygen

################################################################################

# Set R library directory

library(lubridate)

source('.../functions/Muskingum.R') # Muskingum Routing

source('.../functions/Rtsed.R') # Default Bagnold

source('.../functions/Watqual.R') # Qual2E

source('.../functions/Clgen.R') # Clgen

source('.../functions/Reservoir.R') # Reservoir

source('.../functions/Resnut.R') # Reservoir QUAL2E

source('.../functions/VS.R') # Variable Storage Routing


  Routing<-function(hydrology,sediment,contaminant,routingParams,simulation,
                    
                    nsub,res_option) {
  
  start.date <- as.Date(simulation$start,format="%Y-%m-%d")   
  
  end.date <- as.Date(simulation$end,format="%Y-%m-%d")  
  
  dates <- seq.Date(start.date, end.date, by ="day") 
  
  out_h <- matrix(0,nrow=length(dates),ncol=14)  # output from hydrology routing
  
  out_s <- matrix(0,nrow=length(dates),ncol=4)  # output from sediment routing
  
  out_c <- matrix(0,nrow=length(dates),ncol=20)  # output from water quality
  
  out_res <- matrix(0,nrow=length(dates),ncol=27)  # reservoir
  
  out_resnut <- matrix(0,nrow=length(dates),ncol=20)  # reservoir



varoute_2 <- 0

PRECIP <- 0

PET <- 0

T_ave <- 0
  
solar_rad <- 0

varoute_3 <-0

varoute_23 <-0

varoute_24 <-0
  
varoute_25 <-0

varoute_26 <-0

varoute_27 <-0

varoute_28 <-0

varoute_4 <- 0

varoute_5 <-0

varoute_6 <-0

varoute_7 <-0

varoute_14 <-0

varoute_15 <-0

varoute_16 <-0

varoute_17 <-0

 varoute_13 <-0

  
  # read input
  
  varoute_2 <- hydrology$varoute_2    # [m3/s]
  
  PRECIP <-hydrology$PRECIP # [mm]
  
  PET <- hydrology$PET      # [mm]
  
  T_ave <- hydrology$T_ave    # [C]
  
  solar_rad <-hydrology$solar_rad
  
  varoute_3 <- sediment$varoute_3 # [ton]
  
  varoute_23 <- sediment$varoute_23 # [ton]
  
  varoute_24 <- sediment$varoute_24 # [ton]
  
  varoute_25 <- sediment$varoute_25 # [ton]
  
  varoute_28 <- sediment$varoute_28 # [ton]
  
  varoute_26 <- sediment$varoute_26 # [ton]
  
  varoute_27 <- sediment$varoute_27 # [ton]
  
  varoute_4 <- contaminant$varoute_4 # [kg]
  
  varoute_5 <- contaminant$varoute_5 # [kg]
  
  varoute_6 <- contaminant$varoute_6  # [kg]
  
  varoute_7 <- contaminant$varoute_7  # [kg]
  
  varoute_14 <- contaminant$varoute_14  # [kg]
  
  varoute_15 <- contaminant$varoute_15  # [kg]
  
  varoute_16 <- contaminant$varoute_16  # [kg]
  
  varoute_17 <- contaminant$varoute_17 # [kg]
  
  varoute_13 <-contaminant$varoute_13  # [kg]
  
 

  dayl<-c()
  
  for(i in 1:length(dates)){
    
    day<-i  # day number of simulation
    
    i_mo<-month(dates[i]) 
    
    iyr <- year(dates[i])
    
    
    jul_day <- as.POSIXlt(dates[i])$yday # Julian day
    
    dayl[i]<-clgen(iida=jul_day,routingParams)
    
    
    out_h[i,] <- VS(varoute_2= varoute_2[i],pet_day=PET[i],
                            
                          routingParams=routingParams,day=day,flwin=flwin,
                          
                          flwout=flwout)
    
    
    
    out_s[i,] <- Rtsed(varoute_3=varoute_3[i],rtwtr=out_h[i,4],sdti=out_h[i,5],
                         
                         rchstor=out_h[i,3],rcharea=out_h[i,6],
                      
                         rchdep=out_h[i,7],routingParams)
    
  
    
    
    out_c[i,]<-Watqual(varoute_2=varoute_2[i], rchwtr=routingParams$rchstor,
                      
                        rchdep=out_h[i,7],tmpav=T_ave[i],hru_ra=solar_rad[i],
                      
                        dayl = dayl[i],varoute_4=varoute_4[i],
                      
                        varoute_5=varoute_5[i],varoute_6=varoute_6[i],
                      
                        varoute_7=varoute_7[i],varoute_13=varoute_13[i],
         
                        varoute_14=varoute_14[i],varoute_15=varoute_15[i],
                       
                       varoute_16=varoute_16[i],varoute_17=varoute_17[i],
                       
                       i_mo=i_mo,rttime=out_h[i,12],routingParams)
                         
    
    
    varoute_2[i] <- out_h[i,4]
    
    varoute_3[i] <- out_s[i,2]
    
    varoute_23[i] <- 0
    
    varoute_24[i] <- 0
    
    varoute_25[i] <- 0
    
    varoute_26[i] <- 0
    
    varoute_27[i] <- 0
    
    varoute_28[i] <- 0
    
    
    varoute_4[i] <- out_c[i,3] * out_h[i,4] / 1000
    
    varoute_5[i] <- out_c[i,7] * out_h[i,4] / 1000
    
    varoute_6[i] <- out_c[i,6] * out_h[i,4] / 1000
    
    varoute_7[i] <- out_c[i,8] * out_h[i,4] / 1000
    
    varoute_13[i] <- out_c[i,2] * out_h[i,4] / 1000
    
    varoute_14[i] <- out_c[i,4] * out_h[i,4] / 1000
    
    varoute_15[i] <- out_c[i,5] * out_h[i,4] / 1000
    
    varoute_16[i] <- out_c[i,9] * out_h[i,4] / 1000
    
    varoute_17[i] <- out_c[i,10] * out_h[i,4] / 1000
    
   
    
    if(res_option>0){

    if(iyr > routingParams$iyres | 
       
       (i_mo >= routingParams$mores & iyr==routingParams$iyres) ){
      
      
      out_res[i,] <- Res(varoute_2=varoute_2[i],pet_day=PET[i],
                              
                              percp_day=PRECIP[i],
                              
                              varoute_3=varoute_3[i],varoute_23=varoute_23[i],
                              
                              varoute_24=varoute_24[i],varoute_25=varoute_25[i],
                              
                              varoute_26=varoute_26[i],varoute_27=varoute_27[i],
                              
                              varoute_28=varoute_28[i],routingParams)
      
      
      
      out_resnut[i,]  <- Resnut(resflwo=out_res[i,1],res_vol=out_res[i,2],
                                
                                ressa=out_res[i,3],tmpav=T_ave[i],
                                
                                varoute_4=varoute_4[i],varoute_5=varoute_5[i],
                                
                                varoute_6=varoute_6[i],varoute_7=varoute_7[i],
                                
                                varoute_14=varoute_14[i],
                                
                                varoute_15=varoute_15[i],i_mo=i_mo,routingParams)
      
      
        
        
       varoute_2[i] <- out_res[i,1]
      
      varoute_3[i] <- out_res[i,11]
      
      varoute_23[i] <- out_res[i,12]
      
      varoute_24[i] <- out_res[i,13]
      
      varoute_25[i] <- out_res[i,14]
      
      varoute_26[i] <- out_res[i,15]
      
      varoute_27[i] <- out_res[i,16]
      
      varoute_28[i] <- out_res[i,17]
      
      
      varoute_4[i] <- out_resnut[i,8] 
      
      varoute_5[i] <- out_resnut[i,9] 
      
      varoute_6[i] <- out_resnut[i,14] 
      
      varoute_7[i] <- out_resnut[i,10] 
      
      varoute_14[i] <- out_resnut[i,12] 
      
      varoute_15[i] <- out_resnut[i,13] 
      
      varoute_13[i] <- out_resnut[i,11] 
      
      varoute_16[i] <- 0.          # CBOD
      
      varoute_17[i] <- 0.          # dis O2
  
      
    } # reservoir
   
    } # res_option

    
    if(i < length(dates)){
      
      routingParams$rchstor <- out_h[i,3]
      
      routingParams$bankst <- out_h[i,9]
      
      flwout <- out_h[i,2]
      
      flwin <- out_h[i,1]
      
      routingParams$sedst <-out_s[i,1] 
      
      routingParams$depch <- out_s[i,3] 
      
      routingParams$algae <- out_c[i,1]
           
      routingParams$organicn<-out_c[i,3]
           
      routingParams$ammonian<-out_c[i,4]
           
      routingParams$nitriten<-out_c[i,5]
           
      routingParams$nitraten<-out_c[i,6]
           
      routingParams$organicp<-out_c[i,7]
           
      routingParams$disolvp<-out_c[i,8]
           
      routingParams$rch_cbod<-out_c[i,9]
           
      routingParams$rch_dox<-out_c[i,10]
           
      if(res_option>0){
      
      routingParams$res_vol <- out_res[i,2]
      
      routingParams$res_sed <- out_res[i,4]
      
      routingParams$res_san <- out_res[i,5]
     
      routingParams$res_sil <-  out_res[i,6]
      
      routingParams$res_cla <- out_res[i,7]
      
      routingParams$res_sag <- out_res[i,8]
      
      routingParams$res_lag <- out_res[i,9]
      
      routingParams$res_gra <- out_res[i,10]
      
      
      routingParams$res_solp <- out_resnut[i,6]
      
      routingParams$res_orgp <- out_resnut[i,2]
      
      routingParams$res_no3 <- out_resnut[i,3]
      
      routingParams$res_no2 <- out_resnut[i,5]
      
      routingParams$res_nh3 <- out_resnut[i,4]
      
      routingParams$res_orgn <- out_resnut[i,1]
      
      } # res_option
        
        
      
    }
    
    
    
    
  }
  
  
  

  
  
  
  mat <- as.data.frame(matrix(NA,length(dates),29))
  
  
  
  colnames(mat) <- c("Dates","Reach","I(m3/s)","O(m3/s)",
                     
                     "Depth(m)",'Velocity(m/s)',"EVAP(m3/s)","TLOSS(m3/s)",
                     
                     'SED_in(ton)',"SED_out(ton)", 
                   
                     "Chlora_out(kg)","ORGN_out(kg)","NH4_out(kg)","NO2_out(kg)","NO3_out(kg)",
                   
                     "ORGP_out(kg)","SOLP_out(kg)","CBOD_out(kg)",
                   
                     "DISOX_out(kg)", 'Chlora_in(mg/L)',"ORGN_in(mg/L)","NH4_in(mg/L)",
                   
                     "NO2_in(kg)","NO3_in(kg)","ORGP_in(kg)",
                   
                     "SOLP_in(kg)","CBOD_in(kg)", "DISOX_in(kg)",
                   
                     'TEMPAV(C)')
                   
                    
                   
   ####----- Additional Variables---------------                  

                    #  ,'SOXY(mg/L)'

                    #  ,'Day_length(hr)','RESINFLOW (m3/s)', 'RESFLOW (m3/s)',
                   
                    # 'RESVOL(1e4m3)','RESSA','RESVAP','RESPCP', 'RES_SED(mg/L)',
                   
                   #  'RES_SAN(mg/L)', 'RES_SIL(mg/L)','RES_CLA(mg/L)', 
                   
                   #  'RES_SAG(mg/L)', 'RES_LAG(mg/L)','RES_GRA(mg/L)',
                   
                   #  'RES_SED_out(ton)','RES_SAN_out(ton)','RES_SIL_out(ton)',
                   
                   #  'RES_CLA_out(ton)','RES_SAG_out(ton)',
                   
                   #  'RES_LAG_out(ton)','RES_GRA_out(ton)',
                   
                   #  'RES_SED_in(ton)','RES_SAN_in(ton)','RES_SIL_in(ton)',
                   
                    # 'RES_CLA_in(ton)','RES_SAG_in(ton)',
                   
                    # 'RES_LAG_in(ton)','RES_GRA_in(ton)', 'RES_ORGN(mg/L)',
                   
                   #  'RES_ORGP(mg/L)','RES_NO3(mg/L)','RES_NH3(mg/L)','RES_NO2(mg/L)',
                   
                   #  'RES_SOLP(mg/L)','RES_CHLA(mg/L)','RES_ORGN_out(kg)',
                   
                   #  'RES_ORGP_out(kg)','RES_SOLP_out(kg)','RES_CHLA_out(kg)',
                   
                    # 'RES_NH3_out(kg)','RES_NO2_out(kg)','RES_NO3_out(kg)', 
                   
                   #  'RES_ORGN_in(kg)','RES_ORGP_in(kg)','RES_NO3_in(kg)',
                   
                   #  'RES_SOLP_in(kg)','RES_NH3_in(kg)','RES_NO2_in(kg)')
  
  
  
     
  
  mat[,1] <- dates
  
  mat[,2] <- rep(nsub,length(dates))
  
  mat[,3] <- out_h[,14] / 86400                       # inflow [m3/s]
  
  mat[,4] <- out_h[,4] / 86400                        # outflow [m3/s]
  
  mat[,5] <- out_h[,7]                                # water depth [m]
  
  mat[,6] <- out_h[,13]                               # velocity [m/s]   
  
  mat[,7] <- out_h[,10] /86400                        # Evaporation from channel [m3/s]
  
  mat[,8] <- out_h[,11] /86400                        # transmission Loss [m3/s]
  
  mat[,9] <- out_s[,4]                                # sediment entering channel [ton]
  
  mat[,10] <- out_s[,2]                               # sediment leaving channel [ton]                      
  
  mat[,11] <- out_c[,2] * out_h[,4] / 1000            # chlora leaving channel [kg]
  
  mat[,12] <- out_c[,3] * out_h[,4] / 1000            # Particulate N leaving channel [kg]
  
  mat[,13] <- out_c[,4] * out_h[,4] / 1000            # NH4 leaving channel [kg]
  
  mat[,14] <- out_c[,5] * out_h[,4] / 1000            # NO2 leaving channel [kg]
  
  mat[,15] <- out_c[,6] * out_h[,4] / 1000            # NO3 leaving channel [kg]
  
  mat[,16] <- out_c[,7] * out_h[,4] / 1000            # Particulate P leaving channel [kg]
  
  mat[,17] <- out_c[,8] * out_h[,4] / 1000            # Soluble P leaving channel [kg]
  
  mat[,18] <- out_c[,9] * out_h[,4] / 1000            # CBOD leaving channel [kg]
  
  mat[,19] <- out_c[,10] * out_h[,4] / 1000           # DO leaving channel [kg]
  
  mat[,20] <- out_c[,16]                              # chlora entering channel [kg]
  
  mat[,21] <- out_c[,12]                              # Particulate N entering channel [kg]
  
  mat[,22] <- out_c[,17]                              # NH4 entering channel [kg]

  mat[,23] <- out_c[,18]                              # NO2 entering channel [kg]
  
  mat[,24] <- out_c[,14]                              # NO3 entering channel [kg]
  
  mat[,25] <- out_c[,13]                              # Particulate P entering channel [kg]
 
  mat[,26] <- out_c[,15]                              # Soluble P entering channel [kg]
  
  mat[,27] <- out_c[,19]                              # CBOD entering channel [kg]
  
  mat[,28] <- out_c[,20]                              # DO entering channel [kg]

  mat[,29] <- out_c[,11]                              # Water temperature [C]

  
  varoute1<-matrix(c(varoute_2=varoute_2,varoute_3=varoute_3,
                    
                    varoute_4=varoute_4,varoute_5=varoute_5,varoute_6=varoute_6,
                    
                    varoute_7=varoute_7,varoute_13=varoute_13,
                    
                    varoute_14=varoute_14,varoute_15=varoute_15,
                    
                    varoute_16=varoute_16,varoute_17=varoute_17,
                    
                   varoute_23=varoute_23,varoute_24=varoute_24,
                    
                   varoute_25=varoute_25,varoute_26=varoute_26,
                    
                    varoute_27=varoute_27,varoute_28=varoute_28),ncol=17)


#varoute1<-matrix(c(varoute_2,varoute_3,
                    
                #    varoute_4,varoute_5,varoute_6,
                    
                 #   varoute_7,varoute_13,
                    
                 #   varoute_14,varoute_15,
                    
                 #   varoute_16,varoute_17,
                    
                  # varoute_23,varoute_24,
                    
                  # varoute_25,varoute_26,
                    
                  #  varoute_27,varoute_28),ncol=17)



  
  route.out<-list(mat=mat,varoute=varoute1)
  
  
  
  return(route.out)
  
}   





 
 

 
 
 
 
 
 
 