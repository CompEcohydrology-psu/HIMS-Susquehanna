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

# varoute_24:  Silt (ton)               varoute_7: Soluble/Mineral P (kg)

# varoute_25: Clay (ton)                varoute_13: Ohlorophyll-a (kg)

# varoute_26: Small aggregates (ton)    varoute_14: NH4 (kg)

# varoute_27: Large aggregates (ton)    varoute_15: NO2 (kg)

# varoute_28: Gravel (ton)              varoute_16: CBOD (kg)

# varoute_17: Dissolved oxygen

################################################################################

# Call functions                 

source(".../functions/Rtsed.R") # Default Bagnold

source(".../functions/Watqual.R") # Qual2E

source(".../functions/Clgen.R") # Clgen

source(".../functions/Reservoir.R") # Reservoir

source(".../functions/Resnut.R") # Reservoir QUAL2E

source(".../functions/Liu.R") # Liu algorithm

source(".../functions/Wetland_rip_sed.R") # Wetland sediment

source(".../functions/Wetland_rip_nut.R") # Wetland nutrients

source(".../functions/Calculate_week.R") # Calculate week from Nov 1 (water year)

source(".../functions/VS.R") # Variable storage routing


# Set R library path here ...


library(lubridate)



Routing <- function(hydrology,sediment,contaminant,wat_yld,sed_yld,san_yld,

                            sil_yld,cla_yld,sag_yld,lag_yld,psol_yld,psed_yld,orgn_yld, no3_yld,

                             routingParams,simulation,nsub,res_option) {

  
  start.date <- as.Date(simulation$start,format="%Y-%m-%d")   
  
  end.date <- as.Date(simulation$end,format="%Y-%m-%d")  
  
  dates <- seq.Date(start.date, end.date, by ="day") 

  out_h <- matrix(0,nrow=length(dates),ncol=14)
  
  out_s <- matrix(0,nrow=length(dates),ncol=4)  # output from sediment routing in channel
  
  out_c <- matrix(0,nrow=length(dates),ncol=21)  # output from nutrient routing in channel

  out_res <- matrix(0,nrow=length(dates),ncol=27)  # reservoir
  
  out_resnut <- matrix(0,nrow=length(dates),ncol=20)  # reservoir


if( is.na(routingParams$wet_fr)==FALSE){ # riperian wetland exists
  
  out_h <- matrix(0,nrow=length(dates),ncol=19)  # output from water routing in wetland and channel

  outwet_s <- matrix(0,nrow=length(dates),ncol=12) # output from sediment routing in wetland

  outwet_c <- matrix(0,nrow=length(dates),ncol=12) # output from nutrient routing in wetland

}


  
  
  
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

    week <- Calculate_week(dates[i])
    
    
    jul_day <- as.POSIXlt(dates[i])$yday # Julian day

    
    dayl[i]<-clgen(iida=jul_day,routingParams)


if(is.na(routingParams$wet_fr)==FALSE){  # riperian wetland exists
    
    

    out_h[i,]<- Liu(varoute_2=varoute_2[i],pet_day=PET[i],flwin= flwin,flwout= flwout,watyld=wat_yld[i],
               
              routingParams=routingParams,day=day)



    
    outwet_s[i,] <- Wetland_rip_sed(wet_vol=out_h[i,15],wet_voli=out_h[i,17],S_wet=out_h[i,19],
                                
                               sedyld=sed_yld[i], sanyld=san_yld[i], silyld=sil_yld[i], 
                                
                                clayld=cla_yld[i],sagyld=sag_yld[i],lagyld=lag_yld[i], 
                               
                               varoute_2=varoute_2[i],varoute_3=varoute_3[i],
                                
                                routingParams=routingParams)


   outwet_c[i,] <- Wetland_rip_nut(wet_vol= out_h[i,15],wet_voli= out_h[i,17], S_wet=out_h[i,19],
                                  
                                 psolyld=psol_yld[i], psedyld=psed_yld[i],
                                  
                                  orgnyld= orgn_yld[i], no3yld=no3_yld[i],watyld=wat_yld[i],
                                  
                                  varoute_2=varoute_2[i],varoute_4=varoute_4[i],
                                  
                                  varoute_5=varoute_5[i],varoute_6=varoute_6[i],
                                  
                                  varoute_7=varoute_7[i],
                                  
                                  varoute_14=varoute_14[i],varoute_15=varoute_15[i],
                                  
                                  routingParams=routingParams,i_mo=i_mo)


   out_s[i,] <- Rtsed(varoute_3=varoute_3[i], rtwtr=out_h[i,4],sdti=out_h[i,5],
                     
                     rchstor=out_h[i,3],rcharea=out_h[i,6],
                     
                     rchdep=out_h[i,7],wetsedo=outwet_s[i,7],routingParams=routingParams)  



    out_c[i,]<-Watqual(varoute_2=varoute_2[i], rchwtr=routingParams$rchstor,
                     
                     rchdep=out_h[i,7],tmpav=T_ave[i],hru_ra=solar_rad[i],
                     
                     dayl = dayl[i],varoute_4=varoute_4[i],
                     
                     varoute_5=varoute_5[i],varoute_6=varoute_6[i],
                     
                     varoute_7=varoute_7[i],varoute_13=varoute_13[i],
                     
                     varoute_14=varoute_14[i],varoute_15=varoute_15[i],
                     
                     varoute_16=varoute_16[i],varoute_17=varoute_17[i],
                     
                     i_mo=i_mo,rttime=out_h[i,12],S_wet=out_h[i,19], 
                     
                     wet_orgn_out=outwet_c[i,7],wet_no3_out=outwet_c[i,8],wet_psed_out=outwet_c[i,9],
                     
                     wet_psol_out=outwet_c[i,10], wet_no2_out=outwet_c[i,11], wet_nh4_out=outwet_c[i,12],
                     
                     routingParams=routingParams)
  
}else{



 out_h[i,]<- VS(varoute_2=varoute_2[i],pet_day=PET[i],day=day,flwin=flwin,flwout=flwout,S_wet=0,routingParams=routingParams)
  

    
    out_s[i,] <- Rtsed(varoute_3=varoute_3[i],rtwtr=out_h[i,4],sdti=out_h[i,5],
                         
                         rchstor=out_h[i,3],rcharea=out_h[i,6],
                      
                         rchdep=out_h[i,7],wetsedo=0,routingParams)


    
    out_c[i,]<-Watqual(varoute_2=varoute_2[i], rchwtr=routingParams$rchstor,
                      
                        rchdep=out_h[i,7],tmpav=T_ave[i],hru_ra=solar_rad[i],
                      
                        dayl = dayl[i],varoute_4=varoute_4[i],
                      
                        varoute_5=varoute_5[i],varoute_6=varoute_6[i],
                      
                        varoute_7=varoute_7[i],varoute_13=varoute_13[i],
         
                        varoute_14=varoute_14[i],varoute_15=varoute_15[i],
                       
                       varoute_16=varoute_16[i],varoute_17=varoute_17[i],
                       
                       i_mo=i_mo,rttime=out_h[i,12],S_wet=0,wet_orgn_out=0,

                      wet_no3_out=0,wet_psed_out=0,wet_psol_out=0,wet_no2_out=0,

                      wet_nh4_out=0,routingParams)


}
                         
    
    
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
    
   
    
    if(res_option>0){ # reservoirs included

    if(iyr > routingParams$iyres | 
       
       (i_mo >= routingParams$mores & iyr==routingParams$iyres) ){
      
      
      out_res[i,] <- Res(varoute_2=varoute_2[i],pet_day=PET[i],
                              
                              percp_day=PRECIP[i],
                              
                              varoute_3=varoute_3[i],varoute_23=varoute_23[i],
                              
                              varoute_24=varoute_24[i],varoute_25=varoute_25[i],
                              
                              varoute_26=varoute_26[i],varoute_27=varoute_27[i],
                              
                              varoute_28=varoute_28[i],routingParams,week=week)
      
      
      
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
      
            
      
      routingParams$algae<-out_c[i,1]
           
      routingParams$organicn<-out_c[i,3]
           
      routingParams$ammonian<-out_c[i,4]
           
      routingParams$nitriten<-out_c[i,5]
           
      routingParams$nitraten<-out_c[i,6]
           
      routingParams$organicp<-out_c[i,7]
           
      routingParams$disolvp<-out_c[i,8]
           
      routingParams$rch_cbod<-out_c[i,9]
           
      routingParams$rch_dox<-out_c[i,10]



if( is.na(routingParams$wet_fr)==FALSE){

  routingParams$wet_vol <- out_h[i,15]
  
  routingParams$wet_sed <- outwet_s[i,1]   # sediment concentration in wetland [kg/L]
  
  routingParams$wet_san <- outwet_s[i,2]
  
  routingParams$wet_sil <- outwet_s[i,3]
  
  routingParams$wet_cla <- outwet_s[i,4]
  
  routingParams$wet_sag <- outwet_s[i,5]
  
  routingParams$wet_lag <- outwet_s[i,6]
  
  routingParams$wet_psol <- outwet_c[i,4] 
  
  routingParams$wet_psed <- outwet_c[i,3]
  
  routingParams$wet_orgn <- outwet_c[i,1]
  
  routingParams$wet_no3 <-  outwet_c[i,2]
  
  routingParams$wet_no2 <-  outwet_c[i,5]
  
  routingParams$wet_nh4 <- outwet_c[i,6]

}


           
      if(res_option>0){

if(iyr > routingParams$iyres | 
       
       (i_mo >= routingParams$mores & iyr==routingParams$iyres) ){

      
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

      routingParams$res_chla <- out_resnut[i,7]


} # if for operation year

      
      } # res_option
        
        
      
    }
    
    
    
    
  }
  
  
  

  
  
  
  mat <- as.data.frame(matrix(NA,length(dates),24))
  
  
  
  colnames(mat) <- c("Dates","Reach",'I(m3/s)','O(m3/s)',
                                    
                     'SED_in(ton)',"SED_out(ton)",
                   
                     'Chlora_out(kg)','ORGN_out(kg)',

                     'NH4_out(kg)','NO2_out(kg)','NO3_out(kg)',
                   
                     'ORGP_out(kg)','SOLP_out(kg)', 'CBOD_out(kg)', 'DISOX_out(kg)',
                                      
                     'RESFLOW (m3/s)','RES_SED_out(ton)', 'RES_ORGN_out(kg)',
                   
                     'RES_ORGP_out(kg)','RES_SOLP_out(kg)','RES_CHLA_out(kg)',
                   
                     'RES_NH4_out(kg)','RES_NO2_out(kg)','RES_NO3_out(kg)') 
  
  
  
  mat[,1] <- dates
  
  mat[,2] <- rep(nsub,length(dates))         # Reach code
  
  mat[,3] <- out_h[,14] / 86400              # Inflow [m3/s]
  
  mat[,4] <- out_h[,4] / 86400               # Outflow [m3/s]
  
  mat[,5] <- out_s[,4]                      # Sediment in [ton]
    
  mat[,6] <- out_s[,2]                       # Sediment out [ton]
  
  mat[,7] <- out_c[,2] * out_h[,4] / 1000    # Chlorophyl [kg]
  
  mat[,8] <- out_c[,3] * out_h[,4] / 1000    # Particulate nitrogen [kg]
  
  mat[,9] <- out_c[,4] * out_h[,4] / 1000   # Ammonium [kg]
  
  mat[,10] <- out_c[,5] * out_h[,4] / 1000   # NO2 [kg]
  
  mat[,11] <- out_c[,6] * out_h[,4] / 1000   # NO3 [kg]
  
  mat[,12] <- out_c[,7] * out_h[,4] / 1000   # Organic phosphorus [kg]
  
  mat[,13] <- out_c[,8] * out_h[,4] / 1000   # Soluble phosphorus [kg]

  mat[,14] <- out_c[,9] * out_h[,4] / 1000   # CBOD [kg]
  
  mat[,15] <- out_c[,10] * out_h[,4] / 1000   # DISOX [kg]

  mat[,16] <-out_res[,1] / 86400             # Reservoir outflow (m3/s)
    
  mat[,17] <-out_res[,11]                    # Reservoir sediment out (ton)
        
  mat[,18] <-out_resnut[,8]                  # Reservoir particulate nitrogen out (kg)
  
  mat[,19] <-out_resnut[,9]                  # Reservoir particulate phosphorus out (kg)

  mat[,20] <-out_resnut[,10]                 # Reservoir soluble phosphorus out (kg)
  
  mat[,21] <-out_resnut[,11]                 # Reservoir Chlorophyl out (kg)
  
  mat[,22] <-out_resnut[,12]                 # Reservoir NH4 out (kg)

  mat[,23] <-out_resnut[,13]                 # Reservoir NO2 out (kg)
  
  mat[,24] <-out_resnut[,14]                 # Reservoir NO3 out (kg)
  
  
  
  varoute1<-matrix(c(varoute_2=varoute_2,varoute_3=varoute_3,
                    
                    varoute_4=varoute_4,varoute_5=varoute_5,varoute_6=varoute_6,
                    
                    varoute_7=varoute_7,varoute_13=varoute_13,
                    
                    varoute_14=varoute_14,varoute_15=varoute_15,
                    
                    varoute_16=varoute_16,varoute_17=varoute_17,
                    
                    varoute_23=varoute_23,varoute_24=varoute_24,
                    
                    varoute_25=varoute_25,varoute_26=varoute_26,
                    
                    varoute_27=varoute_27,varoute_28=varoute_28),ncol=17)
  
  route.out<-list(mat=mat,varoute=varoute1)
  
  
  
  return(route.out)
  
}   




  

