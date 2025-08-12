################################################################################

# Module 3: Module 3: Stream Routing and Infrastructure Representation...

# This code routes water, nutrient and sediments on NHDPlus HR scale for one subbasin

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu 

################################################################################


# set directory ------------------------------------------------------------

setwd(system("pwd", intern = T) ) # The directory where the code is running

# read files ---------------------------------------------------------------

flowline<-readRDS('flowline.rds') # in-catchment (HUC12) NHD streams

#---- add below for filling out ksat in some cases

n_m <- which(is.na(flowline$rip_ksat)==TRUE & is.na(flowline$rip_drain_fr)==FALSE)

if(length(n_m)>0) {flowline$rip_ksat[n_m] <- mean(flowline$rip_ksat, na.rm = TRUE)} # assign average of riparian wetlands

n_m <- which(is.na(flowline$rip_ksat)==TRUE & is.na(flowline$rip_drain_fr)==FALSE)

if(length(n_m)>0) {flowline$rip_ksat[n_m] <- mean(flowline$iso_ksat, na.rm = TRUE)}  # assign average infiltration rate of isolated wetlands

n_m <- which(is.na(flowline$rip_ksat)==TRUE & is.na(flowline$rip_drain_fr)==FALSE)

if(length(n_m)>0) {flowline$rip_ksat[n_m] <- 50} # assign similar to channel bank


#-------

runpars<-readRDS('runpars.rds') # simulation parameters

# Simulation setting ---------------------------------------------------------

simulation<-list(start=runpars$start,end=runpars$end) # simulation start and end date

start.date <- as.Date(simulation$start,format="%Y-%m-%d")   

end.date <- as.Date(simulation$end,format="%Y-%m-%d")  

dates <- seq.Date(start.date, end.date, by ="day") 


# reset directory ------------------------------------------------------------

setwd(runpars$dir2)  # directory where all data are available (data folder in Roar)

# read files ------------------------------------------------------------

dam_starfit_pars <-read.csv('dam_starfit_pars.csv')  # read starfit parameters for controlled reservoirs

NHD_all<-readRDS('flowline.rds') # all NHD streams

# NHD_all$br1 <-NA  # Comment this line to incorporate reservoirs

# NHD_all$br2 <- NA  # Comment this line to incorporate reservoirs


# reset directory ------------------------------------------------------------

setwd(paste(runpars$dir3,'/O',runpars$sub,sep=""))  # directory of folder where parameter file are available

# read files ----------------------------------------------------------------

pars<-readRDS('pars.rds')                            # routing parameters 


# add common parameters for riperian wetland (We used medium removel capicity 10 m/year)


   xp <-10  # Average 

   xn <-10  # average 


  

  pars$wet_sed  <- 0      # sediment concentration in wetland  [ton/m3]
  
  pars$wet_san  <- 0 
  
  pars$wet_sil  <- 0

  pars$wet_cla  <- 0

  pars$wet_sag  <- 0
  
  pars$wet_lag  <- 0
  
  pars$wet_nsed <- 4000   # [mg/l]

  pars$wet_psol <-  0     # Soluble P available in wetland [kg]
  
  pars$wet_psed <-  0     # Sediment attached P available in wetland [kg]
  
  pars$wet_orgn <-  0     # Organic N available in wetland [kg]
  
  pars$wet_no3  <-  0     # Nitrate available in wetland [kg]
  
  pars$wet_no2  <-  0     # Nitrite available in wetland [kg]
  
  pars$wet_nh4  <-  0     # Amonium available in wetland [kg]
  
  pars$psetlw   <-  xp    # Phosphorus setelling rate [m/year] [0-20]
  
  pars$nsetlw   <-  xn    # Nitrogen setelling rate [m/year]   [0-20]

  pars$nsed <- 4000       # mg/L

  pars$res_d50   <- 10    # Micrometer (silt: 2:50 micrometer) # caliberated

  pars$psetlr1   <- xp    # m/year
  
  pars$psetlr2   <- xp    # m/year
  
  pars$nsetlr1   <- xn    # m/year
  
  pars$nsetlr2   <- xn    # m/year




#---------------------------------------------------------------------------



# Call Routing function ----------------------------------------------------------------

source("/storage/home/abh6005/work/swat/functions/Routing.R") 

# Dataframe for saving routing output --------------------------------------------------


  
  data <- as.data.frame(matrix(0,0,24))
  
  colnames(data) <- c("Dates","Reach",'I(m3/s)','O(m3/s)',
                                    
                     'SED_in(ton)',"SED_out(ton)",
                   
                     'Chlora_out(kg)','ORGN_out(kg)',

                     'NH4_out(kg)','NO2_out(kg)','NO3_out(kg)',
                   
                     'ORGP_out(kg)','SOLP_out(kg)', 'CBOD_out(kg)', 'DISOX_out(kg)',
                                      
                     'RESFLOW (m3/s)','RES_SED_out(ton)', 'RES_ORGN_out(kg)',
                   
                     'RES_ORGP_out(kg)','RES_SOLP_out(kg)','RES_CHLA_out(kg)',
                   
                     'RES_NH4_out(kg)','RES_NO2_out(kg)','RES_NO3_out(kg)') 
  
  


# set directory ------------------------------------------------------------

setwd(paste(runpars$dir1,'/R',runpars$sub,sep="")) 


# Loading ----------------------------------------------------------------

sub_loads<-readRDS('load.rds')

hydrology.mat<-sub_loads$hydrology # 1:WYLD, 2:PET, 3:PRECIP, 4:TMP_AV, 5:SOLAR, 6: GWQ  7: SURF

contaminant.mat<-sub_loads$contaminant # 1:ORGN, 2:ORGP(SED P), 3:NO3, 4:Mineral P(Soluble P), 5:NH4, 6:NO2, 7:CBOD, 8:DO, 9:Chlorophyll-a

sediment.mat<-sub_loads$sediment # 1:SYLD, ...

load<-array(NA, dim = c(length(dates),17,dim(flowline)[1])) # load matrix



# Read upstream watershed routing outputs ------------------------------------------------------------

to_ids<-c()

HUC_ups<-runpars$upstream   # HUC12 upstream catchments

kk<-0

if(length(HUC_ups)>0){


for(k in HUC_ups){

kk<-kk+1

setwd(paste(runpars$dir1,'/R',k,sep=""))

if(kk==1){

data_ups<-rbind(data,readRDS('route.rds'))  # route output for upstream HUC-12 catchments

}else{

data_ups<-rbind(data_ups,readRDS('route.rds'))  # route output for upstream HUC-12 catchments

}



} # loop

terminals<-unique(data_ups$Reach)

to_ids<-NHD_all$to_cor_id[NHD_all$core_id %in% terminals]

}



# Routing ------------------------------------------------------------

remaining <- 1:dim(flowline)[1]

completed<-c()

while(length(remaining)>0){  # while loop
  
  # cat(length(remaining), "remaining\n")  
  
  
  for(i in remaining){  # for loop

   hydrology <- NULL

   contaminant <- NULL

   sediment <- NULL
     
   ID<-flowline$core_id[i]     # NHD ID of stream

   control <- 0

   if(ID %in%  dam_starfit_pars$cor_id){

   control <-1  # indicates

   n_star <- which(dam_starfit_pars$cor_id == ID)

}


    from_ID<-flowline$from_cor_id[i]   # upstream node ID of the stream

    n_bifur<-length(which(flowline$from_cor_id==from_ID)) # inverse bifurication number

    if(n_bifur==0){n_bifur <-1} # control


    routingParams<-pars    # read routing parameters
    
    
    routingParams$ch_w2    <- flowline$ch_w2[i]              # channel bankful width [m]
    
    routingParams$ch_d     <- flowline$ch_d[i]               # channel bankful depth [m]
    
    routingParams$ch_s2    <- flowline$ch_s2[i]              # channel slope [-]
    
    routingParams$ch_l2    <- flowline$LengthKM[i]           # channel length [m]
    
    routingParams$ch_n2    <- flowline$ch_n2[i]          # channel Manning roughness 
        
    routingParams$iyres    <- flowline$iyres[i]              # reservoir operational year
    
    routingParams$res_evol <- flowline$res_evol[i]           # reservoir maximum storage [m3]
    
    routingParams$res_pvol <- flowline$res_pvol[i]           # reservoir normal storage [m3]

    routingParams$res_vol  <- flowline$res_pvol[i]           # reservoir initial storage [m3]
    
    routingParams$res_esa  <- 1.5 * flowline$res_psa[i]      # reservoir surface area at maximum level [hectare]
    
    routingParams$res_psa  <- flowline$res_psa[i]            # reservoir surface area at normal level [hectare]
    
    routingParams$res_rr   <- flowline$res_rr[i]             # reservoir maximum discharge [m3/day]
        
    routingParams$br1      <- flowline$br1[i]                # shape parameter
    
    routingParams$br2      <- flowline$br2[i]                # shape parameter
    
    routingParams$phi_1    <- flowline$phi_1[i]              # bankfull area [m2]
    
    routingParams$phi_6    <- flowline$phi_6[i]              # channel bed [m]
    
    routingParams$phi_10   <- flowline$phi_10[i]
    
    routingParams$phi_13   <- flowline$phi_13[i]

    #routingParams$evrsv    <- 0.6

    routingParams$ndtargr  <- 1

    routingParams$ch_k2    <- 3.81  

    #routingParams$ch_k2    <- 50     # hydraulic conductivity of channel [mm/hr] calibrated  

   # routingParams$alpha_bnk <- 0.6   # channel bank storage  caliberated
   
               
   # routingParams$evrch <- 0.6
   
   routingParams$spexp <- 1.5        # sediment calibration
   
   routingParams$spcon <- 0.0001      # sediment calibration


   routingParams$res_k  <-   routingParams$ch_k2      # hydraulic conductivity of reservoir [mm/hr]
   
   routingParams$rch_cbod <- 0       # initial cbod in stream
  
   routingParams$rch_dox  <- 0       # initial dissolved oxygen in stream
   


   routingParams$control <- 0


  if(control==1){

routingParams$control <- 1

routingParams$hi_mu <- dam_starfit_pars$NORhi_mu[n_star]

routingParams$hi_alpha <- dam_starfit_pars$NORhi_alpha[n_star]

routingParams$hi_beta <- dam_starfit_pars$NORhi_beta[n_star]

routingParams$hi_min <- dam_starfit_pars$NORhi_min[n_star]

routingParams$hi_max <- dam_starfit_pars$NORhi_max[n_star]


routingParams$lo_mu <- dam_starfit_pars$NORlo_mu[n_star]

routingParams$lo_alpha <- dam_starfit_pars$NORlo_alpha[n_star]

routingParams$lo_beta <- dam_starfit_pars$NORlo_beta[n_star]

routingParams$lo_min <- dam_starfit_pars$NORlo_min[n_star]

lo_max <- routingParams$lo_max <- dam_starfit_pars$NORlo_max[n_star]

routingParams$Release_alpha1 <- dam_starfit_pars$Release_alpha1[n_star]

routingParams$Release_alpha2 <- dam_starfit_pars$Release_alpha2[n_star]

routingParams$Release_beta1 <- dam_starfit_pars$Release_beta1[n_star]

routingParams$Release_beta2 <- dam_starfit_pars$Release_beta2[n_star]

routingParams$Release_c <- dam_starfit_pars$Release_c[n_star]

routingParams$Release_p1 <- dam_starfit_pars$Release_p1[n_star]

routingParams$Release_p2 <- dam_starfit_pars$Release_p2[n_star]

}


#---- riperian wetland parameters


    routingParams$wet_fr <- flowline$rip_drain_fr[i]         # wetland drainage fraction [-]

    routingParams$wet_vol <- flowline$rip_storage[i]   # volume of water in wetland at begining of day [m3]

    routingParams$wet_nvol <- flowline$rip_storage[i]  # wetland normal storage [m3]

    routingParams$wet_mxvol<- flowline$rip_storage[i]        # wetland maximum storage [m3]

    routingParams$wet_nsa  <- flowline$rip_area[i]      # wetland surface area at normal level [hectare]

    routingParams$wet_mxsa <- flowline$rip_area[i]           # wetland surface area at maximum level [hectare]

    routingParams$wet_k    <- flowline$rip_ksat[i]         # hydraulic conductivity of soil at the place of wetland [mm/hr]

    routingParams$sed_stl  <- flowline$sed_stl[i]           # fraction of sediment remaining [-] 
    
   routingParams$eps <- 0.01 * flowline$ch_d[i]             # Liu convergence parameter  



 # routingParams$wet_fr <- NA  # Comment this line to apply riperian wetland


 # flowline$br1[i] <- NA # Comment this line to apply reservoirs

    
 if(is.na(flowline$br1[i])==FALSE & is.na(flowline$br2[i])==FALSE & is.infinite(flowline$br1[i])==FALSE & is.infinite(flowline$br2[i])==FALSE ){
   
   res_option<-1 # stream segment is regulated by a reservoir

 
   
 }else{
   
   res_option<-0 # no reservoir on stream segment
   
 }
   
    
    
    upstream<-which(flowline$to_cor_id==from_ID)  # internal upstreams
    
    
    
    if(length(upstream)>0){ # not head watershed 
      
      if(all(upstream %in% completed)==TRUE){ # all upstream watersheds routed
        
        
        completed<-c(completed,i)
        
        completed<-unique(completed)


        r1 <- routingParams$wet_fr

       if(is.na(r1) == TRUE) {r1 <- 0}

       r2 <- 1 - r1


# initialize

hydrology <- 0

contaminant <- 0

sediment <- 0

        
        
       # hydrology <- list(varoute_2=hydrology.mat[,1,i] * r2 ,PET=hydrology.mat[,2,i],  PRECIP=hydrology.mat[,3,i],
                          
                        #  T_ave=hydrology.mat[,4,i],solar_rad=hydrology.mat[,5,i])
                          
                          
                        hydrology <- list(varoute_2=(hydrology.mat[,1,i] * r2 + hydrology.mat[,6,i])  ,PET=hydrology.mat[,2,i],  PRECIP=hydrology.mat[,3,i],
                          
                         T_ave=hydrology.mat[,4,i],solar_rad=hydrology.mat[,5,i])
        
        
        
        contaminant <- list(varoute_4=contaminant.mat[,1,i] * r2,varoute_5=contaminant.mat[,2,i]* r2,
                            
                            varoute_6=contaminant.mat[,3,i]* r2, varoute_7=contaminant.mat[,4,i]* r2, varoute_14=contaminant.mat[,5,i]* r2,
                            
                            varoute_15=contaminant.mat[,6,i]* r2,varoute_16=contaminant.mat[,7,i],varoute_17=contaminant.mat[,8,i],
                            
                            varoute_13=contaminant.mat[,9,i])

# CHLA, CBOD and DO directly goes to stream
        
        
        sediment <- list(varoute_3=sediment.mat[,1,i]* r2,varoute_23=sediment.mat[,2,i]* r2,varoute_24=sediment.mat[,3,i]* r2,varoute_25=sediment.mat[,4,i]* r2,
                         varoute_28=sediment.mat[,5,i]* r2,varoute_26=sediment.mat[,6,i]* r2,varoute_27=sediment.mat[,7,i]* r2)
        
        
### load from upland area to riparian wetland

        wat_yld <- hydrology.mat[,1,i]   * r1
  
        sed_yld <- sediment.mat[,1,i]    * r1

        san_yld <- sediment.mat[,2,i]    * r1

        sil_yld <- sediment.mat[,3,i]    * r1
                           
        cla_yld <- sediment.mat[,4,i]    * r1

        sag_yld <- sediment.mat[,6,i]    * r1

        lag_yld <- sediment.mat[,7,i]    * r1

        psol_yld <- contaminant.mat[,4,i] * r1
 
        psed_yld <- contaminant.mat[,2,i] * r1
                            
        orgn_yld <- contaminant.mat[,1,i] * r1
 
        no3_yld  <- contaminant.mat[,3,i] * r1


        
        
        for(j in upstream){ # internal loop j
          
          
          # adding upstream loads
          
          hydrology$varoute_2 <- hydrology$varoute_2 +load[,1,j] / n_bifur
          
          contaminant$varoute_4 <- contaminant$varoute_4 +load[,3,j] / n_bifur
          
          contaminant$varoute_5 <- contaminant$varoute_5 +load[,4,j] / n_bifur
          
          contaminant$varoute_6 <- contaminant$varoute_6 +load[,5,j] / n_bifur
          
          contaminant$varoute_7 <- contaminant$varoute_7 +load[,6,j] / n_bifur
          
          contaminant$varoute_13 <- contaminant$varoute_13 +load[,7,j] / n_bifur
          
          contaminant$varoute_14 <- contaminant$varoute_14 +load[,8,j] / n_bifur
          
          contaminant$varoute_15 <- contaminant$varoute_15 +load[,9,j] / n_bifur
          
          contaminant$varoute_16 <- contaminant$varoute_16 +load[,10,j] / n_bifur
          
          contaminant$varoute_17 <- contaminant$varoute_17 +load[,11,j] / n_bifur
          
          sediment$varoute_3 <- sediment$varoute_3 +load[,2,j] / n_bifur
          
          sediment$varoute_23 <- sediment$varoute_23 +load[,12,j] / n_bifur
          
          sediment$varoute_24 <- sediment$varoute_24 +load[,13,j] / n_bifur
          
          sediment$varoute_25 <- sediment$varoute_25 +load[,14,j] / n_bifur
          
          sediment$varoute_26 <- sediment$varoute_26 +load[,15,j] / n_bifur
          
          sediment$varoute_27 <- sediment$varoute_27 +load[,16,j] / n_bifur
          
          sediment$varoute_28 <- sediment$varoute_28 +load[,17,j] / n_bifur
          
          
          
        } # end of internal loop j


# external terminal --------------------------------------------------------------------------------------------
     


if(from_ID %in% to_ids){ # if 101: if upstream branch is external streams

dummy1<-NHD_all$core_id[which(NHD_all$to_cor_id==from_ID)] 

dummy1<-dummy1[which(dummy1 %in% unique(data_ups$Reach))]

for(j in dummy1){ # 2nd for over j

dummy2<-subset(data_ups,Reach==j)

 if(is.na(NHD_all$br1[which(NHD_all$core_id==j)])==TRUE | is.na(NHD_all$br2[which(NHD_all$core_id==j)])==TRUE | 

is.infinite(NHD_all$br1[which(NHD_all$core_id==j)])==TRUE | is.infinite(NHD_all$br2[which(NHD_all$core_id==j)])==TRUE){  # without reservoir


hydrology$varoute_2 <- hydrology$varoute_2 + dummy2$`O(m3/s)`*86400 / n_bifur
          
          contaminant$varoute_4 <- contaminant$varoute_4 + dummy2$`ORGN_out(kg)` / n_bifur
          
          contaminant$varoute_5 <- contaminant$varoute_5 + dummy2$`ORGP_out(kg)` / n_bifur
          
          contaminant$varoute_6 <- contaminant$varoute_6 + dummy2$`NO3_out(kg)` / n_bifur
          
          contaminant$varoute_7 <- contaminant$varoute_7 + dummy2$`SOLP_out(kg)` / n_bifur
          
          contaminant$varoute_13 <- contaminant$varoute_13 + dummy2$`Chlora_out(kg)` / n_bifur
          
          contaminant$varoute_14 <- contaminant$varoute_14 + dummy2$`NH4_out(kg)` / n_bifur
          
          contaminant$varoute_15 <- contaminant$varoute_15 + dummy2$`NO2_out(kg)` / n_bifur
          
          contaminant$varoute_16 <- contaminant$varoute_16 + dummy2$`CBOD_out(kg)` / n_bifur
          
          contaminant$varoute_17 <- contaminant$varoute_17 + dummy2$`DISOX_out(kg)` / n_bifur
          
          sediment$varoute_3 <- sediment$varoute_3 + dummy2$`SED_out(ton)` / n_bifur
          
          sediment$varoute_23 <- rep(0,length(dates))
          
          sediment$varoute_24 <- rep(0,length(dates))
          
          sediment$varoute_25 <- rep(0,length(dates))
          
          sediment$varoute_26 <- rep(0,length(dates))
          
          sediment$varoute_27 <- rep(0,length(dates))
          
          sediment$varoute_28 <- rep(0,length(dates))

}else{ # with reservoir

hydrology$varoute_2 <- hydrology$varoute_2 + dummy2$`RESFLOW (m3/s)`*86400 / n_bifur
          
          contaminant$varoute_4 <- contaminant$varoute_4 + dummy2$`RES_ORGN_out(kg)` / n_bifur
          
          contaminant$varoute_5 <- contaminant$varoute_5 + dummy2$`RES_ORGP_out(kg)` / n_bifur
          
          contaminant$varoute_6 <- contaminant$varoute_6 + dummy2$`RES_NO3_out(kg)` / n_bifur
          
          contaminant$varoute_7 <- contaminant$varoute_7 + dummy2$`RES_SOLP_out(kg)` / n_bifur
          
          contaminant$varoute_13 <- contaminant$varoute_13 + dummy2$`RES_CHLA_out(kg)` / n_bifur
          
          contaminant$varoute_14 <- contaminant$varoute_14 + dummy2$`RES_NH4_out(kg)` / n_bifur
          
          contaminant$varoute_15 <- contaminant$varoute_15 + dummy2$`RES_NO2_out(kg)` / n_bifur

          contaminant$varoute_16 <- rep(0,length(dates))

          contaminant$varoute_17 <- rep(0,length(dates))

          
          sediment$varoute_3 <- sediment$varoute_3 + dummy2$`RES_SED_out(ton)` / n_bifur
          
          sediment$varoute_23 <- rep(0,length(dates))
          
          sediment$varoute_24 <- rep(0,length(dates))
          
          sediment$varoute_25 <- rep(0,length(dates))
          
          sediment$varoute_26 <- rep(0,length(dates))
          
          sediment$varoute_27 <- rep(0,length(dates))
          
          sediment$varoute_28 <- rep(0,length(dates))





} # else


} # for over 2nd j


} # if 101


#-----------------------------------------------------------------------------------------------------

if(control==1){  # additional parameters for controlled reservoirs

routingParams$Release_max <- quantile(hydrology$varoute_2,0.99) # maximum flow

routingParams$Release_min <- quantile(hydrology$varoute_2,0.01)  # environmental flow (1th percintile of inflow)

routingParams$I_bar <- mean(hydrology$varoute_2)    # long term average flow

}


        
        if(is.na(flowline$ch_s2[i])==FALSE & is.na(flowline$ch_n2[i])==FALSE & is.na(flowline$TotDASqKm[i])==FALSE){   

        if(flowline$ch_s2[i]>0 & flowline$ch_n2[i]>0 & flowline$TotDASqKm[i]>0){
        
        route.out <-  Routing(hydrology,sediment,contaminant,wat_yld,sed_yld,san_yld,

                            sil_yld,cla_yld,sag_yld,lag_yld,psol_yld,psed_yld,orgn_yld, no3_yld,

                             routingParams,simulation,nsub=ID,res_option=res_option)
        
        data<-rbind(data,route.out$mat)
        
        load[,,i]<-route.out$varoute
        
        }else{
          
          
same<- as.data.frame(matrix(nrow=length(dates),ncol=24))
        
        colnames(same)<-colnames(data)


same[,c(4,6,7:24)]<- matrix(c(hydrology$varoute_2 / 86400,
                                                                    
              sediment$varoute_3,contaminant$varoute_13,
              
              contaminant$varoute_4,contaminant$varoute_14,contaminant$varoute_15,contaminant$varoute_6,
              
              contaminant$varoute_5,contaminant$varoute_7,contaminant$varoute_16,contaminant$varoute_17,
              
              hydrology$varoute_2 / 86400, sediment$varoute_3,contaminant$varoute_4,
              
              contaminant$varoute_5,contaminant$varoute_7,contaminant$varoute_13,contaminant$varoute_14,
              
              contaminant$varoute_15,contaminant$varoute_6),nrow=length(dates),ncol=20)

              
          same[,1]<-dates
          
          same[,2]<-rep(ID,length(dates))


          
          
         data<-rbind(data,same) 
         
        load[,,i]<-matrix(c(hydrology$varoute_2,sediment$varoute_3,contaminant$varoute_4,

              contaminant$varoute_5,contaminant$varoute_6,

              contaminant$varoute_7,contaminant$varoute_13,contaminant$varoute_14,

               contaminant$varoute_15,contaminant$varoute_16,

               contaminant$varoute_17,sediment$varoute_23,sediment$varoute_24,

              sediment$varoute_25,sediment$varoute_26,sediment$varoute_27,

              sediment$varoute_28),nrow=length(dates),ncol=17)
     
} # else


                  
         }else{


 
same<- as.data.frame(matrix(nrow=length(dates),ncol=24))
        
        colnames(same)<-colnames(data)
        
        same[,c(4,6,7:24)]<- matrix(c(hydrology$varoute_2 / 86400,
                                                                    
              sediment$varoute_3,contaminant$varoute_13,
              
              contaminant$varoute_4,contaminant$varoute_14,contaminant$varoute_15,contaminant$varoute_6,
              
              contaminant$varoute_5,contaminant$varoute_7,contaminant$varoute_16,contaminant$varoute_17,
              
              hydrology$varoute_2 / 86400, sediment$varoute_3,contaminant$varoute_4,
              
              contaminant$varoute_5,contaminant$varoute_7,contaminant$varoute_13,contaminant$varoute_14,
              
              contaminant$varoute_15,contaminant$varoute_6),nrow=length(dates),ncol=20)

                        
          same[,1]<-dates
          
          same[,2]<-rep(ID,length(dates))



          
          
         data<-rbind(data,same) 
         
         
load[,,i]<-matrix(c(hydrology$varoute_2,sediment$varoute_3,contaminant$varoute_4,

              contaminant$varoute_5,contaminant$varoute_6,

              contaminant$varoute_7,contaminant$varoute_13,contaminant$varoute_14,

               contaminant$varoute_15,contaminant$varoute_16,

               contaminant$varoute_17,sediment$varoute_23,sediment$varoute_24,

              sediment$varoute_25,sediment$varoute_26,sediment$varoute_27,

              sediment$varoute_28),nrow=length(dates),ncol=17)


} ##
         
         
          
       
        
        
        
      }
      
      
      
    }else{   # internally headwatershed
	

      completed<-c(completed,i)
      
      completed<-unique(completed)
      
       r1 <- routingParams$wet_fr

       if(is.na(r1) == TRUE) {r1 <- 0}

       r2 <- 1 - r1


# initialize

hydrology <- 0

contaminant <- 0

sediment <- 0
      
      
    #  hydrology <- list(varoute_2=hydrology.mat[,1,i] * r2 ,PET=hydrology.mat[,2,i],  PRECIP=hydrology.mat[,3,i],
                        
                  #      T_ave=hydrology.mat[,4,i],solar_rad=hydrology.mat[,5,i])
                        
                         hydrology <- list(varoute_2=(hydrology.mat[,1,i] * r2 + hydrology.mat[,6,i])  ,PET=hydrology.mat[,2,i],  PRECIP=hydrology.mat[,3,i],
                          
                         T_ave=hydrology.mat[,4,i],solar_rad=hydrology.mat[,5,i])
      
      
      contaminant <- list(varoute_4=contaminant.mat[,1,i] * r2,varoute_5=contaminant.mat[,2,i] * r2,
                          
                          varoute_6=contaminant.mat[,3,i] * r2, varoute_7=contaminant.mat[,4,i] * r2, varoute_14=contaminant.mat[,5,i] * r2,
                          
                          varoute_15=contaminant.mat[,6,i] * r2,varoute_16=contaminant.mat[,7,i],varoute_17=contaminant.mat[,8,i],
                          
                          varoute_13=contaminant.mat[,9,i])

# CHLA, CBOD and DO directly goes to stream
      
      
      sediment <- list(varoute_3=sediment.mat[,1,i] * r2,varoute_23=sediment.mat[,2,i] * r2,varoute_24=sediment.mat[,3,i] * r2,varoute_25=sediment.mat[,4,i] * r2,
                       varoute_28=sediment.mat[,5,i] * r2,varoute_26=sediment.mat[,6,i] * r2,varoute_27=sediment.mat[,7,i] * r2)



### load from upland area to riparian wetland

        wat_yld <- hydrology.mat[,1,i]   * r1
  
        sed_yld <- sediment.mat[,1,i]    * r1

        san_yld <- sediment.mat[,2,i]    * r1

        sil_yld <- sediment.mat[,3,i]    * r1
                           
        cla_yld <- sediment.mat[,4,i]    * r1

        sag_yld <- sediment.mat[,6,i]    * r1

        lag_yld <- sediment.mat[,7,i]    * r1

        psol_yld <- contaminant.mat[,4,i] * r1
 
        psed_yld <- contaminant.mat[,2,i] * r1
                            
        orgn_yld <- contaminant.mat[,1,i] * r1
 
        no3_yld  <- contaminant.mat[,3,i] * r1




      
 
# external terminal --------------------------------------------------------------------------------------------
     


if(from_ID %in% to_ids){ # if 101

dummy1<-NHD_all$core_id[which(NHD_all$to_cor_id==from_ID)] 

dummy1<-dummy1[which(dummy1 %in% unique(data_ups$Reach))]

for(j in dummy1){ # 2nd for over j

dummy2<-subset(data_ups,Reach==j)

 
if(is.na(NHD_all$br1[which(NHD_all$core_id==j)])==TRUE | is.na(NHD_all$br2[which(NHD_all$core_id==j)])==TRUE | 

is.infinite(NHD_all$br1[which(NHD_all$core_id==j)])==TRUE | is.infinite(NHD_all$br2[which(NHD_all$core_id==j)])==TRUE){  # without reservoir




hydrology$varoute_2 <- hydrology$varoute_2 + dummy2$`O(m3/s)`*86400 / n_bifur
          
          contaminant$varoute_4 <- contaminant$varoute_4 + dummy2$`ORGN_out(kg)` / n_bifur
          
          contaminant$varoute_5 <- contaminant$varoute_5 + dummy2$`ORGP_out(kg)` / n_bifur
          
          contaminant$varoute_6 <- contaminant$varoute_6 + dummy2$`NO3_out(kg)` / n_bifur
          
          contaminant$varoute_7 <- contaminant$varoute_7 + dummy2$`SOLP_out(kg)` / n_bifur
          
          contaminant$varoute_13 <- contaminant$varoute_13 + dummy2$`Chlora_out(kg)` / n_bifur
          
          contaminant$varoute_14 <- contaminant$varoute_14 + dummy2$`NH4_out(kg)` / n_bifur
          
          contaminant$varoute_15 <- contaminant$varoute_15 + dummy2$`NO2_out(kg)` / n_bifur
          
          contaminant$varoute_16 <- contaminant$varoute_16 + dummy2$`CBOD_out(kg)` / n_bifur
          
          contaminant$varoute_17 <- contaminant$varoute_17 + dummy2$`DISOX_out(kg)` / n_bifur


          
          sediment$varoute_3 <- sediment$varoute_3 + dummy2$`SED_out(ton)` / n_bifur
          
          sediment$varoute_23 <- rep(0,length(dates))
          
          sediment$varoute_24 <- rep(0,length(dates))
          
          sediment$varoute_25 <- rep(0,length(dates))
          
          sediment$varoute_26 <- rep(0,length(dates))
          
          sediment$varoute_27 <- rep(0,length(dates))
          
          sediment$varoute_28 <- rep(0,length(dates))

}else{ # with reservoir

hydrology$varoute_2 <- hydrology$varoute_2 + dummy2$`RESFLOW (m3/s)`*86400 / n_bifur
          
          contaminant$varoute_4 <- contaminant$varoute_4 + dummy2$`RES_ORGN_out(kg)` / n_bifur
          
          contaminant$varoute_5 <- contaminant$varoute_5 + dummy2$`RES_ORGP_out(kg)` / n_bifur
          
          contaminant$varoute_6 <- contaminant$varoute_6 + dummy2$`RES_NO3_out(kg)` / n_bifur
          
          contaminant$varoute_7 <- contaminant$varoute_7 + dummy2$`RES_SOLP_out(kg)` / n_bifur
          
          contaminant$varoute_13 <- contaminant$varoute_13 + dummy2$`RES_CHLA_out(kg)` / n_bifur
          
          contaminant$varoute_14 <- contaminant$varoute_14 + dummy2$`RES_NH4_out(kg)` / n_bifur
          
          contaminant$varoute_15 <- contaminant$varoute_15 + dummy2$`RES_NO2_out(kg)` / n_bifur

          contaminant$varoute_16 <- rep(0,length(dates))
          
          contaminant$varoute_17 <- rep(0,length(dates))

          
          
          sediment$varoute_3 <- sediment$varoute_3 + dummy2$`RES_SED_out(ton)` / n_bifur
          
          sediment$varoute_23 <- rep(0,length(dates))
          
          sediment$varoute_24 <- rep(0,length(dates))
          
          sediment$varoute_25 <- rep(0,length(dates))
          
          sediment$varoute_26 <- rep(0,length(dates))
          
          sediment$varoute_27 <- rep(0,length(dates))
          
          sediment$varoute_28 <- rep(0,length(dates))





}


} # for over 2nd j


} # if 101


#-----------------------------------------------------------------------------------------------------

if(control==1){  # additional parameters for controlled reservoirs

routingParams$Release_max <- quantile(hydrology$varoute_2,0.99) # maximum flow

routingParams$Release_min <- quantile(hydrology$varoute_2,0.01)  # environmental flow

routingParams$I_bar <- mean(hydrology$varoute_2)    # long term average flow

}


      
       if(is.na(flowline$ch_s2[i])==FALSE & is.na(flowline$ch_n2[i])==FALSE & is.na(flowline$TotDASqKm[i])==FALSE){ 

      if(flowline$ch_s2[i]>0 & flowline$ch_n2[i]>0 & flowline$TotDASqKm[i]>0){
        
        
      
      route.out <-  Routing(hydrology,sediment,contaminant,wat_yld,sed_yld,san_yld,

                            sil_yld,cla_yld,sag_yld,lag_yld,psol_yld,psed_yld,orgn_yld, no3_yld,

                             routingParams,simulation,nsub=ID,res_option=res_option)

      
      data<-rbind(data,route.out$mat)
      
      load[,,i]<-route.out$varoute
      
      
      }else{
        
        same<- as.data.frame(matrix(nrow=length(dates),ncol=24))
        
        colnames(same)<-colnames(data)
        
        same[,c(4,6,7:24)]<- matrix(c(hydrology$varoute_2,
                                                                    
              sediment$varoute_3,contaminant$varoute_13,
              
              contaminant$varoute_4,contaminant$varoute_14,contaminant$varoute_15,contaminant$varoute_6,
              
              contaminant$varoute_5,contaminant$varoute_7,contaminant$varoute_16,contaminant$varoute_17,
              
              hydrology$varoute_2, sediment$varoute_3,contaminant$varoute_4,
              
              contaminant$varoute_5,contaminant$varoute_7,contaminant$varoute_13,contaminant$varoute_14,
              
              contaminant$varoute_15,contaminant$varoute_6),nrow=length(dates),ncol=20)

                        
          same[,1]<-dates
          
          same[,2]<-rep(ID,length(dates))


        
       data<-rbind(data,same)
       
       
       load[,,i]<-matrix(c(hydrology$varoute_2,sediment$varoute_3,contaminant$varoute_4,

              contaminant$varoute_5,contaminant$varoute_6,

              contaminant$varoute_7,contaminant$varoute_13,contaminant$varoute_14,

               contaminant$varoute_15,contaminant$varoute_16,

               contaminant$varoute_17,sediment$varoute_23,sediment$varoute_24,

              sediment$varoute_25,sediment$varoute_26,sediment$varoute_27,

              sediment$varoute_28),nrow=length(dates),ncol=17)

       
      }

}else{

 same<- as.data.frame(matrix(nrow=length(dates),ncol=24))
        
        colnames(same)<-colnames(data)
        
        same[,c(4,6,7:24)]<- matrix(c(hydrology$varoute_2,
                                                                    
              sediment$varoute_3,contaminant$varoute_13,
              
              contaminant$varoute_4,contaminant$varoute_14,contaminant$varoute_15,contaminant$varoute_6,
              
              contaminant$varoute_5,contaminant$varoute_7,contaminant$varoute_16,contaminant$varoute_17,
              
              hydrology$varoute_2, sediment$varoute_3,contaminant$varoute_4,
              
              contaminant$varoute_5,contaminant$varoute_7,contaminant$varoute_13,contaminant$varoute_14,
              
              contaminant$varoute_15,contaminant$varoute_6),nrow=length(dates),ncol=20)

                        
          same[,1]<-dates
          
          same[,2]<-rep(ID,length(dates))
        
       data<-rbind(data,same)
       
       
       load[,,i]<-matrix(c(hydrology$varoute_2,sediment$varoute_3,contaminant$varoute_4,

              contaminant$varoute_5,contaminant$varoute_6,

              contaminant$varoute_7,contaminant$varoute_13,contaminant$varoute_14,

               contaminant$varoute_15,contaminant$varoute_16,

               contaminant$varoute_17,sediment$varoute_23,sediment$varoute_24,

              sediment$varoute_25,sediment$varoute_26,sediment$varoute_27,

              sediment$varoute_28),nrow=length(dates),ncol=17)




}


      
     
      
    }
    
    
    
  }  # end of external for loop
  
  remaining<-setdiff(remaining,completed)
  
  remaining<-unique(remaining)
  
  
} # end of while loop



# set directory ------------------------------------------------------------

setwd(paste(runpars$dir1,'/R',runpars$sub,sep=""))


# save routing file ------------------------------------------------------------

saveRDS(data,'route.rds')

finish_route <-1

saveRDS(finish_route,'finish_route.rds')


