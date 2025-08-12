################################################################################

# Water, sediment and nutrient routing across the basin  (HUC12)

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu 

################################################################################

#---------------------------------------------------------------------------------------

source(".../functions/Routing.R") # call Routing function

############################Load libraries######################################

# Set R library directory here ...

library(sp)

library(sf)

library(readr)

############################## Body#############################################
dir<-".../swat"   # output folders directory

num <- 911 # number of subbasins

res_option <-0 # 0:remove reservoirs 1:include reservoirs

subbasins <- 1:num


simulation<-list(start='2025-01-01',end='2059-12-31') # simulation start and end date   

start.date <- as.Date(simulation$start,format="%Y-%m-%d")   

end.date <- as.Date(simulation$end,format="%Y-%m-%d")  

dates <- seq.Date(start.date, end.date, by ="day") 


hydrology.mat<-array(0, dim = c(length(dates),5,num)) # hydrology load matrix

contaminant.mat<-array(0, dim = c(length(dates),9,num)) # contaminant load matrix

sediment.mat <- array(0, dim = c(length(dates),7,num)) # sediment load matrix




for(i in subbasins){  # for loop




  setwd(paste(dir,'/O',i,sep = ""))   # set path to read load
  
  
  routingParams<-readRDS('pars.rds') 
  
  rch <- read.table('output.rch',skip=9)
  
  hru<-read.table('output.hru',skip=9)
  
  sub <-read.table('output.sub',skip=9) 
  
  
  hydrology.mat[,1,i] <- rch$V6 *86400   # i  subbasin & 1:  inflow volume # code: 1
  
  hydrology.mat[,2,i] <- sub$V6       # 2:  potential evaporation  # code: 3
  
  hydrology.mat[,3,i] <- sub$V5      #  3:  percipitation  # code: 1
  
  hydrology.mat[,4,i] <- hru$V7   # 4: temperature 
  
  hydrology.mat[,5,i] <- hru$V8      # 5: SOLAR  # code: 28

  contaminant.mat[,1,i] <- rch$V13 # ORGN

  contaminant.mat[,2,i] <-  rch$V15  # ORGP

   contaminant.mat[,3,i] <- rch$V17  # NO3

   contaminant.mat[,4,i] <- rch$V23  # SOLP

  contaminant.mat[,5,i] <-   rch$V19  # NH4

  contaminant.mat[,6,i] <- rch$V21 #NO2

  contaminant.mat[,7,i] <- rch$V27 # CBOD

  contaminant.mat[,8,i] <- rch$V29 # DISOX

  contaminant.mat[,9,i] <-rch$V25 #  CHLIN
  
  sediment.mat[,1,i] <- rch$V10  # sediment
  
  sediment.mat[,2,i] <- rep(0,length(dates))
  
  sediment.mat[,3,i] <-  rep(0,length(dates))
  
  sediment.mat[,4,i] <-  rep(0,length(dates))
  
  sediment.mat[,5,i] <-  rep(0,length(dates))
  
  sediment.mat[,6,i] <-  rep(0,length(dates))
  
  sediment.mat[,7,i] <-  rep(0,length(dates))
  
 
} # end of forming subbasin load matrix



# stream connection 

fig <- st_read(".../data/huc12_streams.shp")   # SWAT configuration file 


load<-array(0, dim = c(length(dates),17,num))


remaining<-subbasins

completed<-c()


while(length(remaining)>0){  # while loop
  
 # cat(length(remaining), "remaining\n")  
  
  
  for(i in remaining){  # for loop


   hydrology <- 0
    
   contaminant<-0
    
   sediment <- 0
    
  
    
    nsub<-i
    
    setwd(paste(dir,'/O',i,sep = ""))   # set path to read load
	
	
	# change channel parameters (for caliberation)
    
    
    routingParams<-readRDS('pars.rds')  

    routingParams$ch_n2  <- 0.03   

    routingParams$ch_k2 <- 3.81

    routingParams$spcon <-  0.0001

    routingParams$spexp <- 1.5

 
         
    upstream<-fig$Subbasin[which(fig$SubbasinR==nsub)] # upstream watersheds
    
    
    if(length(upstream)>0){ # not head watershed
      
      if(all(upstream %in% completed)==TRUE){ # all upstream watersheds routed
        
        completed<-c(completed,nsub)
               
       
       setwd(paste(dir,'/O',i,sep = ""))   # set path for saving output
       
    
       
       hydrology <- list(varoute_2=hydrology.mat[,1,nsub] ,PET=hydrology.mat[,2,nsub],  PRECIP=hydrology.mat[,3,nsub],
                         
                        T_ave=hydrology.mat[,4,nsub],solar_rad=hydrology.mat[,5,nsub])
       
       
      contaminant <- list(varoute_4=contaminant.mat[,1,nsub],varoute_5=contaminant.mat[,2,nsub],
                           
                           varoute_6=contaminant.mat[,3,nsub], varoute_7=contaminant.mat[,4,nsub], varoute_14=contaminant.mat[,5,nsub],
                           
                           varoute_15=contaminant.mat[,6,nsub],varoute_16=contaminant.mat[,7,nsub],varoute_17=contaminant.mat[,8,nsub],
                           
                           varoute_13=contaminant.mat[,9,nsub])
       
       
       sediment <- list(varoute_3=sediment.mat[,1,nsub],varoute_23=sediment.mat[,2,nsub],varoute_24=sediment.mat[,3,nsub],varoute_25=sediment.mat[,4,nsub],
                        varoute_28=sediment.mat[,5,nsub],varoute_26=sediment.mat[,6,nsub],varoute_27=sediment.mat[,7,nsub])
       
       
       
       
        
        for(j in upstream){ # internal loop j
          
          
          # adding upstream loads
        
          hydrology$varoute_2 <- hydrology$varoute_2 +load[,1,j]
          
          contaminant$varoute_4 <- contaminant$varoute_4 +load[,3,j]
          
          contaminant$varoute_5 <- contaminant$varoute_5 +load[,4,j]
          
          contaminant$varoute_6 <- contaminant$varoute_6 +load[,5,j]
          
          contaminant$varoute_7 <- contaminant$varoute_7 +load[,6,j]
          
          contaminant$varoute_13 <- contaminant$varoute_13 +load[,7,j]
          
          contaminant$varoute_14 <- contaminant$varoute_14 +load[,8,j]
          
          contaminant$varoute_15 <- contaminant$varoute_15 +load[,9,j]
          
          contaminant$varoute_16 <- contaminant$varoute_16 +load[,10,j]
          
          contaminant$varoute_17 <- contaminant$varoute_17 +load[,11,j]
          
          sediment$varoute_3 <- sediment$varoute_3 +load[,2,j]
          
          sediment$varoute_23 <- sediment$varoute_23 +load[,12,j]
          
          sediment$varoute_24 <- sediment$varoute_24 +load[,13,j]
          
          sediment$varoute_25 <- sediment$varoute_25 +load[,14,j]
          
          sediment$varoute_26 <- sediment$varoute_26 +load[,15,j]
          
          sediment$varoute_27 <- sediment$varoute_27 +load[,16,j]
          
          sediment$varoute_28 <- sediment$varoute_28 +load[,17,j]
          
          
      
        } # end of internal loop j
        
        
        route.out <-  Routing(hydrology,sediment,contaminant,routingParams,
                              
                              simulation,nsub,res_option)


        saveRDS(route.out$mat,'output.rds')
        
        load[,,nsub]<-route.out$varoute
        
      }
  
      
      
    }else{
      
      completed<-c(completed,nsub)
      
            
      setwd(paste(dir,'/O',i,sep = ""))   # set path for saving output
      
      
      hydrology <- list(varoute_2=hydrology.mat[,1,nsub] ,PET=hydrology.mat[,2,nsub],  PRECIP=hydrology.mat[,3,nsub],
                        
                        T_ave=hydrology.mat[,4,nsub],solar_rad=hydrology.mat[,5,nsub])
      
      
      contaminant <- list(varoute_4=contaminant.mat[,1,nsub],varoute_5=contaminant.mat[,2,nsub],
                          
                         varoute_6=contaminant.mat[,3,nsub], varoute_7=contaminant.mat[,4,nsub], varoute_14=contaminant.mat[,5,nsub],
                          
                         varoute_15=contaminant.mat[,6,nsub],varoute_16=contaminant.mat[,7,nsub],varoute_17=contaminant.mat[,8,nsub],
                          
                         varoute_13=contaminant.mat[,9,nsub])
      
      
      sediment <- list(varoute_3=sediment.mat[,1,nsub],varoute_23=sediment.mat[,2,nsub],varoute_24=sediment.mat[,3,nsub],varoute_25=sediment.mat[,4,nsub],
                       varoute_28=sediment.mat[,5,nsub],varoute_26=sediment.mat[,6,nsub],varoute_27=sediment.mat[,7,nsub])
      
      
      
      
      
      
      
      route.out <-  Routing(hydrology,sediment,contaminant,routingParams,
                            
                            simulation,nsub, res_option)
      
      
        saveRDS(route.out$mat,'output.rds')
        
      
      load[,,nsub]<-route.out$varoute
      
    }
    
    
    
    
    
  }  # end of external for loop
  
  # update
  
  remaining<-setdiff(remaining,completed)

  remaining<-unique(remaining)
 
} # end of while loop


setwd(dir3)

finish<-'finish'

saveRDS(finish,'finish.rds')










