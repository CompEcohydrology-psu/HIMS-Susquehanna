
################################################################################

# Module 2: Fine-Scale Catchment Load Allocation, which spatially disaggregates SWAT ...

# outputs using the NHDPlus HR catchment dataset. 

# parallel (job submissioin manager)

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu 

################################################################################

# call functions ------------------------------------------------------------

source(".../functions/Wetland_iso.R") # nonriparian wetland

source(".../functions/Pond.R") # pond


# set directory ------------------------------------------------------------

setwd(system("pwd", intern = T) ) # same directory where R cose is available

# read files ---------------------------------------------------------------

flowline<-readRDS('flowline.rds') # flowlines

runpars<-readRDS('runpars.rds') # simulation parameters

# Simulation setting ---------------------------------------------------------

simulation<-list(start=runpars$start,end=runpars$end) # simulation start and end date

start.date <- as.Date(simulation$start,format="%Y-%m-%d")   

end.date <- as.Date(simulation$end,format="%Y-%m-%d")  

dates <- seq.Date(start.date, end.date, by ="day") 


# reset directory ------------------------------------------------------------

setwd(paste(runpars$dir3,'/O',runpars$sub,sep=""))  # directory of folder where all landscape output [O] are available

# read files ----------------------------------------------------------------

 pars<-readRDS('pars.rds')                            # routing parameters 

 chla_subco <- pars$chla_subco                        # 

sub_hru<-read.table('output.hru',skip=9)              # hru output


# reset directory ------------------------------------------------------------

setwd(runpars$dir2)  # directory of folder where all datasets are available

# read files ------------------------------------------------------------

hru_code<-readRDS('hru_extract.rds')

hru_lookup<-readRDS('hru_lookup.rds')

catchment<-readRDS('catchment.rds')


#---------------------------------------------------------------------------

sub_hru_codes<-hru_lookup$orig_num[hru_lookup$sub==runpars$sub]

#-------------------- additional calculation---------------------------------

hydrology.mat<-array(0, dim = c(length(dates),7,dim(flowline)[1])) # hydrology load matrix

contaminant.mat<-array(0, dim = c(length(dates),9,dim(flowline)[1])) # contaminant load matrix

sediment.mat <- array(0, dim = c(length(dates),7,dim(flowline)[1])) # sediment load matrix

area<- 27.474 * 27.474 * 0.0001 # area of raster cell in hectare

hru_km <- 27.474 * 27.474 * 0.000001

cnv <- 10 * area  # convertor

# external loop over i Loading streams -------------------------------


for(i in 1:dim(flowline)[1]){


n<-which(catchment$cor_id==flowline$core_id[i])

nr<-n  # row number

if(length(n)>0){

n<-hru_code[[n]]

n<-as.vector(n[[1]])

n<-na.omit(n)

codes<-table(n)


if(length(codes)>0){

# internal for loop over j finding hru information among all  ---------

for(j in 1:length(codes)){


if(as.numeric(names(codes)[j]) %in% sub_hru_codes){

nrow<-which(hru_lookup$orig_num==as.numeric(names(codes)[j]))

hru_newnum<-hru_lookup$new_num[nrow]

sol_cbn <- hru_lookup$sol_cbn[nrow]

dummy<-which(sub_hru[,2]==hru_newnum)

hru<-sub_hru[dummy,]

}else{


nrow<-which(hru_lookup$orig_num==as.numeric(names(codes)[j]))

hru_subs<-hru_lookup$sub[nrow] 
  
hru_newnum<-hru_lookup$new_num[nrow]

sol_cbn <- hru_lookup$sol_cbn[nrow]

hru<-read.table(paste(runpars$dir3,'/O',hru_subs,'/output.hru',sep=""),skip=9)

dummy<-which(hru[,2]==hru_newnum)

hru<-hru[dummy,]

}



  hydrology.mat[,1,i] <-hydrology.mat[,1,i] + hru[,9] * codes[j]  #  WYLD (mm H2O)  # Groundwater + Lateral flow + Surface Runoff

  hydrology.mat[,2,i] <-  hydrology.mat[,2,i] + hru[,8] * codes[j]  # PET (mm H2O)
  
  hydrology.mat[,3,i] <- hydrology.mat[,3,i] + hru[,7] * codes[j]   # PRECIP (mmH2O)
  
  hydrology.mat[,4,i] <- hydrology.mat[,4,i] + hru[,10] * codes[j]   # TMP_AV (�C)

  hydrology.mat[,5,i] <-  hydrology.mat[,5,i] +  hru[,11] * codes[j]  # SOLAR (MJ/m2)
   
  hydrology.mat[,6,i] <- hydrology.mat[,6,i] + hru[,21] * codes[j]  #  Q_GW (mm H2O)

  hydrology.mat[,7,i] <-  hydrology.mat[,7,i] + hru[,22] * codes[j]  #  Surface Runoff (mm H2O)
          
  contaminant.mat[,1,i] <- contaminant.mat[,1,i] + hru[,13] * codes[j]  # ORGN (kg N/ha)
  
  contaminant.mat[,2,i] <- contaminant.mat[,2,i] + (hru[,14] + hru[,15]) * codes[j]  # ORGP (Sed P) (kg P/ha)
  
  contaminant.mat[,3,i] <-  contaminant.mat[,3,i] + (hru[,16] + hru[,17] + hru[,18]) * codes[j]  # NO3
    
  contaminant.mat[,4,i] <- contaminant.mat[,4,i] + (hru[,19] + hru[,20] ) * codes[j]  # Mineral P (Soluble P) (kg P/ha)



#---------------------------CBOD, DO, CHLA---------------------

sedmod <- hru[,12] * area  # sediment yield [ton]

sedmod[sedmod < 1.e-4] <- 0
  
cy <- 0.1 * sedmod / (area * hru[,22] + 1.e-6)



enratio <- rep(0,length(cy))

wn <-which (cy > 1.e-6)

if(length(wn) > 0){

enratio[wn] <- .78 * cy[wn] ^ (-0.2468)

wn <-which(enratio > 3.5)

enratio[wn] <- 3.5

}


wtmp <- 5.0 + 0.75 * hru[,10]  # water temperature

wtmp[wtmp <= 0.1] <- 0.1

wtmp <- wtmp + 273.15    # deg C to deg K


qdr <- hru[,9]

wn <- which(qdr > 1.e-4)


tp <- rep(0,length(qdr))

cbodu <- rep(0,length(qdr))

org_c <- rep(0,length(qdr))

doxq <- rep(0,length(qdr))

chl_a <- rep(0,length(qdr))

soxy <- rep(0,length(qdr))


if(length(wn) > 0) {

tp[wn] <- 100. * (hru[wn,13] + hru[wn,16]) / qdr[wn]   # 100*kg/ha/mm = ppm 



chl_a[wn] <- chla_subco * tp[wn]  #  microgram/L  


# calculate organic carbon loading to main channel

          
          org_c[wn] <- (sol_cbn / 100) * enratio[wn] * sedmod[wn]  * 1000.


# calculate carbonaceous biological oxygen demand (CBOD)

         
          cbodu[wn] <-  2.7 * org_c[wn] / (qdr[wn] * hru_km) # mg/L


# calculate dissolved oxygen saturation concentration

          # QUAL2E equation III-29

          ww <- -139.34410 + (1.575701e05 / wtmp)

          xx <- 6.642308e07 / (wtmp ^ 2)

          yy <- 1.243800e10 / (wtmp ^ 3)

          zz <- 8.621949e11 / (wtmp ^ 4)

          soxy <- exp(ww - xx + yy - zz)

          soxy[soxy < 0] <- 0

         doxq[wn] <- soxy[wn] * exp(-0.1 * cbodu[wn])

         doxq[doxq < 0] <- 0


        doxq[doxq > soxy] <- soxy[doxq > soxy]    # mg/L

      }
        
         CHLA <- chl_a * hru[,22] * 1e-6 * cnv / area   # [kg chlorophyll-a/ha]	

         CBOD <- cbodu * hru[,22] * 1e-3 * cnv / area  # [kg CBOD/ha]

         DO <-  (doxq * hru[,22] + soxy * (hru[,9]-hru[,22])) * cnv * 1e-3 / area   # [kg DO/ha]

         DO[DO < 0] <- 0

         hru <- cbind(hru, CHLA, CBOD, DO)
#-----------------------------------------------------------------------------------

    
  contaminant.mat[,5,i] <- rep(0,length(dates))      # NH4 (kg/ha)
    
  contaminant.mat[,6,i] <-  rep(0,length(dates))     # NO2 (kg/ha)
    
  contaminant.mat[,7,i] <- contaminant.mat[,7,i] + hru[,24] * codes[j]       # CBOD (kg/ha)
    
  contaminant.mat[,8,i] <- contaminant.mat[,8,i] + hru[,25] * codes[j]         # DO (kg/ha)
    
  contaminant.mat[,9,i] <- contaminant.mat[,9,i] + hru[,23] * codes[j]       # COLOROPHYLL-a (kg/ha)
  
  sediment.mat[,1,i] <- sediment.mat[,1,i] + hru[,12] * codes[j]   # SYLD (metric tons/ha)
  
  sediment.mat[,2,i] <- rep(0,length(dates))
  
  sediment.mat[,3,i] <-  rep(0,length(dates))
  
  sediment.mat[,4,i] <-  rep(0,length(dates))
  
  sediment.mat[,5,i] <-  rep(0,length(dates))
  
  sediment.mat[,6,i] <-  rep(0,length(dates))
  
  sediment.mat[,7,i] <-  rep(0,length(dates))

} # over j

hydrology.mat[,1,i] <- hydrology.mat[,1,i] * area * 10 

hydrology.mat[,2,i]<-  hydrology.mat[,2,i] / sum(codes)

hydrology.mat[,3,i]<-  hydrology.mat[,3,i] / sum(codes)

hydrology.mat[,4,i]<-  hydrology.mat[,4,i] / sum(codes)

hydrology.mat[,5,i]<-  hydrology.mat[,5,i] / sum(codes)

hydrology.mat[,6,i] <- hydrology.mat[,6,i] * area * 10  # GW_Q

hydrology.mat[,7,i] <- hydrology.mat[,7,i] * area * 10  # Surface Runoff

hydrology.mat[,1,i] <- pmax(0,hydrology.mat[,1,i] - hydrology.mat[,6,i]) # only surface runoff and lateral flow

surf_c <- ifelse(hydrology.mat[,1,i] == 0, 0, hydrology.mat[,7,i] / hydrology.mat[,1,i])  # ratio of surface runoff to summation of surface runoff and lateral flow

hydrology.mat[,1,i][hydrology.mat[,1,i] < 0] <- 0  # surface runoff should not be negative

contaminant.mat[,1,i] <- contaminant.mat[,1,i] *  area

contaminant.mat[,2,i] <- contaminant.mat[,2,i] *  area

contaminant.mat[,3,i] <- contaminant.mat[,3,i] *  area

contaminant.mat[,4,i] <- contaminant.mat[,4,i] *  area

contaminant.mat[,7,i] <- contaminant.mat[,7,i] *  area

contaminant.mat[,8,i] <- contaminant.mat[,8,i] *  area

contaminant.mat[,9,i] <- contaminant.mat[,9,i] *  area

sediment.mat[,1,i] <- sediment.mat[,1,i] *  area



#---- routing through wetland and pond -------------------------------------

isout <- matrix(0,nrow=length(dates),ncol=22)  #  wetland output

pndout <- matrix(0,nrow=length(dates),ncol=22) #  pond output

param <- list(wet_sed=0, wet_san=0,  wet_sil=0, wet_cla=0, wet_sag=0, wet_lag=0, wet_psol=0, wet_psed=0,

                 wet_orgn=0, wet_no3=0, wet_nvol=0, wet_nsed=4000, psetlw=10, nsetlw=10,

                 pnd_sed=0, pnd_san=0, pnd_sil=0, pnd_cla=0, pnd_sag=0, pnd_lag=0, pnd_psol=0, pnd_psed=0,

                 pnd_orgn =0,  pnd_no3=0, pnd_nsed=4000, pnd_d50=10, psetlp=10, nsetlp=10)



            

  param$wet_fr     <-  flowline$iso_drain_fr[i]           # wetland drainage area [-]

  param$wet_k      <-  flowline$iso_ksat[i]         # wetland hydraulic conductivity [mm/hr]
    
  param$wet_nvol   <-  flowline$iso_storage[i]     # wetland normal storage [m3]

  param$wet_mxvol  <-   flowline$iso_storage[i]           # wetland maximum storage [m3]

  param$wet_vol    <-   param$wet_nvol

  param$wet_nsa    <-   flowline$iso_area[i]         # area of wetland at normal storage [hectare]

  param$wet_mxsa   <-   flowline$iso_area[i]              # area of wetland at normal storage [hectare]

     
  
  param$pnd_fr   <-  flowline$pond_drain_fr[i]            # pond drainage fraction [-]

  param$pnd_k    <-   flowline$pond_ksat[i]           # pond hydraulic conductivity [mm/hr]

  param$pnd_pvol <- flowline$pond_storage[i]       # pond normal storage [m3]
  
  param$pnd_evol <-  flowline$pond_storage[i]             # pond maximum storage [m3]

  param$pnd_vol <- param$pnd_pvol 
  
  param$pnd_psa  <-  flowline$pond_area[i]           # pond surface area at normal level [ha] 
  
  param$pnd_esa  <-  flowline$pond_area[i]                # pond surface area at maximum level [ha] 

  
  param$sed_stl    <-  flowline$sed_stl[i]                # sediment remaining fraction [-]




 # param$wet_fr     <-  NA  # Comment this line to apply wetlands

 # param$pnd_fr <- NA  # Comment this line to apply ponds


wn <- which(hydrology.mat[,1,i] > 1e-6)

  fact <- rep(0,length(dates))



if(is.na(param$pnd_fr)==TRUE & is.na(param$wet_fr)==FALSE){  # only isolated wetland


for( k in 1: length(dates)){  # for loop



isout[k,] <- Wetland_iso(watyld=hydrology.mat[k,1,i], sedyld=sediment.mat[k,1,i], sanyld=sediment.mat[k,2,i],

                   silyld=sediment.mat[k,3,i], clayld=sediment.mat[k,4,i],
                   
                   sagyld=sediment.mat[k,5,i],lagyld=sediment.mat[k,6,i],

                   psolyld=contaminant.mat[k,4,i], psedyld=contaminant.mat[k,2,i],

                   orgnyld=contaminant.mat[k,1,i], no3yld= contaminant.mat[k,3,i], param=param, i_mo=1)


  param$wet_vol <- isout[k,12]
  
  param$wet_sed <- isout[k,13]   # sediment concentration in wetland [kg/L]
  
  param$wet_san <- isout[k,14]
  
  param$wet_sil <- isout[k,15]
  
  param$wet_cla <- isout[k,16]
  
  param$wet_sag <- isout[k,17]
  
  param$wet_lag <- isout[k,18]
  
  param$wet_psol <- isout[k,19] 
  
  param$wet_psed <- isout[k,20]
  
  param$wet_orgn <- isout[k,21]
  
  param$wet_no3 <-  isout[k,22]
  




} # for loop 

   
if(length(wn)>0){

  fact[wn] <- isout[wn,1] / hydrology.mat[wn,1,i]
}

  hydrology.mat[,1,i] <- isout[,1]

  sediment.mat[,1,i]  <- isout[,2]

  sediment.mat[,2,i]  <- isout[,3]
  
  sediment.mat[,3,i]  <- isout[,4]

  sediment.mat[,4,i]  <- isout[,5]

  sediment.mat[,5,i]  <- isout[,6]

  sediment.mat[,6,i]  <- isout[,7]

  contaminant.mat[,1,i] <- isout[,8]

  contaminant.mat[,2,i] <- isout[,10]

  contaminant.mat[,3,i] <- isout[,9]

  contaminant.mat[,4,i] <- isout[,11]

  contaminant.mat[,7,i] <-  fact * contaminant.mat[,7,i]

  contaminant.mat[,8,i] <-  fact * contaminant.mat[,8,i]

  contaminant.mat[,9,i] <-  fact * contaminant.mat[,9,i]


} # only wetland


if(is.na(param$pnd_fr)==FALSE & is.na(param$wet_fr)==TRUE) {  # only pond


for( k in 1: length(dates)){  # for loop


pndout[k,] <- Pond(watyld=hydrology.mat[k,1,i], sedyld=sediment.mat[k,1,i], sanyld=sediment.mat[k,2,i],

                   silyld=sediment.mat[k,3,i], clayld=sediment.mat[k,4,i],
                   
                   sagyld=sediment.mat[k,5,i],lagyld=sediment.mat[k,6,i],

                   psolyld=contaminant.mat[k,4,i], psedyld=contaminant.mat[k,2,i],

                   orgnyld=contaminant.mat[k,1,i], no3yld=contaminant.mat[k,3,i], param=param, i_mo=1)


  param$pnd_vol <- pndout[k,12]
  
  param$pnd_sed <- pndout[k,13]   # sediment concentration in pond [kg/L]
  
  param$pnd_san <- pndout[k,14]
  
  param$pnd_sil <- pndout[k,15]
  
  param$pnd_cla <- pndout[k,16]
  
  param$pnd_sag <- pndout[k,17]
  
  param$pnd_lag <- pndout[k,18]
  
  param$pnd_psol <-pndout[k,19] 
  
  param$pnd_psed <- pndout[k,20]
  
  param$pnd_orgn <- pndout[k,21]
  
  param$pnd_no3 <-  pndout[k,22]




}



if(length(wn)>0){

  fact[wn] <- pndout[wn,1] / hydrology.mat[wn,1,i]

}

  hydrology.mat[,1,i] <- pndout[,1]

  sediment.mat[,1,i]  <- pndout[,2]

  sediment.mat[,2,i]  <- pndout[,3]
  
  sediment.mat[,3,i]  <- pndout[,4]

  sediment.mat[,4,i]  <- pndout[,5]

  sediment.mat[,5,i]  <- pndout[,6]

  sediment.mat[,6,i]  <- pndout[,7]

  contaminant.mat[,1,i] <- pndout[,8]

  contaminant.mat[,2,i] <- pndout[,10]

  contaminant.mat[,3,i] <- pndout[,9]

  contaminant.mat[,4,i] <- pndout[,11]

  contaminant.mat[,7,i] <-  fact * contaminant.mat[,7,i]

  contaminant.mat[,8,i] <-  fact * contaminant.mat[,8,i]

  contaminant.mat[,9,i] <-  fact * contaminant.mat[,9,i]


}  # only pond




if(is.na(param$pnd_fr)==FALSE & is.na(param$wet_fr)==FALSE){ # both pond and isolated wetland


if(param$pnd_fr < param$wet_fr ) {  # pond >> wetland



for( k in 1: length(dates)){  # for loop



pndout[k,] <- Pond(watyld=hydrology.mat[k,1,i], sedyld=sediment.mat[k,1,i], sanyld=sediment.mat[k,2,i],

                   silyld=sediment.mat[k,3,i], clayld=sediment.mat[k,4,i],
                   
                   sagyld=sediment.mat[k,5,i],lagyld=sediment.mat[k,6,i],

                   psolyld=contaminant.mat[k,4,i], psedyld=contaminant.mat[k,2,i],

                   orgnyld=contaminant.mat[k,1,i], no3yld=contaminant.mat[k,3,i], param=param, i_mo=1)



isout[k,] <- Wetland_iso(watyld=pndout[k,1], sedyld=pndout[k,2], sanyld=pndout[k,3],

                   silyld=pndout[k,4], clayld=pndout[k,5],
                   
                   sagyld=pndout[k,6],lagyld=pndout[k,7],

                   psolyld=pndout[k,11], psedyld=pndout[k,10],

                   orgnyld=pndout[k,8], no3yld= pndout[k,9], param=param, i_mo=1)


  param$pnd_vol <- pndout[k,12]
  
  param$pnd_sed <- pndout[k,13]   # sediment concentration in pond [kg/L]
  
  param$pnd_san <- pndout[k,14]
  
  param$pnd_sil <- pndout[k,15]
  
  param$pnd_cla <- pndout[k,16]
  
  param$pnd_sag <- pndout[k,17]
  
  param$pnd_lag <- pndout[k,18]
  
  param$pnd_psol <-pndout[k,19] 
  
  param$pnd_psed <- pndout[k,20]
  
  param$pnd_orgn <- pndout[k,21]
  
  param$pnd_no3 <-  pndout[k,22]


  param$wet_vol <- isout[i,12]
  
  param$wet_sed <- isout[k,13]   # sediment concentration in wetland [kg/L]
  
  param$wet_san <- isout[k,14]
  
  param$wet_sil <- isout[k,15]
  
  param$wet_cla <- isout[k,16]
  
  param$wet_sag <- isout[k,17]
  
  param$wet_lag <- isout[k,18]
  
  param$wet_psol <- isout[k,19] 
  
  param$wet_psed <- isout[k,20]
  
  param$wet_orgn <- isout[k,21]
  
  param$wet_no3 <-  isout[k,22]



} # for loop

if(length(wn)>0){

  fact[wn] <- isout[wn,1] / hydrology.mat[wn,1,i]

}

  hydrology.mat[,1,i] <- isout[,1]

  sediment.mat[,1,i]  <- isout[,2]

  sediment.mat[,2,i]  <- isout[,3]
  
  sediment.mat[,3,i]  <- isout[,4]

  sediment.mat[,4,i]  <- isout[,5]

  sediment.mat[,5,i]  <- isout[,6]

  sediment.mat[,6,i]  <- isout[,7]

  contaminant.mat[,1,i] <- isout[,8]

  contaminant.mat[,2,i] <- isout[,10]

  contaminant.mat[,3,i] <- isout[,9]

  contaminant.mat[,4,i] <- isout[,11]

  contaminant.mat[,7,i] <-  fact * contaminant.mat[,7,i]

  contaminant.mat[,8,i] <-  fact * contaminant.mat[,8,i]

  contaminant.mat[,9,i] <-  fact * contaminant.mat[,9,i]



}else{  # wetland >> pond


for( k in 1: length(dates)){  # for loop


isout[k,] <- Wetland_iso(watyld=hydrology.mat[k,1,i], sedyld=sediment.mat[k,1,i], sanyld=sediment.mat[k,2,i],

                   silyld=sediment.mat[k,3,i], clayld=sediment.mat[k,4,i],
                   
                   sagyld=sediment.mat[k,5,i],lagyld=sediment.mat[k,6,i],

                   psolyld=contaminant.mat[k,4,i], psedyld=contaminant.mat[k,2,i],

                   orgnyld=contaminant.mat[k,1,i], no3yld= contaminant.mat[k,3,i], param=param, i_mo=1)



pndout[k,] <- Pond(watyld=isout[k,1], sedyld=isout[k,2], sanyld=isout[k,3],

                   silyld=isout[k,4], clayld=isout[k,5],
                   
                   sagyld=isout[k,6],lagyld=isout[k,7],

                   psolyld=isout[k,11], psedyld=isout[k,10],

                   orgnyld=isout[k,8], no3yld=isout[k,9], param=param, i_mo=1)


  param$wet_vol <- isout[k,12]
  
  param$wet_sed <- isout[k,13]   # sediment concentration in wetland [kg/L]
  
  param$wet_san <- isout[k,14]
  
  param$wet_sil <- isout[k,15]
  
  param$wet_cla <- isout[k,16]
  
  param$wet_sag <- isout[k,17]
  
  param$wet_lag <- isout[k,18]
  
  param$wet_psol <- isout[k,19] 
  
  param$wet_psed <- isout[k,20]
  
  param$wet_orgn <- isout[k,21]
  
  param$wet_no3 <-  isout[k,22]

  param$pnd_vol <- pndout[i,12]
  
  param$pnd_sed <- pndout[k,13]   # sediment concentration in pond [kg/L]
  
  param$pnd_san <- pndout[k,14]
  
  param$pnd_sil <- pndout[k,15]
  
  param$pnd_cla <- pndout[k,16]
  
  param$pnd_sag <- pndout[k,17]
  
  param$pnd_lag <- pndout[k,18]
  
  param$pnd_psol <-pndout[k,19] 
  
  param$pnd_psed <- pndout[k,20]
  
  param$pnd_orgn <- pndout[k,21]
  
  param$pnd_no3 <-  pndout[k,22]



} # loop

if(length(wn)>0){

  fact[wn] <- pndout[wn,1] / hydrology.mat[wn,1,i]

}


  hydrology.mat[,1,i] <- pndout[,1]

  sediment.mat[,1,i]  <- pndout[,2]

  sediment.mat[,2,i]  <- pndout[,3]
  
  sediment.mat[,3,i]  <- pndout[,4]

  sediment.mat[,4,i]  <- pndout[,5]

  sediment.mat[,5,i]  <- pndout[,6]

  sediment.mat[,6,i]  <- pndout[,7]

  contaminant.mat[,1,i] <- pndout[,8]

  contaminant.mat[,2,i] <- pndout[,10]

  contaminant.mat[,3,i] <- pndout[,9]

  contaminant.mat[,4,i] <- pndout[,11]

  contaminant.mat[,7,i] <-  fact * contaminant.mat[,7,i]

  contaminant.mat[,8,i] <-  fact * contaminant.mat[,8,i]

  contaminant.mat[,9,i] <-  fact * contaminant.mat[,9,i]


} #





} # both wetland and pond





}else{


hydrology.mat[,1,i]<-rep(0,length(dates))

hydrology.mat[,2,i]<-rep(0,length(dates))

hydrology.mat[,3,i]<-rep(0,length(dates))

hydrology.mat[,4,i]<-rep(0,length(dates))

hydrology.mat[,5,i]<-rep(0,length(dates))

 hydrology.mat[,6,i]<-rep(0,length(dates))

 surf_c <- 0

contaminant.mat[,1,i] <- rep(0,length(dates))
  
  contaminant.mat[,2,i] <- rep(0,length(dates))
  
  contaminant.mat[,3,i] <-  rep(0,length(dates))
    
  contaminant.mat[,4,i] <- rep(0,length(dates))
    
  contaminant.mat[,5,i] <- rep(0,length(dates))
    
  contaminant.mat[,6,i] <-  rep(0,length(dates))
    
  contaminant.mat[,7,i] <- rep(0,length(dates))
    
  contaminant.mat[,8,i] <- rep(0,length(dates))
    
  contaminant.mat[,9,i] <-rep(0,length(dates))

  sediment.mat[,1,i] <- rep(0,length(dates))
  
  sediment.mat[,2,i] <- rep(0,length(dates))
  
  sediment.mat[,3,i] <-  rep(0,length(dates))
  
  sediment.mat[,4,i] <-  rep(0,length(dates))
  
  sediment.mat[,5,i] <-  rep(0,length(dates))
  
  sediment.mat[,6,i] <-  rep(0,length(dates))
  
  sediment.mat[,7,i] <-  rep(0,length(dates))


}


}else{

hydrology.mat[,1,i]<-rep(0,length(dates))

hydrology.mat[,2,i]<-rep(0,length(dates))

hydrology.mat[,3,i]<-rep(0,length(dates))

hydrology.mat[,4,i]<-rep(0,length(dates))

hydrology.mat[,5,i]<-rep(0,length(dates))

hydrology.mat[,6,i]<-rep(0,length(dates))

surf_c <- 0


contaminant.mat[,1,i] <- rep(0,length(dates))
  
  contaminant.mat[,2,i] <- rep(0,length(dates))
  
  contaminant.mat[,3,i] <-  rep(0,length(dates))
    
  contaminant.mat[,4,i] <- rep(0,length(dates))
    
  contaminant.mat[,5,i] <- rep(0,length(dates))
    
  contaminant.mat[,6,i] <-  rep(0,length(dates))
    
  contaminant.mat[,7,i] <- rep(0,length(dates))
    
  contaminant.mat[,8,i] <- rep(0,length(dates))
    
  contaminant.mat[,9,i] <-rep(0,length(dates))

  sediment.mat[,1,i] <- rep(0,length(dates))
  
  sediment.mat[,2,i] <- rep(0,length(dates))
  
  sediment.mat[,3,i] <-  rep(0,length(dates))
  
  sediment.mat[,4,i] <-  rep(0,length(dates))
  
  sediment.mat[,5,i] <-  rep(0,length(dates))
  
  sediment.mat[,6,i] <-  rep(0,length(dates))
  
  sediment.mat[,7,i] <-  rep(0,length(dates))


}



#---------------------------------------------------------------------------

hydrology.mat[,7,i] <-hydrology.mat[,1,i] * surf_c


} # over i (end of loading)



# set directory ------------------------------------------------------------

setwd(paste(runpars$dir1,'/R',runpars$sub,sep="")) 

# save load ------------------------------------------------------------

load<-list(hydrology=hydrology.mat,contaminant=contaminant.mat,sediment=sediment.mat)

saveRDS(load,'load.rds')

finish_load <-1

saveRDS(finish_load,'finish_load.rds')





