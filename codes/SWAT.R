
################################################################################

# Module 1: Coarse-Scale Landscape Simulation.This code perform simulates landscape ...

# process for one subbasin

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

################################################################################

#---------------------------------------------------------------------------------------

# HRU outputs required for HIMS

#1 PRECIP: Total precipitation on HRU (mm H2O)

#5 PET: Potential evapotranspiration (mm H2O)

#22 WYLD: Net amount of water contributed by the HRU to the reach (mm H2O)

#24 TMP_AV: Average air temperature for time period (°C)

#28 SOLAR: Average daily solar radiation for time period (MJ/m2).

#29 SYLD: Amount of sediment contributed by the HRU to the reach (metric tons/ha)

#51 ORGN: Organic N contributed by HRU to reach (kg N/ha)

#52 ORGP: Organic P contributed by HRU to reach (kg P/ha)

#53 SEDP: Mineral P attached to sediment contributed by HRU to reach (kg P/ha)

#54 NSURQ:NO3 contributed by HRU in surface runoff to reach (kg N/ha)

#55 NLATQ:NO3 contributed by HRU in lateral flow to reach (kg N/ha)

#57 NO3GW:NO3 contributed by HRU in groundwater flow to reach (kgN/ha)

#58 SOLP: Soluble phosphorus contributed by HRU in surface runoff to reach (kg P/ha)

#59 P_GW: Soluble phosphorus contributed by HRU in groundwater flow to reach (kg P/ha)

###############################################################################################


# set R library path here 


library(numbers)

library(readr)


setwd(system("pwd", intern = T))



source(".../functions/Separator.R") # subbasin separator function

source(".../functions/Paramread.R") # read parameters


runpars<-readRDS('runpars.rds')


dir1 <-   runpars$dir1 #locate Nabsed folder

dir2 <-   runpars$dir2  #locate common files for running the mdoel 

dir3 <-  runpars$dir3  #locate storage directory

res_dir <- runpars$res_dir

i<-runpars$sub


pattern <- paste('output',"|","pars.rds" ,sep="")


log_pattern <- 'fin.fin' # run report file pattern

res_option<-0 # 0: reservoir parameters for NHD scale from data frame 1: reservoir parameters from HUC12 swat files 

#######################################      

      
      path<-Separator(nsub=i,dir1=dir1,dir2=dir2,dir3=dir3) # call seperator function
      
      pars<-Paramread(nsub=i,dir=path,res_option=res_option,res_dir=res_dir)



      ######################copy climate change .pcp and .tmp files############################

       flist<-list.files(path=".../climate/EC-Earth3/ssp245",full.names = TRUE)  # climate scenario
      
       file.copy(flist,path,overwrite = TRUE)

      #----------------------------------------------------------------------------------------
	  
      setwd(path)
      
      saveRDS(pars,'pars.rds')

      system('./swat2012_664')  # Run SWAT wxecutable
	  
	  
	 
    flist<-list.files(path=paste(dir3,'/S',i,sep=""),pattern=log_pattern,full.names = TRUE)


key<-0


while(key==0){
    
    if(length(flist)>0){
      
      
      fileConn <- file(flist)
      
      tx<-readLines(fileConn)
      
      close(fileConn)

 if(substr(tx[1],2,21)=="Execution successful"){key<-1}

}

} # while loop
      
      
              
        flist<-list.files(path=paste(dir3,'/',paste('S',i,sep=''),sep=""),pattern=pattern,full.names = TRUE)
        
        
        setwd(dir3)
        
        dir.create( paste('O',i,sep='')) 
        
        file.copy(flist, paste(dir3,'/',paste('O',i,sep=''),sep=""),overwrite = TRUE)

        finish<-'job completed'

    setwd(paste(dir3,'/',paste('O',i,sep=''),sep=""))

      unlink('runpars.rds')
    
      saveRDS(finish,'log.rds')
       
 