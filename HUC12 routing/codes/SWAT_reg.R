################################################################################

# Land scape simulation for multiple subbasins (JOB MANAGER)

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu 

################################################################################


# USER Inputs ----------------------------------------------------------------------


num <- 911      # number of SWAT subbasins


dir1 <- ".../model"                                                                  # model input files directory

dir2 <- ".../runfiles"                                                               # Common files for running the mdoel 

dir3 <- ".../swat"                                                                   # Output directory

dir4 <- ".../codes"                                                                  # directory of routing and land scape simulation codes

res_option <- 0                                                                      # not used for NHD+ routing


max_jobs <-90  # maximum number of submitting jobs                                  # Job limitation in Roar


subbasins <- 1:num

pattern<-paste("SWAT.R","|","SWAT.pbs",sep="")

flist<-list.files(path=dir4,pattern=pattern,full.names = TRUE)



for(i in subbasins){
   
  dir.create(paste(dir3,'/S',i,sep=""))
    
  setwd(paste(dir3,'/S',i,sep=""))

  runpars<-list(sub=i,dir1=dir1,dir2=dir2,dir3=dir3,dir4=dir4,res_dir=res_dir)

  saveRDS(runpars,'runpars.rds')

  setwd(dir4)
      
  file.copy(flist, paste(dir3,'/S',i,sep=""),overwrite = TRUE)
  
}




njob <- 0 

completed <- c()

remaining <- subbasins

run_vec <- rep(0,num)

running_jobs <- 0




while(length(remaining)>0){    
  
  for(i in remaining){  
    
    path<-paste(dir3,'/O',i,sep="") 
    
    flist<-list.files(path=path,pattern='log.rds',full.names = TRUE)
    
    if(length(flist)>0){                                                               # if routing output is available
      
      completed<-c(completed,i)
      
      completed<-unique(completed)
      
      running_jobs<-njob-length(completed)
      
     unlink(paste(dir3,'/S',i,sep=""),recursive = TRUE)

      unlink(paste(path,'/log.rds',sep=""))
      
    } else {
      
      running_jobs<-njob-length(completed)
      
      if(run_vec[i]==0 & running_jobs<=max_jobs) {

      path<-paste(dir3,'/S',i,sep="") 
        
        setwd(path)
        
        system('sbatch SWAT.pbs') # run batch file 
        
        Sys.sleep(5)
        
        njob<-njob+1
        
        running_jobs<-njob-length(completed)
        
        run_vec[i]<-1
        
      }
      
      
      
    } 

 } 



remaining<-setdiff(remaining,completed)

remaining<-unique(remaining)

    
 } # while loop

for(i in subbasins){

dir<-paste(dir3,'/S',i,sep="")

unlink(dir,recursive=TRUE)

}

finish<-'finish'

setwd(dir3)

saveRDS(finish,'finish.rds')










  
  