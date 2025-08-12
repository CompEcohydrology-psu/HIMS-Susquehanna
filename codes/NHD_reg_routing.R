
################################################################################

# Module 3: Module 3: Stream Routing and Infrastructure Representation...

# This code routes water, nutrient and sediments on NHDPlus HR scale for ...

# multiple subbasins - job manager

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu 

################################################################################


# Inputs ----------------------------------------------------------------------


num <- 911     # number of SWAT subbasins

start<-'2055-01-01' # start date of simulation same as swat landscape simulation

end<-'2089-12-31' # end date of simulation same as swat lanscape simulation

# directory of folder for saving routing output:

dir1<- ".../NHD_rout" # output directory

# directory of the folder where NHD flowline,

# catchment, fig, and terminal files are available:

dir2<- ".../data" # data directory

# directory where SWAT landscape outputs are available: 

dir3<-".../swat" lnadscape simulation (Hru output files) directory

dir4<-".../codes" 

max_jobs <- 90  # maximum number of submitting jobs

#------------------------------------------------------------------------------------

pattern<-'remaining.rds'

flist<-list.files(path=dir1,pattern=pattern,full.names = TRUE)

if(length(flist)>0){

remaining<-readRDS(flist)

subbasins<-remaining

completed<-setdiff(1:911,subbasins)

}else{

subbasins<-1:num


completed<-c()

remaining<-subbasins

for(i in subbasins){
      
 unlink( list.files(path=paste(dir1,'/R',i,sep=""),pattern="route.rds",full.names = TRUE))
  
}

}


pattern<-paste('NHD_routing.R',"|","NHD_routing.pbs",sep="")

pattern2<-paste('.pbs.o',"|",".pbs.e","|","report.txt",    sep="")


flist<-list.files(path=dir4,pattern=pattern,full.names = TRUE)


# Distribute routing code-----------------------------------------------------------------------

for(i in subbasins){
      
      
      file.copy(flist, paste(dir1,'/R',i,sep=""),overwrite = TRUE)


     flist2<-list.files(path=paste(dir1,'/R',i,sep=""),pattern=pattern2,full.names = TRUE)

unlink(flist2)
  
}




setwd(dir2)

fig<-as.matrix(read.csv('fig.csv',header = FALSE))





# Routing ----------------------------------------------------------------------

njob<-0 # counter for job submission

n_rem1<- length(subbasins)


run_vec<-rep(0,num)


running_jobs<-0

pattern <- 'finish_route.rds' # routing output file pattern



while(length(remaining)>0){  # while loop


for(i in remaining){  # for loop

path<-paste(dir1,'/R',i,sep="") #path to routing folder of the interested subbasin



upstream<-as.vector(na.omit(fig[i,])) # upstream watersheds

if(length(upstream)>0){ # not head watershed
  

if(all(upstream %in% completed)==TRUE ){  # 1
  
  
  flist<-list.files(path=path,pattern=pattern,full.names = TRUE)
  
  if(length(flist)>0){ # if routing output is available
    
    completed<-c(completed,i)

     completed<-unique(completed)

   running_jobs<-njob-length(completed)

  
    
  }else{

    running_jobs<-njob-length(completed)

    if(run_vec[i]==0 & running_jobs<max_jobs) {

   setwd(path)
      
    system('sbatch NHD_routing.pbs') # run batch file 

    Sys.sleep(10)

      
      njob<-njob+1
      
      run_vec[i]<-1

run_report<-c(njob,running_jobs,length(remaining))

sink("rout_report.txt")

print(run_report)

sink()


      
    }
  
  }
  



} # 1

}else{ # else for headwatershed


flist<-list.files(path=path,pattern=pattern,full.names = TRUE)

 if(length(flist)>0){ # if routing output is available

completed<-c(completed,i)

completed<-unique(completed)


running_jobs<-njob-length(completed)

remaining<-setdiff(remaining,completed)

remaining<-unique(remaining)

setwd(dir1)

   saveRDS(remaining,'remaining.rds')


}else{
  
 running_jobs<-njob-length(completed)

  if(run_vec[i]==0 & running_jobs<max_jobs) {

  setwd(path)

  system('sbatch NHD_routing.pbs') # run batch file

 Sys.sleep(10)
  
  njob<-njob+1
  
  run_vec[i]<-1


  
}


} 

}# else for headwatershed

}  # end of external for loop

remaining<-setdiff(remaining,completed)

  remaining<-unique(remaining)

n_rem2<-length(remaining)

if(n_rem1 != n_rem2){

setwd(dir1)

   saveRDS(remaining,'remaining.rds')

n_rem1 <- n_rem2

}

} # end of while loop




