################################################################################

# Module 2: Fine-Scale Catchment Load Allocation, which spatially disaggregates SWAT ...

# outputs using the NHDPlus HR catchment dataset. Job manager to submit multiple jobs

# parallel (job submissioin manager)

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu 

################################################################################


num <-911    # number of SWAT subbasins

start <- '2055-01-01' # start date of simulation same as swat landscape simulation

end <-'2089-12-31' # end date of simulation same as swat lanscape simulation

# directory of folder for saving routing output:

 dir1<- ".../NHD_rout" # Output directory

# directory of the folder where NHD flowline,

# catchment, fig, and terminal files are available:

dir2<- ".../data" 

# directory where SWAT landscape (HRUs) outputs are available: 

dir3<-".../swat" 

dir4<-".../codes"

max_jobs <- 90  # maximum number of submitting jobs

#------------------------------------------------------------------------------------

pattern<-'remaining1.rds'

flist<-list.files(path=dir1,pattern=pattern,full.names = TRUE)

if(length(flist)>0){

remaining<-readRDS(flist)

subbasins<-remaining

}else{

subbasins<-1:num


}



pattern<-paste('NHD_loading.R',"|","NHD_loading.pbs",sep="")


flist<-list.files(path=dir4,pattern=pattern,full.names = TRUE)



# Deviding NHD streams into HUC-12 computational units ------------------------

# call SWAT-NHD convertor:

source("/storage/home/abh6005/work/swat/functions/SWAT_NHD.R") 

setwd(dir2)

flowline<-readRDS('flowline.rds')

fig<-as.matrix(read.csv('fig.csv',header = FALSE))

# NHD_subs<-readRDS('catchment.rds') # optional (comment it)


for(i in subbasins){
  
   
  dir.create(paste(dir1,'/R',i,sep=""))
  
  
  unlink(paste(dir1,'/R',i,"/*",sep=""))
  
  
  setwd(paste(dir1,'/R',i,sep=""))
  
  ids<-SWAT_NHD(nsub=i,dir=dir2)
  
  n<-which(flowline$core_id %in% ids)
  
  set1<-flowline[n,]
  

  
  saveRDS(set1,'flowline.rds')
  


upstream<-as.vector(na.omit(fig[i,])) # upstream watersheds

downstream<-which(fig ==i, arr.ind=TRUE)[1]



runpars<-list(sub=i,start=start,end=end,dir1=dir1,dir2=dir2,dir3=dir3,upstream=upstream,downstream=downstream)

saveRDS(runpars,'runpars.rds')

 setwd(dir4)
      
      file.copy(flist, paste(dir1,'/R',i,sep=""),overwrite = TRUE)
  
}



# Loading ----------------------------------------------------------------------

njob<-0 # counter for job submission

completed<-c()

n_rem1<- length(subbasins)


remaining<-subbasins

run_vec<-rep(0,num)


running_jobs<-0

pattern <- 'finish_load.rds' # routing output file pattern



while(length(remaining)>0){  # while loop


for(i in remaining){  # for loop

path<-paste(dir1,'/R',i,sep="") #path to routing folder of the interested subbasin


flist<-list.files(path=path,pattern=pattern,full.names = TRUE)

 if(length(flist)>0){ # if routing output is available

completed<-c(completed,i)

completed<-unique(completed)

running_jobs<-njob-length(completed)


}else{
  
 running_jobs<-njob-length(completed)

  if(run_vec[i]==0 & running_jobs<=max_jobs) {

  setwd(path)

  system('sbatch NHD_loading.pbs') # run batch file 

Sys.sleep(6)

  
  njob<-njob+1

running_jobs<-njob-length(completed)
  
  run_vec[i]<-1
  
}



}# else 

}  # end of external for loop

remaining<-setdiff(remaining,completed)

remaining<-unique(remaining)

n_rem2<-length(remaining)


if(n_rem1 != n_rem2){

setwd(dir1)

   saveRDS(remaining,'remaining1.rds')

n_rem1 <- n_rem2

}


} # end of while loop




