################################################################################

# Splitting Subbasin Input Files for Parallel Execution

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

################################################################################

# Set R library path here

library(readr)

Separator<-function(nsub,dir1,dir2,dir3){
  
  #dir1: SWAT model input files  directory
  
  #dir2: commonly required files directory
  
  #dir3 storage directory


nsub.a <- nsub + 1  


pattern <- paste(substr('000000000',1,5-nchar(nsub)),nsub,sep="")


path<-dir1  # directory of Nabsed folder / all SWAT files



flist<-list.files(path=path,pattern=pattern,full.names = TRUE)

flist.c <- substr(flist,nchar(path)+2,nchar(path)+ 2 + 8)

n <- which(as.numeric(flist.c)>=nsub*10000 & as.numeric(flist.c)< nsub.a*10000 )




flist <- flist[n]



path<-dir2


comflist<-list.files(path=path,full.names = TRUE)

flist<-c(flist,comflist)


x<-data.frame(x=flist)


path<-dir3 # directory

setwd(path)


# create folder for subbasin

dir.create( paste('S',nsub,sep=''))  

# copy files to directory of subbasin

file.copy(flist, paste(path,'/',paste('S',nsub,sep=''),sep=""),overwrite = TRUE)


replace <- paste(substr('000000000',1,5-nchar(nsub)),nsub,'0000',sep='')


fileConn <- file(paste(path,'/',paste('S',nsub,sep=''),'/fig.fig',sep=""))

tx  <- readLines(fileConn)

tx2  <- gsub(pattern = "000000000", replace = replace, x = tx)


writeLines(tx2, con=fileConn)


close(fileConn)

path<-paste(path,'/',paste('S',nsub,sep=''),sep='')

return(path)
}



