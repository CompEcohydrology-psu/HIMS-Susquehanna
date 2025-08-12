################################################################################
# Finding Upstream HUC12 Subbasins
#
# Pennsylvania State University
# Computational Ecohydrology Lab - https://compecohydrology-psu.github.io/
#
# Code Developer: Rashid Ansari - rashid.hojjatansari@gmail.com
# Corresponding Author: Raj Cibin - czr58@psu.edu
#
# Description:
# This script identifies upstream HUC12 subbasins that immediately contribute
# to a specified subbasin.
################################################################################

setwd("/storage/home/abh6005/work/swat/data")

fig<-as.matrix(read.csv('fig.csv',header = FALSE))


HUC12_ups<-function(basin){


upstream<-c(basin)


n<-1

set2<-upstream


while(n==1){

set1<-c()


for(j in upstream){
  
  
 set1<-c(set1,as.vector(na.omit(fig[j,])))
  
  
}

set2<-c(set2,set1)

upstream<-set1



if(length(set1)==0){n<-0}



} # while


return(set2)

} # function