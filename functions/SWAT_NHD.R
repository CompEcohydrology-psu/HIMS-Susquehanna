################################################################################
# This function identies MHD streams inside a coarser landscape unit ( e.g., HUC-12)

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

# References: 

# SWAT source codes

# SWAT 2009 theory

################################################################################

##### Readme

# terminals: Matrix of SWAT subbasin terminals on NHD+ scale

# nsub: SWAT subbasin number

# fig: Matrix of SWAT .fig file

# dir: folder directory where terminals.csv (most downstream NHD IDs) and ...

# fig.csv (SWAT configuration file are available

################################################################################



source(".../functions/Upstream.R") # call Upstream

  
 SWAT_NHD<-function(nsub,dir){
   
   terminals<-as.matrix(read.csv(paste(dir,'/terminals.csv',sep=""),header = FALSE))
   
   fig<-as.matrix(read.csv(paste(dir,'/fig.csv',sep=""),header = FALSE))
  
 sub_ter<-na.omit(terminals[nsub,])
 
 upstream.sub<- na.omit(fig[nsub,]) # upstream subbasins
 
 upstream_ter<- na.omit(as.vector(terminals[upstream.sub,])) 
 
 sub_set<-c()
 
 upstream_set<-c()
 
 for(j in sub_ter){
  
  sub_set<-c(sub_set,Upstream(id=j,flowline=flowline))
  
}

sub_set<-unique(sub_set)


if(length(upstream_ter)>0){



for(j in upstream_ter ){
  
upstream_set<-c(upstream_set,Upstream(id=j,flowline=flowline))

}

}


upstream_set<-union(upstream_set,upstream_ter)

sub_set<-union(sub_set,sub_ter)


net_set<-setdiff( sub_set,upstream_set)

return(net_set)

} # end of function




 
 
