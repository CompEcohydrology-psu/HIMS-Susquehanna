################################################################################

#Finding Upstream Flowlines for an NHDPlus Stream Flowline

# Pennsylvania State University 

# Computational Echohydorlogy Lab - https://compecohydrology-psu.github.io/

# Code developer: Rashid Ansari - rashid.hojjatansari@gmail.com

# Corresponding Author:  Raj Cibin - czr58@psu.edu

# References: 

# SWAT source codes

# SWAT 2009 theory

################################################################################

##### Readme

# flowline: NHD+ flow line dataframe [prepare NHD+ according to instruction]

# id: iDs of streamflow of interest

################################################################################


Upstream<-function(id,flowline){
  
  
  up_node<-flowline$from_cor_id[flowline$core_id==id]
  
  
  set<-c(which(flowline$core_id==id))
  
  n<-c(1)
  
  
  while(length(n)>0){
    
    n<-which(flowline$to_cor_id %in% up_node)
    
    set<-c(set,n)
    
    up_node<-flowline$from_cor_id[n]
    
    
  }
  
  set_id<-unique(flowline$core_id[set])
  
  set_id<-setdiff(set_id,id)
  
  return(set_id)
  
}




