DGE_load<- function(samp.name, name.barc=F, path=F){

  if (path ==F){
  raw.data <- read.table(file = paste0(getwd(),"/data/",samp.name,"_dge.txt"), header = TRUE, row.names = 1)
  no.cols <- ncol(raw.data)

  # load DGE with right no of columns
  raw.data <- read.table(file = paste0("data/",samp.name,"_dge.txt"), header = TRUE, row.names = 1, colClasses =c("character", rep("numeric", no.cols)))
  }
  
  else{
    raw.data <- read.table(file = samp.name, header = TRUE, row.names = 1, colClasses =c("character", rep("numeric", 2)))
    no.cols <- ncol(raw.data)
    
    # load DGE with right no of columns
    raw.data <- read.table(file = samp.name, header = TRUE, row.names = 1, colClasses =c("character", rep("numeric", no.cols)))
  }
    

  if (name.barc == T){
    colnames(raw.data)<-paste0(colnames(raw.data),"_",samp.name)
  }
  return(raw.data)
}