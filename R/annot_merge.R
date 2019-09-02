#A function to merge the annotation files any number of samples

annot_merge<-function(samp.name.list, txtfile=F, samp = F){
  
  if (samp==T){
    path_annot<-paste0(getwd(),"/data/",samp.name.list[1], "_samp.txt") 
  }
  else{
  path_annot<-paste0(getwd(),"/data/",samp.name.list[1], "_cluster.txt")
  }
  
  annot<-read.table(path_annot)
  annot<-annot[-1,]
  annot[,1]<-paste0(annot[,1],"_",samp.name.list[1])
  annot_filename=samp.name.list[1]
  
  i=length(samp.name.list)
  for(samp.name in samp.name.list[2:i]){
    i<-i+1
    annot_filename<-paste0(annot_filename,"+",samp.name)

    if (samp==T){
      path_annot<-paste0(getwd(),"/data/",samp.name, "_samp.txt") 
    }
    else{
      path_annot<-paste0(getwd(),"/data/",samp.name, "_cluster.txt")
    }
    
    annotn<-read.table(path_annot)
    annotn<-annotn[-1,]
    annotn[,1]<-paste0(annotn[,1],"_",samp.name)

    annot<-rbind(annot,annotn)
  }
  
  if (txtfile ==T){
    write.table(annot, file=paste0(annot_filename,"_cluster.txt"), row.names=F, sep = "\t", col.names=c("barcode", "cluster"))
  }
}
