#A function for loading annotations the way SingleR likes it. If the annotation file has all clusters as annotations, the file should be called samp.name_clusters.txt. if the annotations file has sample names a s annotations, the sile should be called samp.name_samp.name.txt

annot_load<-function(samp.name, cluster = T, samp = F, barc.name = F){
  
  if (cluster ==T){
  annot_path=paste0(getwd(),"/data/",samp.name,"_cluster.txt")
  }
  
  if (samp ==T){
    annot_path=paste0(getwd(),"/data/",samp.name,"_samp.txt")
  }
  annot <- read.table(annot_path, header = TRUE, sep = "\t", row.names = 1, as.is = TRUE)
  
  if (barc.name == T){
    rownames(annot)<- paste0(rownames(annot), samp.name)
  }
  annot
}
