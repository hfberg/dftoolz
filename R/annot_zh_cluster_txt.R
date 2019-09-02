# Generate a .txt document with each cell barcode and its cell ranger cluster. Arguments are: the sample name to create the file, 
# the path to where the barcodes for the clusters are kept and a list of how many clusters you want to include, for example 1:7 if there are 7 clusters 
# in the folder and you want to include all.
 
# Make sure to define the row.name column when loading the file.

annot_extr.10X_txt<-function(samp.name, clus_path, clus_list) {

write.table("cluster" ,file=paste0(samp.name,"_cluster.txt"), row.names = "bacode",col.names = F)
  
for (cluster in clus_list){
  cell.barc<- read.table(paste0(clus_path,"/Cluster",cluster,"_samplelist.xls"))
  cell.barc <- gsub("-",".", cell.barc[,1])
  clus.name <- paste("Cluster", cluster, samp.name)
  write_clust=rep(clus.name, NROW(cell.barc))
  write.table(write_clust, file=paste0(samp.name,"_cluster.txt"), sep="\t", eol ="\n",row.names = cell.barc, col.names = F,append = TRUE)
  
}
}
