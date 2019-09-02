# a function to extract a subset of cells from the annotation file. For example to extract all cell barcodes left after filtering a DGE when the annotation file was created from the DGE itself. 
annot_subset<- function(annot_path, sub, txtfile=F, samp.name = NULL){
  annot <- read.table(annot_path, header = TRUE, sep = "\t", row.names = 1, as.is = TRUE)
  annot_sub<-subset(annot, rownames(annot) %in% sub)
  
  if (txtfile==T){
   write.table(annot_sub, file=paste0(samp.name,"samp.name.txt")) 
  }
}