#a small function with what is needed to save DGE:s correctly
DGE_save<-function(DGE, samp.name){
  write.table(DGE, file = paste0(samp.name,"_dge.txt"), sep ="\t", col.names=NA)
}
