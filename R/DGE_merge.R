#A function for merging any number of DGE text files

#you should manually remove the first + sign from the filename. That is a buf I haven't fixed yet.
##devtools::load_all("~/dftoolz")

DGE_merge<-function(samp.name.list, txtfile = F, name.barc=T, path = F){
  
  i=0
  DGE_comb<-dftoolz::DGE_load(samp.name.list[1], name.barc = name.barc, path = path)
  print(paste(samp.name.list[1], "loaded"))
  DGE_filename=samp.name.list[1]
  
  
  i=length(samp.name.list)
  for (samp.name in samp.name.list[2:i]){
    
    DGE_filename<-paste0(DGE_filename,"+",samp.name)
    
    DGEn<-dftoolz::DGE_load(samp.name, name.barc = name.barc, path = path)
    print(paste(samp.name, "loaded"))
    
    de_two_1 <- merge(DGE_comb,DGEn, by = 0, all = TRUE)
    de_two_1[is.na(de_two_1)] <- 0
    de2_two_1 <- as.data.frame(de_two_1, row.names = de_two_1[,1])
    DGE_comb <- de2_two_1[,-1]
    print(paste(samp.name, "merged"))
  }
  
  if (txtfile == T){
    write.table(DGE_comb, file = paste0(DGE_filename,"_dge.txt"), sep ="\t", col.names=NA)
    print(paste("textfile saved in", getwd()))
  }
  DGE_comb
}