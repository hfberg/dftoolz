#load the entire list of celltypes from .RData. Important to run the following in the right order since R.data overwrites each other. If these files are deleated in the future, rest assured that the file celltype.samp.name.txt is saved in the results file of interest.

celltype_RData <- function(filep1, filep2,samp.name){

  load(filep1)
  write.table(singler[["singler"]][[1]][["SingleR.single"]][["labels"]], file=paste0("celltype.",samp.name,".txt"), sep = "\t")

  load(filep2)

  write.table(singler[["singler"]][[1]][["SingleR.single"]][["labels"]], file=paste0("celltype.",samp.name,".txt"), col.names = F, sep = "\t", append = T)

  return(paste0("A file of UMI:s and celltypes named celltype.",samp.name,".txt has been created in the directory ", getwd()))
}