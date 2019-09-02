# If txtfile = TRUE extract cell subtype annotations from .rds, (for example from individual clusters) to celltype.samplenname.txt if txtfile = FALSE or missing, create a data frame of subtypes in the global environment called cell.type


celltype_rds <- function(samp.name, txtfile=F){

    
  singler_load<- readRDS(paste0(getwd(),"/data/", samp.name,".singleR.rds"))
  
  if (txtfile == TRUE){
    write.table(singler_load[["singler"]][[1]][["SingleR.single"]][["labels"]], file=paste0(samp.name,"_celltype.txt"), sep = "\t")
    
    print(paste0("A file of UMI:s and celltypes named celltype.",samp.name,".txt has been created in the directory ", getwd()))
    
  }else{ 
    cell.type <<- singler_load[["singler"]][[1]][["SingleR.single"]][["labels"]]
    
  }
}