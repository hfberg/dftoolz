# Extract annotated cells from the previously loaded SingleR object.

celltype_SingleR <- function(SingleR, txtfile = F, samp.name=NULL){
  
cell.type <<- as.data.frame(SingleR[["singler"]][[1]][["SingleR.single"]][["labels"]])

if (txtfile==T){
  write.table(cell.type, file=paste0(samp.name,"_celltype.txt"))
}

}