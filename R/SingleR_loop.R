# a loop for creating multiple singleR annotated files from DGE:s
# 

SingleR_loop <- function (samp.name.list, max.cell){

  library(SingleR)
  library(Seurat)

  library(ggplot2)
  library(dplyr)
  for (samp.name in samp.name.list){
    print(samp.name)
    DGE <- dftoolz::DGE_load(samp.name, name.barc=F)
    
    no.col = length(colnames(DGE))
    
    if (no.col > max.cell){
      
      seurat<<- SingleR.CreateSeurat_test(project.name= samp.name, sc.data = DGE)
      
      singler<-dftoolz::CreateBigSingleRObject_test(counts=GetAssayData(object = seurat), annot=paste0(getwd(),"/data/",samp.name,"_cluster.txt"),project.name=samp.name, xy=  	Embeddings(object = seurat, reduction = "tsne"),clusters= 	Idents(object = seurat),N=max.cell, min.genes = 0, fine.tune=T, numCores =16, samp.name = samp.name)
     
       #convert
      singler.S4 <- convertSingleR2Browser_test(singler, use.singler.cluster.annot = T)
    }
    
    else{
      singler <- CreateSinglerSeuratObject_test(counts = DGE , annot=paste0(getwd(),"/data/",samp.name,"_cluster.txt"), min.genes = 1, min.cells = 200, project.name = samp.name, technology = "10X Genomics", species = "Human", fine.tune = T, numCores = 16)
      singler.S4 = convertSingleR2Browser_test(singler)
    }
    
    #save final SingleR Objects
    saveRDS(singler,paste0(samp.name,'.singleR.rds'))
    saveRDS(singler.S4, paste0(samp.name,'.singleR.S4.rds'))
    
  }
}
