#plot how number of cells are correlated to the maximum score in a cell. higher score = safer annotations

SingleR_plot_GeneScore<-function(singler){
  df = data.frame(Max.Score=apply(singler$singler[[1]]$SingleR.single$scores,1,max), nGene=singler$seurat@meta.data$nGene,Orig.ident=singler$meta.data$orig.ident)
  
  ggplot(df,aes(x=nGene,y=Max.Score,color=Orig.ident))+geom_point()
  }