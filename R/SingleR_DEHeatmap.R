#generate heatmaps from singler. 
#

SingleR_DEHeatmap<-function(singler, excel =F, samp.name){
  library(Seurat)
  seurat<-singler[["seurat"]]
  all.markers <<- FindAllMarkers(object = seurat, only.pos = FALSE, min.pct = 0.25, thresh.use = 0.25)
  all.markers %>% group_by(cluster) %>% top_n(50, avg_logFC) -> top10
  DoHeatmap(object = seurat, genes.use = top10$gene, slim.col.label = TRUE, remove.key = TRUE, cex.row=2)
  
  if (excel==T){
    markers.excel <-all.markers
    markers.excel<-markers.excel[order(markers.excel$cluster,-markers.excel$avg_logFC,markers.excel$p_val  ),]
    write.csv(markers.excel, file = paste0(samp.name," cluster makers.csv"),row.names=T,quote=F,eol = "\n")
    seurat <- SetAllIdent(object = seurat, id = "ClusterNames_0.6")
    
    exp<-AverageExpression(seurat)
    write.csv(exp,file = paste0(samp.name," average gene exp.csv"),row.names=T,quote=F,eol = "\n")
  }
}
