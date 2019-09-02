Seurat_cluster_heatmap <- function(seurat_name){
  
  seurat_object=paste0(seurat_name,"@meta.data[[","seurat_clusters","]]")
  top_DE_ctrl_stim<-data.frame()
  
  for (i in 0:length(levels(x=seurat_object))-1){
    id1= paste(i,"ctrl")
    id2 = paste(i, "stim")
    pdf_name = paste("Heatmap cluster", i)
    
    #Find DE genes in a cluster between ctrl and stim 
    seurat_subset<-Seurat::SubsetData(x = seurat_name, idents = c(id1, id2))
    markers_subset <- FindAllMarkers(seurat_subset, only.pos = TRUE, min.pct = 0.25)
    
    #top10_subset <- markers_subset %>% group_by(cluster) %>% top_n(n = 10, wt = avg_logFC)
    #DoHeatmap(seurat_subset, features = top10_subset$gene, slot = "scale.data", size = 3) + NoLegend() + theme(axis.text.y = element_text(size = 5))
    pdf(file = pdf_name, height = 8.50, width = 11)
    DoHeatmap(seurat_subset, features = markers_subset$gene, slot = "scale.data", size = 3) + NoLegend() + theme(axis.text.y = element_text(size = 7))
    dev.off()
    
    top1_sub <- markers_subset %>% group_by(cluster) %>% top_n(n = 1, wt = avg_logFC)
    top_DE_ctrl_stim<- rbind(top_DE_ctrl_stim, as.data.frame(top1_sub))
    
  }
  top_DE_ctrl_stim
  print(paste("All heatmaps are saved in folder", getwd()))
}