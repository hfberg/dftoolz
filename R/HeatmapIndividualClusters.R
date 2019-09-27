#A loop to extract a cluster from a seurat seurat and plot the differentially expressed genes between ctrl and stim
# within that cluster. THe cluster is then saved as "Heatmap clusternumber" in the working directory. 
# Next cluster is then analyzed.

HeatmapIndividualClusters<-function(seurat, top_n_genes = 1, text_size = 4){
  
  # Create an empty vector
  top_DE_ctrl_stim<-data.frame()
  tabl<-table(seurat@active.ident)
  
  for (i in sort(unique(seurat@meta.data[["seurat_clusters"]]))){
    
    #set identities
    ident1 = paste(i,"ctrl")
    ident2 = paste(i,"stim")
    
    
    if (any(names(tabl) == ident1) && any(names(tabl) == ident2) == T){
    if (tabl[names(tabl) == ident1]>=3 && tabl[names(tabl) == ident2]>=3){
      # subset the clusters to compare
      seurat_subset<-subset(x = seurat, idents = c(ident1, ident2))
      
      # Find differentially expressed genes.
      markers_subset <- FindAllMarkers(seurat_subset, only.pos = TRUE, min.pct = 0.25)
      
      # Plot heatmap
      filen= paste0("Heatmap ", i)
      pdf(file=filen)
      print(DoHeatmap(seurat_subset, features = as.character(markers_subset$gene), slot = "scale.data", label = F) +  theme(axis.text.y = element_text(size = text_size)))
      dev.off()
      
      # extract the most differentiated genes between successful and unsuccessful in this cluster.
      top_n_genes_sub <- markers_subset %>% group_by(cluster) %>% top_n(n = top_n_genes, wt = avg_logFC)
      
      # save those genes in a data frame.
      top_DE_ctrl_stim<- rbind(top_DE_ctrl_stim, as.data.frame(top_n_genes_sub))
    }
    }
  }
  top_DE_ctrl_stim<<-top_DE_ctrl_stim
  
}
