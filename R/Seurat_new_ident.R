# assign a name such as "cluster 1, ctrl" "and cluster 1 stim" for each cell. The rerun findAllMarkers and heatmap.
Seurat_new_ident<-function(seurat.combined){
samp.clus.ident=c()

for (i in 1:length(seurat.combined@active.ident)){
  nw<-paste0(seurat.combined@meta.data[["seurat_clusters"]][i]," ",seurat.combined@meta.data[["stim"]][i])
  samp.clus.ident<-append(samp.clus.ident, nw, after = i)
}

#set the new identity
Idents(object = seurat.combined)<<- as.factor(samp.clus.ident)

}