CreateCombinedIdentity<-function(seurat,ident1,ident2){
  new.ident=c()

if (ident1 == "active.ident"){  
  for (i in 1:length(seurat@active.ident)){
    nw<-paste0(seurat@active.ident[i]," ",seurat@meta.data[[ident2]][i])
    new.ident<-append(new.ident, nw, after = i)
  }
}
else if (ident2 == "active.ident"){
  for (i in 1:length(seurat@active.ident)){
    nw<-paste0(seurat@meta.data[[ident1]][i]," ",seurat@active.ident[i])
    new.ident<-append(new.ident, nw, after = i)
  }
}
else {
  for (i in 1:length(seurat@meta.data[[ident1]])){
    nw<-paste0(seurat@meta.data[[ident1]][i]," ",seurat@meta.data[[ident2]][i])
    new.ident<-append(new.ident, nw, after = i)
  }
}
new.ident<<-as.factor(new.ident)
  #seurat@meta.data[["new.ident"]]<-c()

# set the new meta data to the identity to be evaluated.
# seurat@meta.data[["new.ident"]]<<- as.factor(new.ident)
  #seurat@meta.data[["orig.ident"]]<- as.factor(new.ident)
  #Idents(seurat=seurat)<-new.ident
  #seurat@meta.data[["orig.ident"]]<-as.vector(new.ident)
  #seurat@orig.ident<-as.vector(new.ident)
  #Idents(object = seurat)<<- as.factor(new.ident)
  #return(seurat)
# seurat@active.ident<<- as.factor(new.ident)
}
