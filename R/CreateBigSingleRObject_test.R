CreateBigSingleRObject_test<-function (counts, annot = NULL, project.name, xy, clusters, N = N, min.genes = 0, technology = "10X", species = "Human", citation = "", ref.list = list(), normalize.gene.length = F, variable.genes = "de", fine.tune = T, reduce.file.size = T, do.signatures = F, do.main.types = T, temp.dir = getwd(), numCores = SingleR.numCores, samp.name=samp.name) 
{
  n = ncol(counts)
  s = seq(1, n, by = N)
  
  direct_name= paste0("/singler.temp_",project.name,"/")
  dir.create(paste0(temp.dir, direct_name), showWarnings = FALSE) 
  for (i in s) {
    print(i)
    A = seq(i, min(i + N - 1, n))
    singler = CreateSinglerObject(counts[, A], 
                                  project.name = project.name, min.genes = min.genes, 
                                  technology = technology, species = species, citation = citation, 
                                  do.signatures = do.signatures, clusters = NULL, numCores = numCores, fine.tune = fine.tune)

    save(singler, file = paste0(temp.dir, direct_name, 
                                project.name, ".", i, ".RData"))
  }
  
  singler.objects.file <- list.files(paste0(temp.dir, direct_name), pattern = "RData", full.names = T)
  singler.objects = list()
  for (i in 1:length(singler.objects.file)) {
    load(singler.objects.file[[i]])
    singler.objects[[i]] = singler
  }
  #singler[["singler"]][[1]][["about"]][["RefData"]]<-'hpca'
  singler = SingleR.Combine_test(singler.list=singler.objects, order = colnames(counts), expr = counts, 
                         clusters = clusters, xy = xy)
#saveRDS(singler, file=paste0(samp.name,"_backup_single.rds"))
  
singler[["seurat"]] = c()
singler[["seurat"]] <- seurat
  
if (is.character(annot)==T){
  annot <- read.table(annot, header = TRUE, sep = "\t", row.names = 1, as.is = TRUE)
  
  sub<-as.vector(colnames(singler[["seurat"]]@assays[["RNA"]]@counts))
  annot_sub<-subset(annot, rownames(annot) %in% sub) #this could be a bug, annot2 should not exist. Look at this if trouble occur.
  singler[["meta.data"]][["orig.ident"]]  = annot_sub[, 1]
  names(singler[["meta.data"]][["orig.ident"]] ) = rownames(annot_sub)
  singler[["singler"]][[1]][["SingleR.single"]][["cell.names"]] <- as.character(rownames(annot_sub))
}
  
  singler[["meta.data"]][["project.name"]]<- samp.name
  
  singler
}

