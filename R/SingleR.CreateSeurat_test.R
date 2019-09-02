SingleR.CreateSeurat_test<-function (project.name, sc.data, min.genes = 200, min.cells = 2, 
          regress.out = "nUMI", npca = 10, resolution = 0.8, temp.dir = NULL) 
{
  mtgenes = "^mt-"
  if (packageVersion("Seurat") >= 3) {
    sc = CreateSeuratObject(sc.data, min.cells = min.cells, 
                            min.features = min.genes, project = project.name)
    percent.mito <- PercentageFeatureSet(object = sc, pattern = "^(?i)mt-")
    sc <- AddMetaData(object = sc, metadata = percent.mito, 
                      col.name = "percent.mito")
  }
  else {
    sc = CreateSeuratObject(sc.data, min.cells = min.cells, 
                            min.genes = min.genes, project = project.name)
    mito.genes <- grep(pattern = mtgenes, x = rownames(x = sc@data), 
                       value = TRUE, ignore.case = TRUE)
    percent.mito <- colSums((sc.data[mito.genes, ]))/colSums(sc.data)
    sc <- AddMetaData(object = sc, metadata = percent.mito, 
                      col.name = "percent.mito")
    sc <- NormalizeData(object = sc, normalization.method = "LogNormalize", 
                        scale.factor = 10000)
  }
  if (packageVersion("Seurat") >= 3) {
    sc <- SCTransform(object = sc, vars.to.regress = "percent.mito", 
                      verbose = FALSE, do.correct.umi = T)
    sc <- RunPCA(object = sc, verbose = FALSE)
    sc <- FindNeighbors(object = sc, dims = 1:30)
    sc <- FindClusters(object = sc)
    if (ncol(sc@assays$RNA@data) < 100) {
      sc <- RunTSNE(sc, perplexity = 10, dims = 1:npca)
    }
    else {
      sc <- RunTSNE(sc, dims = 1:30)
    }
    #sc <- RunUMAP(sc, dims = 1:30, verbose = FALSE)
  }
  else {
    sc <- FindVariableGenes(object = sc, mean.function = ExpMean, 
                            dispersion.function = LogVMR, x.low.cutoff = 0.0125, 
                            x.high.cutoff = 3, y.cutoff = 0.5, do.contour = F, 
                            do.plot = F)
    if (!is.null(regress.out)) {
      sc <- ScaleData(object = sc, vars.to.regress = regress.out)
    }
    else {
      sc <- ScaleData(object = sc)
    }
    sc <- RunPCA(object = sc, pc.genes = sc@var.genes, do.print = FALSE)
    sc <- FindClusters(object = sc, reduction.type = "pca", 
                       dims.use = 1:npca, resolution = resolution, print.output = 0, 
                       save.SNN = F, temp.file.location = temp.dir)
    if (ncol(sc@data) < 100) {
      sc <- RunTSNE(sc, dims.use = 1:npca, do.fast = T, 
                    perplexity = 10)
    }
    else {
      sc <- RunTSNE(sc, dims.use = 1:npca, do.fast = T, 
                    check_duplicates = FALSE)
    }
  }
  sc
}