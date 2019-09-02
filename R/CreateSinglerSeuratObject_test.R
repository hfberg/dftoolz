CreateSinglerSeuratObject_test<-function (counts, annot = NULL, project.name, min.genes = 200, 
          technology = "10X", species = "Human", citation = "", ref.list = list(), 
          normalize.gene.length = F, variable.genes = "de", fine.tune = T, 
          reduce.file.size = T, do.signatures = F, min.cells = 2, npca = 10, 
          regress.out = "nUMI", do.main.types = T, reduce.seurat.object = T, 
          temp.dir = NULL, numCores = SingleR.numCores) 
{
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop("Seurat needed for this function to work. Please install it.", 
         call. = FALSE)
  }
  print(project.name)
  print("Reading single-cell data...")
  sc.data = ReadSingleCellData(counts, annot)
  print("Create Seurat object...")
  seurat = SingleR.CreateSeurat_test(project.name, sc.data$counts, 
                                min.genes = min.genes, min.cells = min.cells, regress.out = regress.out, 
                                npca = npca, temp.dir = temp.dir)
  if (packageVersion("Seurat") >= 3) {
    data = seurat@assays$RNA@data
    clusters = seurat@active.ident
  }
  else {
    data = seurat@data
    clusters = seurat@ident
  }
  orig.ident = sc.data$orig.ident[colnames(data)]
  counts = as.matrix(sc.data$counts[, colnames(data)])
  seurat@meta.data$orig.ident = factor(orig.ident)
  if (reduce.seurat.object == T) {
    if (packageVersion("Seurat") >= 3) {
      seurat@assays$RNA@counts = matrix()
      seurat@assays$RNA@scale.data = matrix()
    }
    else {
      seurat@raw.data = c()
      seurat@scale.data = c()
      seurat@calc.params = list()
    }
  }
  print("Creat SingleR object...")
  singler = CreateSinglerObject(counts, orig.ident, project.name, 
                                min.genes = min.genes, technology, species, citation, 
                                ref.list, normalize.gene.length, variable.genes, fine.tune, 
                                do.signatures, clusters, do.main.types, reduce.file.size, 
                                temp.dir, numCores = numCores)
  singler$seurat = seurat
  if (packageVersion("Seurat") >= 3) {
    singler$meta.data$xy = seurat@reductions$tsne@cell.embeddings
    singler$meta.data$clusters = seurat@active.ident
  }
  else {
    singler$meta.data$xy = seurat@dr$tsne@cell.embeddings
    singler$meta.data$clusters = seurat@ident
  }
  singler
}