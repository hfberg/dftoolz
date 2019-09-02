convertSingleR2Browser_test<-function (singler, use.singler.cluster.annot = T) 
{
  ref.names = unlist(lapply(singler$singler, FUN = function(x) x$about$RefData))
  cell.names = rownames(singler$singler[[1]]$SingleR.single$labels)
  labels = as.data.frame(sapply(singler$singler, FUN = function(x) x$SingleR.single$labels))
  if (!is.null(singler$singler[[1]]$SingleR.single.main)) {
    labels.main = as.data.frame(sapply(singler$singler, FUN = function(x) x$SingleR.single.main$labels))
    labels = cbind(labels, labels.main)
    colnames(labels) = c(ref.names, paste0(ref.names, ".main"))
  }
  else {
    colnames(labels) = c(ref.names)
  }
  rownames(labels) = cell.names
  labels1 = data.frame()
  if (!is.null(singler$singler[[1]]$SingleR.single$labels1)) {
    labels1 = as.data.frame(sapply(singler$singler, FUN = function(x) x$SingleR.single$labels1))
    if (!is.null(singler$singler[[1]]$SingleR.single.main)) {
      labels1.main = as.data.frame(sapply(singler$singler, 
                                          FUN = function(x) x$SingleR.single.main$labels1))
      labels1 = cbind(labels1, labels1.main)
      colnames(labels1) = c(ref.names, paste0(ref.names, 
                                              ".main"))
    }
    else {
      colnames(labels1) = c(ref.names)
    }
    rownames(labels1) = cell.names
  }
  labels.clusters = data.frame()
  labels.clusters1 = data.frame()
  if (use.singler.cluster.annot == T) {
    if (length(levels(singler$meta.data$clusters)) > 1) {
      if (!is.null(singler$singler[[1]]$SingleR.clusters)) {
        labels.clusters = as.data.frame(sapply(singler$singler, 
                                               FUN = function(x) x$SingleR.clusters$labels))
        if (!is.null(singler$singler[[1]]$SingleR.clusters.main)) {
          labels.clusters.main = as.data.frame(sapply(singler$singler, 
                                                      FUN = function(x) x$SingleR.clusters.main$labels))
          labels.clusters = cbind(labels.clusters, labels.clusters.main)
          colnames(labels.clusters) = c(ref.names, paste0(ref.names, 
                                                          ".main"))
        }
        else {
          colnames(labels.clusters) = c(ref.names)
        }
        rownames(labels.clusters) = levels(singler$meta.data$clusters)
      }
      if (!is.null(singler$singler[[1]]$SingleR.cluster$labels1)) {
        if (!is.null(singler$singler[[1]]$SingleR.clusters)) {
          labels.clusters1 = as.data.frame(sapply(singler$singler, 
                                                  FUN = function(x) x$SingleR.clusters$labels1))
          if (!is.null(singler$singler[[1]]$SingleR.clusters.main)) {
            labels.clusters.main = as.data.frame(sapply(singler$singler, 
                                                        FUN = function(x) x$SingleR.clusters.main$labels1))
            labels.clusters1 = cbind(labels.clusters1, 
                                     labels.clusters.main)
            colnames(labels.clusters1) = c(ref.names, 
                                           paste0(ref.names, ".main"))
          }
          else {
            colnames(labels.clusters1) = c(ref.names)
          }
          rownames(labels.clusters1) = levels(singler$meta.data$clusters)
        }
      }
    }
  }
  scores = lapply(singler$singler, FUN = function(x) x$SingleR.single$scores)
  if (!is.null(singler$singler[[1]]$SingleR.single.main)) {
    scores.main = lapply(singler$singler, FUN = function(x) x$SingleR.single.main$scores)
    scores = c(scores, scores.main)
    names(scores) = c(ref.names, paste0(ref.names, ".main"))
  }
  else {
    names(scores) = c(ref.names)
  }
  clusters = data.frame(clusters = singler$meta.data$clusters)
  rownames(clusters) = cell.names
  ident = data.frame(orig.ident = singler$meta.data$orig.ident)
  rownames(ident) = cell.names
  singler.small = new("SingleR", project.name = singler$meta.data$project.name, 
                      xy = singler$meta.data$xy, labels = labels, labels.NFT = labels1, 
                      labels.clusters = labels.clusters, labels.clusters.NFT = labels.clusters1, 
                      scores = scores, clusters = clusters, ident = ident, 
                      other = data.frame(singler$signatures), expr = singler[["seurat"]]@assays[["RNA"]]@data, 
                      meta.data = c(Citation = singler$singler[[1]]$about$Citation, 
                                    Organism = singler$singler[[1]]$about$Organism, Technology = singler$singler[[1]]$about$Technology))
  singler.small
}