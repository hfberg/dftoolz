SingleR.Combine_test<-function (singler.list, order = NULL, clusters = NULL, expr = NULL, 
          xy = NULL, fine.tune=T) 
{
  singler = c()
  singler$singler = singler.list[[1]]$singler
  for (j in 1:length(singler.list[[1]]$singler)) {
    singler$singler[[j]]$SingleR.cluster = c()
    singler$singler[[j]]$SingleR.cluster.main = c()
    singler$singler[[j]]$SingleR.single$clusters = c()
  }
  singler$meta.data = singler.list[[1]]$meta.data
  singler$meta.data$clusters = c()
  singler$meta.data$xy = c()
  singler$meta.data$data.sets = rep(singler$meta.data$project.name, 
                                    length(singler$meta.data$orig.ident))
  for (i in 2:length(singler.list)) {
    for (j in 1:length(singler$singler)) {
      if (singler.list[[i]]$singler[[j]]$about$RefData != 
          singler.list[[1]]$singler[[j]]$about$RefData) {
        stop("The objects are not ordered by the same reference data.")
      }
      singler$singler[[j]]$about$Organism = c(singler$singler[[j]]$about$Organism, 
                                              singler.list[[i]]$singler[[j]]$about$Organism)
      singler$singler[[j]]$about$Citation = c(singler$singler[[j]]$about$Citation, 
                                              singler.list[[i]]$singler[[j]]$about$Citation)
      singler$singler[[j]]$about$Technology = c(singler$singler[[j]]$about$Technology, 
                                                singler.list[[i]]$singler[[j]]$about$Technology)
      singler$singler[[j]]$SingleR.single$labels = rbind(singler$singler[[j]]$SingleR.single$labels, 
                                                         singler.list[[i]]$singler[[j]]$SingleR.single$labels)
      singler$singler[[j]]$SingleR.single$labels1 = rbind(singler$singler[[j]]$SingleR.single$labels1, 
                                                          singler.list[[i]]$singler[[j]]$SingleR.single$labels1)
      singler$singler[[j]]$SingleR.single$scores = rbind(singler$singler[[j]]$SingleR.single$scores, 
                                                         singler.list[[i]]$singler[[j]]$SingleR.single$scores)
      singler$singler[[j]]$SingleR.single.main$labels = rbind(singler$singler[[j]]$SingleR.single.main$labels, 
                                                              singler.list[[i]]$singler[[j]]$SingleR.single.main$labels)
      singler$singler[[j]]$SingleR.single.main$labels1 = rbind(singler$singler[[j]]$SingleR.single.main$labels1, 
                                                               singler.list[[i]]$singler[[j]]$SingleR.single.main$labels1)
      singler$singler[[j]]$SingleR.single.main$scores = rbind(singler$singler[[j]]$SingleR.single.main$scores, 
                                                              singler.list[[i]]$singler[[j]]$SingleR.single.main$scores)
      singler$singler[[j]]$SingleR.single$cell.names = c(singler$singler[[j]]$SingleR.single$cell.names, 
                                                         singler.list[[i]]$singler[[j]]$SingleR.single$cell.names)
      singler$singler[[j]]$SingleR.single.main$cell.names = c(singler$singler[[j]]$SingleR.single$cell.names, 
                                                              singler.list[[i]]$singler[[j]]$SingleR.single.main$cell.names)
    }
    singler$meta.data$project.name = paste(singler$meta.data$project.name, 
                                           singler.list[[i]]$meta.data$project.name, sep = "+")
    singler$meta.data$orig.ident = c(singler$meta.data$orig.ident, 
                                     singler.list[[i]]$meta.data$orig.ident)
    singler$meta.data$data.sets = c(singler$meta.data$data.sets, 
                                    rep(singler.list[[i]]$meta.data$project.name, length(singler.list[[i]]$meta.data$orig.ident)))
  }
  for (j in 1:length(singler$singler)) {
    if (!is.null(order)) {
      singler$singler[[j]]$SingleR.single$labels = singler$singler[[j]]$SingleR.single$labels[order, 
                                                                                              ]
      singler$singler[[j]]$SingleR.single$labels1 = singler$singler[[j]]$SingleR.single$labels1[order, 
                                                                                                ]
      singler$singler[[j]]$SingleR.single$scores = singler$singler[[j]]$SingleR.single$scores[order, 
                                                                                              ]
      singler$singler[[j]]$SingleR.single$cell.names = singler$singler[[j]]$SingleR.single$cell.names[order]
      singler$singler[[j]]$SingleR.single.main$labels = singler$singler[[j]]$SingleR.single.main$labels[order, 
                                                                                                        ]
      singler$singler[[j]]$SingleR.single.main$labels1 = singler$singler[[j]]$SingleR.single.main$labels1[order, 
                                                                                                          ]
      singler$singler[[j]]$SingleR.single.main$scores = singler$singler[[j]]$SingleR.single.main$scores[order, 
                                                                                                        ]
      singler$singler[[j]]$SingleR.single.main$cell.names = singler$singler[[j]]$SingleR.single.main$cell.names[order]
    }
  }
  if (!is.null(clusters) && !is.null(expr)) {
    for (j in 1:length(singler$singler)) {
      if (is.character(singler$singler[[j]]$about$RefData)) {
        ref = hpca
        load("/home/drugfarm/Tools/hpca.rda")
      }
      else {
        ref = singler$singler[[j]]$about$RefData
      }
      singler$singler[[j]]$SingleR.clusters = SingleR("cluster", 
                                                      expr, ref$data, types = ref$types, clusters = factor(clusters), 
                                                      sd.thres = ref$sd.thres, genes = "de", fine.tune = fine.tune)
      singler$singler[[j]]$SingleR.clusters.main = SingleR("cluster", 
                                                           expr, ref$data, types = ref$main_types, clusters = factor(clusters), 
                                                           sd.thres = ref$sd.thres, genes = "de", fine.tune = fine.tune)
    }
    singler$meta.data$clusters = clusters
    if (!is.null(xy)) {
      singler$meta.data$xy = xy
    }
  }
  singler
}