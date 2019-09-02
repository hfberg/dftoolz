#Plot the cell type annoation from R

SingleR_plot_annot<-function(singler, main=F){
  
  if (main == T){
    singler[["singler"]][[1]][["SingleR.single.main"]][["cell.names"]]<-singler[["singler"]][[1]][["SingleR.single"]][["cell.names"]]
  out = SingleR.PlotTsne(singler$singler[[1]]$SingleR.single.main, singler$meta.data$xy, score.thres=0.25,do.label = TRUE, do.letters = F, dot.size = 1.5, alpha = 0.7)
  }
  if  (main == F){
    out = SingleR.PlotTsne(singler$singler[[1]]$SingleR.single, singler$meta.data$xy, score.thres=0.25,do.label = TRUE, do.letters = F, dot.size = 1.5, alpha = 0.7)
  }
  out$p
  }