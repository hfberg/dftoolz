# creating a barplot comparing cell types in two different samples
# if only one sample is used, remember to define samp.name like this:
# celltype_plot(cell.type1, samp.name1="C25")
#' @import data.table

celltype_plot <- function(cell.type1, cell.type2, samp.name1, samp.name2){
  #for one sample only
  if(missing(cell.type2) && missing(samp.name2)){
    tot <- NROW(cell.type1)
    colnames(cell.type1) = "Type"
    cell.type1a<-plyr::count(cell.type1, 'Type')
    cell.type1a <- data.table::as.data.table(cell.type1a)
    cell.type1a <- cell.type1a[,percen := (freq/tot)*100]
    par(mar=c(12,4.1,4.1,2.1))
    barplot(as.vector(cell.type1a$percen), names.arg = cell.type1a$Type, las=2,ylab="percent", col = c("coral"), cex.names =0.7, main =paste("Cell type percentage in sample", samp.name1), legend= samp.name1)
    
    #for two samples compares
  }else{
    #calculate percentage and prepare data frame for cell.type1
    tot <- NROW(cell.type1)
    colnames(cell.type1) = "Type"
    cell.type1a<-plyr::count(cell.type1, 'Type')
    cell.type1a <- data.table::as.data.table(cell.type1a)
    cell.type1a <- cell.type1a[,percen := (freq/tot)*100]
    cell.type1a <- as.data.frame(cell.type1a[,-2])
    
    #calculate percentage and prepare data frame for cell.type2
    tot <- NROW(cell.type2)
    colnames(cell.type2) = "Type"
    cell.type2a<-plyr::count(cell.type2, 'Type')
    cell.type2a <- data.table::as.data.table(cell.type2a)
    cell.type2a <- cell.type2a[,percen := (freq/tot)*100]
    cell.type2a <- as.data.frame(cell.type2a[,-2])
    
    # merge and plot data frames 
    cell.merge <<- merge(x = cell.type1a, y = cell.type2a, by = "Type", all = TRUE)
    cell.merge2<-cell.merge[,-1]
    rownames(cell.merge2)<-cell.merge[,1]
    par(mar=c(12,4.1,4.1,2.1))
    barplot(t(as.matrix(cell.merge2)),beside=T, names.arg = rownames(cell.merge2), las=2, col = c("darkblue","red"), ylab="percent", cex.names =0.7, main =paste("Cell type percentage in", samp.name1, "and", samp.name2))
    legend("topright",cex = 0.4, legend= c(samp.name1, samp.name2), fill = 1:6,ncol = 2,)
  }
}


#celltype_plot <- function(cell.type1, samp.name1){
 # #for one sample only
#
 #   tot <- NROW(cell.type1)
  #  colnames(cell.type1) = "Type"
   # cell.type1<<-cell.type1
#    cell.type1a <-plyr::count(cell.type1, 'Type')
#    cell.type1a <- data.table::as.data.table(cell.type1a)
#    cell.type1a <- cell.type1a[,percen := (freq/tot)*100]
#    par(mar=c(12,4.1,4.1,2.1))
#    barplot(as.vector(cell.type1a$percen), names.arg = cell.type1a$Type, las=2,ylab="percent", col = c("coral"), cex.names =0.7, main ="Cell type #percentage in sample", legend= samp.name1)
   
    #for two samples compares
#}

