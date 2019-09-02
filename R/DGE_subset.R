#Extracts a subset DGE from original DGE assuming both DGE and the list of subset barcodes exist in the /data folder in working directory. dependent on dplyr. if you want to subset a specific type of cells, like "B_cell:Plasma_cell" or "Cluster 2" that is specified as subset_cell.
#
#  #If barcodes are a rowname not a column, develop this.
# if (missing("barc_rowname")){
#  barc_rowname = F
#}

DGE_subset <- function (samp.name, subset_cell=F, barc.name=F){
  
  cell.type<- read.table(file = paste0("data/",samp.name,"_celltype.txt"))
 
  if (subset_cell==T){
    cell.type <- subset(cell.type, grepl(subset_cell,cell.type[,1]))
    #cell.type <- subset(cell.type, grepl(subset_cell,cell.type[,2]))
    }
 
    cell.type <- as.vector(t(rownames(cell.type)))
    cell.type <- gsub("-",".", cell.type)
  
  # load DGE to get no of columns
  raw.data <- read.table(file = paste0("data/",samp.name,"_dge.txt"), header = TRUE, row.names = 1, colClasses =c("character", rep("numeric", 10)))
  no.cols <- ncol(raw.data)
  
  # load DGE with right no of columns
  raw.data <- read.table(file = paste0("data/",samp.name,"_dge.txt"), header = TRUE, row.names = 1, colClasses =c("character", rep("numeric", no.cols)))
  
  if(barc.name==T){
    colnames(raw.data)<-paste0(colnames(raw.data),"_", samp.name)
  }

  # Subset DGE
  DGE <<- raw.data %>% select(cell.type)

  #save_DGE
  DGE_save(DGE,samp.name=paste0(samp.name,"_",subset_cell))
}
