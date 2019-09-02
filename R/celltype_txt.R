# Load a celltype.samplename.txt for a sample through giving it's path.

celltype_txt <- function(filep1){
#cell.type <- read.table(paste0("celltype.",samp.name,".txt"))
  cell.type <<- read.table(filep1)
}
  