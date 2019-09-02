#Sometimes the original identites is not properly arranged in the singler object. This function can be used to arrage the original identites properly.

SingleR_orig.ident<-function(singler, annot){
#If you forgot how to load annot, example below
#annot<-read.table("/home/drugfarm/proj.zhqugen/PBMC/process/data/s_SHWH_cluster.txt", sep="\t", head=T)
sub<-as.character(names(singler[["singler"]][[1]][["SingleR.single"]][["labels"]]))

annot_sub<-annot[match(sub, annot$barcode),]

singler[["meta.data"]][["orig.ident"]]<-annot_sub[,2]
names(singler[["meta.data"]][["orig.ident"]])<-annot_sub[,1]
singler[["singler"]][[1]][["SingleR.single"]][["cell.names"]]<-annot_sub[,1]

singler.S4<-convertSingleR2Browser(singler)
return(singler)
return(singler.S4)
}