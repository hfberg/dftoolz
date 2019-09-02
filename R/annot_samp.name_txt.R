#A function for generating the annotations file from sample names rather than cluster identities. In an upgrade, make sure the generation of a new txtfile for every new samp.name doesn't occur.

annot_samp.name_txt<-function(samp.name.list, barc.name = F, samp.name.print=F){
  DGE<- DGE_load(samp.name.list[1])
 
 if (samp.name.print ==F){
  write_annot<- rep(samp.name.list[1], ncol(DGE))
 }
 else
 write_annot<- rep(samp.name.print[1], ncol(DGE))
  
  i<- length(samp.name.list)
  filename_annot<- samp.name.list[1]
  for (samp.name in samp.name.list[2:i]){
    filename_annot=paste0(filename_annot,"+",samp.name)
  }
  
  
  if(barc.name==T && samp.name.print!=F){
    rnames<-paste0(colnames(DGE), "_",samp.name.print[1])
  }
 else if(barc.name==T  && samp.name.print==F){
    rnames<-paste0(colnames(DGE), "_",samp.name.list[1])
  }
  else{
    rnames<-colnames(DGE)
  }
  
  write.table("cluster" ,file=paste0(filename_annot,"_samp.txt"), row.names = "bacode",col.names = F)
  write.table(write_annot, file=paste0(filename_annot,"_samp.txt"), append=T, sep="\t", eol ="\n",row.names = rnames, col.names = F)
  
 
  if (i>1){
  for (samp.name in samp.name.list[2:i]){
    
    DGE<- DGE_load(samp.name)

    if (samp.name.print ==F){
      write_annot<- rep(samp.name.list[i], ncol(DGE))
    }
    else{
      write_annot<- rep(samp.name.print[i], ncol(DGE))
    }

 if(barc.name==T && samp.name.print!=F){
    rnames<-paste0(colnames(DGE), "_",samp.name.print[i])
  }
 else if(barc.name==T  && samp.name.print==F){
    rnames<-paste0(colnames(DGE), "_",samp.name.list[i])
  }
  else{
    rnames<-colnames(DGE)
  }

  
    
    write.table(write_annot, file=paste0(filename_annot,"_samp.txt"), append=TRUE, sep="\t", eol ="\n",row.names = rnames, col.names = F,)
  }
  }
}
