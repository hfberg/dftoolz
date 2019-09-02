#a function for checking weather any duplicates in uppercase have sneaked in to the matrix.
DGE_duplicates<-function(DGE){

A<-rownames(DGE)
Ad<- A[duplicated(tolower(A))]


if (length(Ad)!=0){
print(paste0("duplicates exist: ", Ad))
}

else{
  print("no duplicates")
}
#develop to find genes with only uppercase letters
#A<-rownames(DGE)
#grep -oP '\b[A-Z0-9_]+\b'
#grep("^[A:Z]*[0:9]$",A)
#grep("[A-Z][0-9_]", A)
}