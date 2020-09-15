#chargement des packages ==================================================
library(readxl)
library(xlsx)
library(ggplot2)
library(knnflex)
library(reshape2)

#fonction pour codifier les donnes ===========================================================
getModa= function(data)
{
  codes=matrix(,ncol = 3,byrow = F,dimnames = list(NULL,c("Value","Code","ColumnName")))
  for (col in 1:ncol(data)){
    column=data[,col]
    if(sapply(column,class)=="character")
    {
      colName=colnames(data)[col]
      mod=rownames(table(column))
      cd=0:(length(mod)-1)
      
      tst=which(mod %in% codes[,"Value"])
      if(length(tst)>0)
      {
        new_mod = mod[-tst]
        val_index=which(codes[,"Value"]%in%mod[tst])
        codes[val_index,"ColumnName"]=paste(codes[val_index,"ColumnName"],",",colName)
        
        values_ex=codes[val_index,"Code"]
        cd_ex_index=which(cd %in% values_ex)
        cd=cd[-cd_ex_index]
      }else{
        new_mod=mod
      }
      newCodes=cbind(new_mod,cd,rep(colName,length(new_mod)))
      codes=rbind(codes,newCodes)
    }
  }
  return(codes)
}
#========================================================================================
codifier= function(data)
{
  codes=getModa(data)
  rownames(codes)=codes[,1]
  for (col in colnames(data)){
    column=data[,col]
    if(sapply(column,class)=="character")
    {
      #print("=================================")
      #print(paste(col," : begin"))
      mod=rownames(table(column))
      
      for(i in mod)
      {
        #print(paste(":::::: Mod :",i," |code:",codes[i,"Code"]))
        #print(mod)
        index=which(unlist(column) %in% i)
        data[index,col]=codes[i,"Code"]
      }
      data[,col]=as.numeric(unlist(data[,col]))
      #print(paste(col," : finish"))
      #print("=================================")
      
    }
  }
  return(list(data,codes) )
}

codifierAndSave= function(data,filename="data.xlsx")
{
  file=codifier(data)
  write.xlsx2(as.data.frame(file[[1]]), file=filename, sheetName="Data", col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE, password=NULL)
  write.xlsx2(as.data.frame(file[[2]]), file=filename, sheetName="CodeValue", col.names=TRUE, row.names=FALSE, append=TRUE, showNA=TRUE, password=NULL)
  return(file[[1]])
}


#============= fonction qui nous permet appliquer l'algorithme de Knn pour estimers les valeurs manquants

#===============================================
knn_Prepare=function(data,colname) {
  
  rownames(data)=1:nrow(data)
  
  
  #if(sapply(data[,colname],class)=="character")
  if(mode(data[,colname])=="character")
  {
    method="majority"
  }else{
    method="mean"
  }
  Nas=which(is.na(data[,colname]))
  data_NA=data[Nas,]
  
  data_NoNA=data[-Nas,]
  
  dataTT=rbind(data_NoNA,data_NA)
  
  k=ceiling(sqrt(nrow(dataTT)))
  
  train=1:nrow(data_NoNA)
  test=(nrow(data_NoNA)+1):nrow(dataTT)
  
  clIndex=which(colnames(dataTT)==colname)
  
  return(list(knn_data=dataTT[,-clIndex],knn_cl=data_NoNA[,clIndex],knn_train=train,knn_test=test,knn_k=k,knn_method=method,originalEmptyIndex=Nas))
}
knn_estime=function(data) {
  
  colNA=colSums(is.na(data))
  colsToEstimate=colNA[colNA>0]
  colsToEstimate=sort(colsToEstimate,decreasing = T)
  colsToEstimate=names(colsToEstimate)
  
  for(col in colsToEstimate)
  {
    data=as.data.frame(data)
    tst=knn_Prepare(data,col)
    dataCodifier=codifier(tst$knn_data)[[1]]
    message(col)
    dataCodifier_n=as.data.frame(lapply(dataCodifier,nor))
    kdist <- knn.dist(dataCodifier_n)
    preds <- knn.predict(tst$knn_train, tst$knn_test, tst$knn_cl ,kdist, k=tst$knn_k, agg.meth=tst$knn_method)
    #=============
    esti=as.numeric(preds)
    data[tst$originalEmptyIndex,col]=esti
  }
  return(data)
}


