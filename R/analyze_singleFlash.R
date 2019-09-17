# can be removed

analyze_singleFlash<-function(ERG_LIST,FPRE,FREQ){
  ERG_LIST
  RESULTS<-as.data.frame(matrix(nrow=sum((!is.na(ERG_LIST$STIMULUS))),ncol=4))
  colnames(RESULTS)<-c("APOS","AVAL","BPOS","BVAL")
  rownames(RESULTS)<-make.unique(ERG_LIST$STIMULUS[!is.na(ERG_LIST$STIMULUS)])
  for (i in 1:length(ERG_LIST$AVG)){
    if (!is.null(ERG_LIST$AVG[[i]])){
      tmp<-(which.min(ERG_LIST$AVG[[i]]$Mean[FPRE:length(ERG_LIST$AVG[[i]]$Mean)])+FPRE)
      RESULTS[make.unique(ERG_LIST$STIMULUS[i]),]$APOS<-tmp/FREQ
      RESULTS[make.unique(ERG_LIST$STIMULUS[i]),]$AVAL<-min(ERG_LIST$AVG[[i]]$Mean[FPRE:length(ERG_LIST$AVG[[i]]$Mean)])
      RESULTS[make.unique(ERG_LIST$STIMULUS[i]),]$BPOS<-(which.max(ERG_LIST$AVG[[i]]$Mean[tmp:length(ERG_LIST$AVG[[i]]$Mean)])+tmp/FREQ)/FREQ
      RESULTS[make.unique(ERG_LIST$STIMULUS[i]),]$BVAL<-max(ERG_LIST$AVG[[i]]$Mean[RESULTS$APOS:length(ERG_LIST$AVG[[i]]$Mean)])
    }

  }



  ERG_LIST$FLASHSTATS<-RESULTS
  return (ERG_LIST)
}
