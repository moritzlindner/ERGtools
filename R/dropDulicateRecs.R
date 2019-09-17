#' drops Recordings from ERG_LIST objects
#'
#' This function drops duplicate ERG recordings ERG_list objects
#' @param ERG_LIST an ERG_LIST object
#' @param row_name Row name of record to be dropped as in ERG_LIST$FEATURES
#' @export
#'

dropDulicateRecs<-function(ERG_LIST){
  require(dplyr)
  stim<-ERG_LIST$FEATURES[,c("bg","Intensity","Mode","Freq")]
  dups<-duplicated(stim,fromLast = TRUE) | duplicated(stim,fromLast = FALSE)
  duptypes<-stim[rownames(unique(stim[dups,])),]
  print("Duplicates found for:")
  print(duptypes)
  droprows<-NULL
  for (i in rownames(duptypes)){
    print(paste("Duplicate for", i))
    curr<-semi_join(ERG_LIST$FEATURES,duptypes[i,])
    print(curr)
    keep <- readline(prompt="Select record to keep: ")
    droprows<-c(droprows,rownames(ERG_LIST$FEATURES)[ERG_LIST$FEATURES$Filename %in% curr$Filename[rownames(curr)!=keep]])
  }
  ERG_LIST<-dropRec(ERG_LIST,droprows)
  return(ERG_LIST)
}
