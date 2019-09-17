#' drops Recordings from ERG_LIST objects
#'
#' This function imports ERG recordings from abf files in a given folder into one list-type element
#' @param ERG_LIST an ERG_LIST object
#' @param row_name Row name of record to be dropped as in ERG_LIST$FEATURES
#' @export
#'
dropRec<-function(ERG_LIST,row_name){
  ERG_LIST$FILES<-ERG_LIST$FILES[!(rownames(ERG_LIST$FEATURES) %in% row_name)]
  ERG_LIST$TRACES<-ERG_LIST$TRACES[!(rownames(ERG_LIST$FEATURES) %in% row_name)]
  ERG_LIST$FEATURES<-ERG_LIST$FEATURES[!(rownames(ERG_LIST$FEATURES) %in% row_name),]
  return(ERG_LIST)
}
