#' drops Recordings from ERG_LIST objects
#'
#' This function allows for manual adjustment of Flash ERG measured as stored in an ERG_List object. Measures are stored in data frames in the MEASURES slot and overwrite automacical measurements.
#' @param ERG_LIST an ERG_LIST object
#' @param TMAXBWAVE Maximum time of B-Wave Peak
#' @import ggmap
#' @export
#'

manualMeasures<-function(ERG_LIST){
  require(ggmap)
  modify <- readline(prompt="A and B wave measurements OK (Y,n)?")
  if (modify=="n"){
    print(data.frame(ERG_LIST$FEATURES$Stimulus))
    tomodify <- as.numeric(unlist(strsplit(readline(prompt="select traces with measurements to modify. Use commas as separator."), ",", fixed=FALSE)))
    for (i in tomodify){
      isOK="n"
      while (isOK=="n"){
        print("Mark A and B wave peaks")
        print(plot_ERG(ERG_LIST$AVG[[i]],ERG_LIST$FEATURES[i,]$Stimulus,ERG_LIST$FEATURES[i,]$SampleInterval,ERG_LIST$MEASURES[[i]]))
        pnts<-gglocator(n = 2, type = "n")
        names(pnts)[names(pnts)!="Mean"]<-"Pos"
        names(pnts)[names(pnts)=="Mean"]<-"Val"
        pnts$Pos<-pnts$Pos/ERG_LIST$FEATURES$SampleInterval[i]
        print(pnts)

        for (j in 1:2){
          dif<-pnts
          dif$Pos<-as.numeric(ERG_LIST$MEASURES[[i]]$Pos)-pnts[j,]$Pos
          dif$Val<-as.numeric(ERG_LIST$MEASURES[[i]]$Val)-pnts[j,]$Val
          dif$dist<-sqrt(abs(apply(dif, 1, prod)))
          ERG_LIST$MEASURES[[i]][which.min(dif$dist),]$Pos<-as.character(round(pnts[j,]$Pos))
          ERG_LIST$MEASURES[[i]][which.min(dif$dist),]$Val<-as.character(pnts[j,]$Val)
        }
        print(plot_ERG(ERG_LIST$AVG[[i]],ERG_LIST$FEATURES[i,]$Stimulus,ERG_LIST$FEATURES[i,]$SampleInterval,ERG_LIST$MEASURES[[i]]))
        isOK <- readline(prompt="A and B wave measurements OK (Y,n)?")
      }
    }

    graph <- vector('list', length(ERG_LIST$AVG[[i]]))
    for (i in names(ERG_LIST$AVG)){
      if (!is.null(ERG_LIST$AVG[i])){
        graph[[i]]<-plot_ERG(ERG_LIST$AVG[[i]],ERG_LIST$FEATURES[i,]$Stimulus,ERG_LIST$FEATURES[i,]$SampleInterval,ERG_LIST$MEASURES[[i]])
      }
    }
    ERG_LIST$graphs<-graph
  }
  return(ERG_LIST)
}
