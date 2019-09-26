#' drops Recordings from ERG_LIST objects
#'
#' This function measures Flash and flicker ERG recordings as stored in an ERG_List object. Measures are stored in data frames in the MEASURES slot. For Flicker stimuli, Averaces from all measures is tored in FLICKERSTATS.
#' @param ERG_LIST an ERG_LIST object
#' @param TMAXBWAVE Maximum time (in seconds) after flash stimulus to B-Wave peak
#' @export
#'

getMeasures<-function(ERG_LIST,TMAXBWAVE){
  for (i in names(ERG_LIST$AVG)){
    res<-data.frame(Val=numeric(),Pos=numeric(),Type=character(), stringsAsFactors = FALSE)
    flickerstats<-data.frame(Val=numeric(),Pos=numeric(),Type=character(), stringsAsFactors = FALSE)
    if(ERG_LIST$FEATURES[i,]$Mode=="Flash"){
      from<-(1/ERG_LIST$FEATURES[i,]$SampleInterval)*ERG_LIST$TFLASH
      to<-1/ERG_LIST$FEATURES[i,]$SampleInterval*TMAXBWAVE
      res[1,]<-c(min(ERG_LIST$AVG[[i]]$Mean[from:to]),which.min(ERG_LIST$AVG[[i]]$Mean[from:to])+from,"Min")
      res[2,]<-c(max(ERG_LIST$AVG[[i]]$Mean[from:to]),which.max(ERG_LIST$AVG[[i]]$Mean[from:to])+from,"Max")

    }
    if(ERG_LIST$FEATURES[i,]$Mode=="Flicker"){
      intervals<-seq(from=1,to=length(ERG_LIST$AVG[[i]]$Mean),by=round((1/ERG_LIST$FEATURES[i,]$SampleInterval)/ERG_LIST$FEATURES[i,]$Freq))
      k=1
      stat.min.y<-NULL
      stat.min.x<-NULL
      stat.max.y<-NULL
      stat.max.x<-NULL
      for (j in 1:(length(intervals)-1)){
        res[k,]<-c(min(ERG_LIST$AVG[[i]]$Mean[intervals[j]:intervals[j+1]]),which.min(ERG_LIST$AVG[[i]]$Mean[intervals[j]:intervals[j+1]])+intervals[j],"Min")
        stat.min.y[j]<-min(ERG_LIST$AVG[[i]]$Mean[intervals[j]:intervals[j+1]])
        stat.min.x[j]<-which.min(ERG_LIST$AVG[[i]]$Mean[intervals[j]:intervals[j+1]])
        k=k+1
        res[k,]<-c(max(ERG_LIST$AVG[[i]]$Mean[intervals[j]:intervals[j+1]]),which.max(ERG_LIST$AVG[[i]]$Mean[intervals[j]:intervals[j+1]])+intervals[j],"Max")
        stat.max.y[j]<-max(ERG_LIST$AVG[[i]]$Mean[intervals[j]:intervals[j+1]])
        stat.max.x[j]<-which.max(ERG_LIST$AVG[[i]]$Mean[intervals[j]:intervals[j+1]])
        k=k+1
      }
      stat.min.y<-mean(stat.min.y)
      stat.min.x<-mean(stat.min.x)
      stat.max.y<-mean(stat.max.y)
      stat.max.x<-mean(stat.max.x)
      flickerstats[1,]<-c(stat.min.y,stat.min.x,"Min")
      flickerstats[2,]<-c(stat.max.y,stat.max.x,"Max")
      ERG_LIST$FLICKERSTATS[[i]]<-flickerstats
    }
    ERG_LIST$MEASURES[[i]]<-res
  }


  graph <- vector('list', length(ERG_LIST$AVG[[i]]))
  for (i in names(ERG_LIST$AVG)){
    if (!is.null(ERG_LIST$AVG[i])){
      graph[[i]]<-plot_ERG(ERG_LIST$AVG[[i]],ERG_LIST$FEATURES[i,]$Stimulus,ERG_LIST$FEATURES[i,]$SampleInterval,ERG_LIST$MEASURES[[i]])
      }
  }
  ERG_LIST$graphs<-graph

  return(ERG_LIST)
}
