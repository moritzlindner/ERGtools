#' Compilres ERG recordings from multiple mice
#'
#' This function imports ERG recordings from files in given folder and compiles them into the variables TRACES, STATS and STIMTYPES of a SUMMARY object
#' @param RECS a list of files containing ERG recordins
#' @export
#'


compileFromFolder<-function(RECS,Flicker="Flicker"){
  TRACES<-data.frame(Exp=character(),Stim=character(),t=numeric(),val=numeric())
  STATS<-data.frame(Exp=character(),Stim=character(),Val=numeric(),Pos=numeric(),Type=character())
  STIMTYPES<-data.frame(Stimulus=character(),bg=character(),Intensity=numeric(),Mode=character(),Freq=numeric(),SampleInterval=numeric(),Filename=character())
  for (i in recs$recs){
    load(as.character(i))
    STIMTYPES<-rbind(STIMTYPES,ERG_LIST$FEATURES)
    for (j in rownames(ERG_LIST$FEATURES)){
      TRACES<-rbind(TRACES,data.frame(rep(i,length(ERG_LIST$AVG[[j]]$Mean)),rep(ERG_LIST$FEATURES[j,]$Stimulus,length(ERG_LIST$AVG[[j]]$Mean)),seq(1:length(ERG_LIST$AVG[[j]]$Mean))*ERG_LIST$FEATURES[j,]$SampleInterval,as.numeric(ERG_LIST$AVG[[j]]$Mean)))
      if(str_count(ERG_LIST$FEATURES[j,]$Stimulus,Flicker)==0){
        STATS<-rbind(STATS,data.frame("Exp" = rep(i,length(ERG_LIST$MEASURES[[j]]$Val)),"Stim" = rep(ERG_LIST$FEATURES[j,]$Stimulus,length(ERG_LIST$MEASURES[[j]]$Val)),ERG_LIST$MEASURES[[j]]))
      }else{
        STATS<-rbind(STATS,data.frame("Exp" = rep(i,length(ERG_LIST$FLICKERSTATS[[j]]$Val)),"Stim" = rep(ERG_LIST$FEATURES[j,]$Stimulus,length(ERG_LIST$FLICKERSTATS[[j]]$Val)),ERG_LIST$FLICKERSTATS[[j]]))
      }
    }

    #rm("ERG_LIST","Report")
  }
  colnames(TRACES)<-c("Exp","Stim","t","val")
  colnames(STATS)<-c("Exp","Stim","Val","Pos","Type")
  STATS$Val<-as.numeric(STATS$Val)
  STATS$Pos<-as.numeric(STATS$Pos)
  STIMTYPES<-unique(STIMTYPES[,-which(names(STIMTYPES) == "Filename")])

  SUMMARY<-list()
  SUMMARY$STIMTYPES<-STIMTYPES
  SUMMARY$STATS<-STATS
  SUMMARY$TRACES<-TRACES
  return(SUMMARY)
}
