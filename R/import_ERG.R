#' Imports ERG recordings from abf files
#'
#' This function imports ERG recordings from abf files in a given folder into one list-type element
#' @param SRC Source directory. Will show file open dialog if left empty
#' @param downsample target sampling rate for downsampling. If set to FALSE, no downsampling will be performed.
#' @param keywords data.frame assigning keywords, as they can be encountered in comments to stimulus features. The data.frame should have the columns data.frame with the columns "key","feature","value","default", where "key" is a keyword found in the comment of the abf file and "feature" and "value" contain the standardized data pair (e.g. "intensity" "0.01") that should be added for the recording if "key" is encounterd. Column "default" states what should be added if "key" is not encountered. Set to "USER" for prompts.
#' @import readABF
#' @import abf2
#' @export
#'

import_ERG<-function(SRC = NULL,downsample=FALSE, keywords){ # working
  require(readABF)
  require(abf2)
  #list files from folder
  if (is.null(SRC)){
    SRC<-choose.dir()
  }
  FILES<-list.files(SRC, pattern = ".abf",full.names = TRUE)
  if (length(FILES)==0){
    stop(paste("No files found in",SRC))
  }
  DATA<-list()
  DATA$TRACES
  N<-1
  curr<-NULL
  DATA$FEATURES<-as.data.frame(matrix(,length(FILES),7))
  colnames(DATA$FEATURES)<-c("Stimulus","bg","Intensity","Mode","Freq","SampleInterval","Filename")

  for (I in FILES){
    # load comments
    curr<-abfload(I)$strings[3]
    print(curr)
    # analyze comments or ask for user input
    if (keywords!="USER"){
      features<-strsplit(curr, " ")[[1]]
      features<-as.data.frame(keywords[keywords$key %in% features ,])
      # translate comment to recording features
      bg<-features$value[features$feature=="bg"]
      if(length(bg)==0){
        bg<-unique(keywords$default[keywords$feature=="bg"])
      }
      intensity<-features$value[features$feature=="intensity"]
      if(length(intensity)==0){
        intensity<-unique(keywords$default[keywords$feature=="intensity"])
      }
      modus<-features$value[features$feature=="mode"]
      if(length(modus)==0){
        modus<-unique(keywords$default[keywords$feature=="mode"])
      }
      freq<-features$value[features$feature=="freq"]
      if(length(freq)==0){
        freq<-unique(keywords$default[keywords$feature=="freq"])
      }
    }else{
      bg <- readline(prompt="Enter background illumination (DA/LA): ")
      intensity <- readline(prompt="Enter intensity: ")
      modus <- readline(prompt="Enter type of recording (Flash/Flicker): ")
      freq <- readline(prompt="Enter flicker frequency (if applicable): ")
    }

    continue<-FALSE
    continue<-((modus=="Flash" & bg!="NULL" & intensity!="NULL") || (modus=="Flicker" & freq!="NULL"))

    # ask user to input missing values
    if (continue==FALSE){
      warning(paste("Some information on stimulus properties cant be read for", I," ",curr),immediate. = TRUE, call. = FALSE)
      if(length(modus)==0){
        modus <- readline(prompt="Enter type of recording (Flash/Flicker): ")
      }
      if(modus=="Flash"){
        if(length(bg)==0){
          bg <- readline(prompt="Enter background illumination (DA/LA): ")
        }
        if(intensity=="NULL"){
          intensity <- readline(prompt="Enter intensity: ")
        }
      }else{
        if(freq=="NULL"){
          freq <- readline(prompt="Enter flicker frequency: ")
        }
      }
    }
    continue<-((modus=="Flash" & bg!="NULL" & intensity!="NULL") || (modus=="Flicker" & freq!="NULL"))
    # now import, only take those where minimum set of featres is defined
    if (continue!=FALSE){
      DATA$FEATURES$Stimulus[N]<-curr #COMMENT PLACE where Comment is stored

      #set stim features
      DATA$FEATURES$Mode[N]<-as.character(modus)
      DATA$FEATURES$bg[N]<-as.character(bg)
      DATA$FEATURES$Intensity[N]<-as.numeric(as.character(intensity))
      DATA$FEATURES$Freq[N]<-as.numeric(as.character(freq))
      DATA$FEATURES$Filename[N]<-as.character(I)

      # now import data
      CURRENTABF<-readABF(I)
      DATA$FILES[N]<-I
      signal<-as.data.frame(matrix(,length(unlist(list(list(CURRENTABF$data[[1]][,CURRENTABF$channelNames=="CyberAmp"])))),length(CURRENTABF$data)))
      for (J in 1:length(CURRENTABF$data)){
        #DATA$TRACES[N]
        signal[J]<-unlist(list(list(CURRENTABF$data[[J]][,CURRENTABF$channelNames=="CyberAmp"]))) #V Place where voltage trace from CyberAmp is stored
      }

      # downsample
      if (is.numeric(downsample)){
        binwidth<-1/((1/CURRENTABF$samplingIntervalInSec)/downsample)
        idx<-1:dim(signal)[1]
        signal$idx<-as.factor(round(idx/(1/binwidth)))
        signal<-aggregate(signal[,1:length(colnames(signal))-1], list(signal$idx), median)
        CURRENTABF$samplingIntervalInSec<-1/downsample
        try(signal<-signal[, -which(names(signal) %in% c("Group.1"))],silent = TRUE) # delete unwanted column created by aggregate
      }

      DATA$TRACES[[N]]=signal
      DATA$FEATURES$SampleInterval[N]<-as.numeric(CURRENTABF$samplingIntervalInSec)

    }else{
      warning(paste("WARNING: Stimulus", I,"not imported: missing features."),immediate. = TRUE, call. = FALSE)
    }
    N<-N+1
  }
  names(DATA$TRACES)<-rownames(DATA$FEATURES)
  return(DATA)
}
