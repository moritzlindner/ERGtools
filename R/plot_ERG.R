#' Plots ERGs
#'
#' TBC
#' @param curr The current ERG trace to plot
#' @param title The tilte for the plot (eg. recording conditions)
#' @param samp The sampling rate of curr
#' @param marks Any markers to be shown, e.g a-wave and b-wave positions
#' @export
#'
plot_ERG<-function(curr,title,samp,marks=NULL){
  require("ggplot2")
  require("ggplot2")
  PLT<-ggplot(curr,aes(y=Mean,x=(1:dim(curr)[1])*(samp),ymin=P25,ymax=P75))+
    geom_line()+
    geom_ribbon(alpha=0.3)+
    theme_minimal()+
    theme(axis.title = element_blank(),plot.title = element_text(size=12, face="bold"),legend.position = "none")+
    ylim(-0.00060,0.00060)+
    ggtitle(title)
  if(!is.null(marks)){
    PLT<-PLT+geom_point(inherit.aes = FALSE,data=marks,aes(x=as.numeric(Pos)*samp,y=as.numeric(Val),colour=as.factor(Type)))
  }
  return(PLT)
}
