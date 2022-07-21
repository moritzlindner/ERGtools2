#' Get measurements data from an ERGExam object
#'
#' `r lifecycle::badge("stable")` \cr
#' This function subsets \linkS4class{ERGExam} objects by \var{Channels}, \var{Stimuli}, \var{Repeats}, measurement \var{Markers} or \var{Measures}.
#' @param Channels,Stimuli,Markers List of Channels/Stimuli or measurement parameters (e.g. a-wave) to get. By default will get all.
#' @param Repeats Measurement repeats to get. Can be "ALL", to get all repeats, a numeric vector of length >=1 containing the repeats to keep, or a function of the form \code{function(x){x==max(x)}} indicating which repeats to keep.
#' @param Measures Measures to get. A character vector. Can be \code{"Voltage"}, \code{"Time"} or both (default).
#' @param Verbose Show warning/info messages.
#' @return An \linkS4class{ERGExam} object.
#' @examples
#' \dontrun{
#' # import a *.csv files exported from Diagnosys' Epsion
#' ERG_Experiment<-ImportEpsionMeasures("test.csv")
#' # get all measurements with the latest (highest number) repeat
#' df<-GetMeasurements(ERG_Experiment,Repeats = function(x){x==max(x)},verbose=T)
#' }
#' @name GetMeasurements
#' @exportMethod GetMeasurements
setGeneric(
  name = "GetMeasurements",
  def = function(X,
                 Channels = GetChannelNames(X),
                 Stimuli = GetStimulusNames(X),
                 Repeats = "ALL",
                 Markers = GetMarkerNames(X),
                 Measures = c("Voltage","Time"),
                 Verbose=T)
  {
    standardGeneric("GetMeasurements")
  }
)

setMethod("GetMeasurements",
          "ERGExam",
          function(X,
                   Channels = GetChannelNames(X),
                   Stimuli = GetStimulusNames(X),
                   Repeats = "ALL",
                   Markers = GetMarkerNames(X),
                   Measures = c("Voltage","Time"),
                   Verbose=T)
          {
            # generate function for selecting Repeats

            if(is.function(Repeats)){
              Repeats<-Repeats
            }else{
              if(is.numeric(Repeats)){
                Repeats<-function(x){
                  x %in% Repeats
                }
              }
              if(Repeats == "ALL"){
                Repeats<-function(x){
                  x %in% unique(X@Measurements$Repeat)
                }
              }
            }

            #display warnings/info
            if (Verbose) {
              if (isFALSE(all.equal(Channels, GetChannelNames(X)))) {
                message("Keep Channels:", Channels, "\n")
              }
              if (isFALSE(all.equal(Stimuli, GetStimulusNames(X)))) {
                message("Keep Stimuli:", Stimuli, "\n")
              }
              if (isFALSE(all.equal(Markers, GetMarkerNames(X)))) {
                message("Keep Markers:", Markers, "\n")
              }
              if (isFALSE(all.equal(Measures, c("Voltage","Time")))) {
                message("Keep Measures:", Measures, "\n")
              }
              if(is.function(Repeats)){
                message("Keep Repeats by formula: ", deparse(Repeats), "\n")
              }else{
                message("Keep Repeats: ", deparse(Repeats), "\n")
              }
            }

            # translate into indices, if not already
            if (is.character(Channels)){
              Channels<-which(X@Channels %in% Channels)
            }
            if (is.character(Stimuli)){
              Stimuli<-which(X@Stimulus$Description %in% Stimuli)
            }

            # update Measurements slot
            OUT<-X@Measurements

            OUT<-OUT[((OUT$Step %in% Stimuli) &
                             (OUT$Channel %in% Channels) &
                             (OUT$Param %in% Markers))
                           ,]
            OUT<-OUT[with(OUT, ave(Repeat, Step,Channel,Param, FUN = function(x) {Repeats(x)})!=0),]

            keepcols<-c(colnames(X@Measurements)[!(colnames(X@Measurements) %in% c("Voltage","Time"))],Measures)
            OUT<-OUT[,keepcols]

            # update factors
            OUT$Step<-as.factor(OUT$Step)
            levels(OUT$Step)<-X@Stimulus$Description[as.numeric(levels(OUT$Step))]

            OUT$Channel<-as.factor(OUT$Channel)
            levels(OUT$Channel)<-X@Channels[as.numeric(levels(OUT$Channel))]

            # add further info to DF

            Const<-matrix(factor(),nrow=dim(OUT)[1],ncol=3)
            colnames(Const)<-c("Patient","Group","ExamDate")
            Const<-as.data.frame(Const)
            Const$Patient<-as.factor(X@Patient)
            if(length(X@Group)>0){
              Const$Group<-as.factor(X@Group)
            }else{
              Const$Group<-NA
            }
            Const$ExamDate<-as.POSIXct(X@ExamDate)

            OUT<-cbind(Const,OUT)
            return(OUT)
          })


