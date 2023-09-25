#' as.data.frame for ERGProtocol, ERGStep, ERGChannel and ERGMarker
#'
#' Converts an \link{ERGProtocol}, ERGStep, ERGChannel or ERGMarker object to a data frame format.
#' Note that the \code{as.data.frame} method for \link{ERGExam} objects is inherited from the \link{EPhysData::EPhysData}(EPhysData::EPhysData) package. See: \link{EPhysData::as.data.frame}(EPhysData::as.data.frame)
#'
#' @param x An \link{ERGProtocol}, ERGStep, ERGChannel or ERGMarker object.
#' @param ... currently unused.
#'
#' @return A data frame representing the \link{ERGProtocol}, ERGStep, ERGChannel or ERGMarker object in long format.
#'
#' @examples
#' # Create an example EPhysData object
#' new_data <- makeExampleEPhysData()
#' # Convert EPhysData to a data frame
#' new_data <- as.data.frame(new_data)
#' new_data
#'
#' # Create an example EPhysSet object
#' new_data <- makeExampleEPhysSet()
#' new_data <- as.data.frame(new_data)
#'
#' @importFrom tidyr pivot_longer starts_with
#' @importFrom units drop_units as_units
#' @name as.data.frame
NULL

#' @describeIn as.data.frame Method for ERGMarker
#' @exportMethod as.data.frame
setMethod("as.data.frame",
          "ERGMarker",
          function(x,
                   ...){
            out <-
              data.frame(Marker.Name = character(), Marker.Relative.to = character())
            out[1,]<-c(x@Name, x@RelativeTo)
            return(out)
          }
)

#' @describeIn as.data.frame Method for ERGChannel
#' @exportMethod as.data.frame
setMethod("as.data.frame",
          "ERGChannel",
          function(x,
                   ...){

            markers<-lapply(x@Markers,as.data.frame)
            markers<-do.call(rbind,markers)
            out <-
              data.frame(
                Channel.Name = character(),
                Eye = character(),
                LowFreqCutoff = numeric(),
                HighFreqCutoff = numeric(),
                Channel.Inverted = logical(),
                Channel.Enabled = logical()
              )
            nMarkers<-nrow(markers)
            if(length(nMarkers)<1){
              nMarkers<-1
              marker<-ERGtools2:::ERGMarker()
              marker@Name<-""
              markers<-as.data.frame(marker)
            }
            out <- out[1:nMarkers, ]
            out$Channel.Name[1:nMarkers] <- as.character(x@Name)
            out$Eye[1:nMarkers]  <- x@Eye
            out$LowFreqCutoff[1:nMarkers] <- x@LowFreqCutoff
            out$HighFreqCutoff[1:nMarkers] <- x@HighFreqCutoff
            out$Channel.Inverted[1:nMarkers] <- x@Inverted
            out$Channel.Enabled[1:nMarkers] <- x@Enabled

            return(cbind(out,markers))
          }
)

#' @describeIn as.data.frame Method for ERGStep
#' @exportMethod as.data.frame
setMethod("as.data.frame",
          "ERGStep",
          function(x,
                   ...){
            channels<-lapply(x@Channels,as.data.frame)
            channels<-do.call(rbind,channels)
            nChannels<- nrow(channels)

            # Create a data frame for ERGStep slots and repeat values based on nMarkers
            step_df <- data.frame(
              Description = rep(as.character(x@Description), each = nChannels),
              Adaptation = rep(as.character(x@Adaptation), each = nChannels),
              Resultsperrun = rep(as.numeric(x@Resultsperrun), each = nChannels),
              Timebetweenresults = rep(as.character(x@Timebetweenresults), each = nChannels),
              SampleFrequency = rep(as.character(x@SampleFrequency), each = nChannels),
              PreTriggerTime = rep(as.character(x@PreTriggerTime), each = nChannels),
              PostTriggerTime = rep(as.character(x@PostTriggerTime), each = nChannels),
              InterSweepDelay = rep(as.character(x@InterSweepDelay), each = nChannels),
              Sweepsperresult = rep(as.numeric(x@Sweepsperresult), each = nChannels),
              Sweepsperresultmin = rep(as.numeric(x@Sweepsperresultmin), each = nChannels),
              Glitchremoval = rep(as.logical(x@Glitchremoval), each = nChannels),
              Driftremoval = rep(as.logical(x@Driftremoval), each = nChannels),
              Baselineenabled = rep(as.logical(x@Baselineenabled), each = nChannels),
              Baselinepretrigger = rep(as.character(x@Baselinepretrigger), each = nChannels),
              Baselinerange = rep(as.character(x@Baselinerange), each = nChannels)
            )

            # Combine the ERGStep data frame with the channel data frame
            result_df <- cbind(step_df, channels)
            return(result_df)
          }
)
#' @describeIn as.data.frame Method for ERGProtocol
#' @exportMethod as.data.frame
as.data.frame<-setMethod(
  "as.data.frame",
  signature = "ERGProtocol",
  function(x, ...) {

    steps<-lapply(x@Step,as.data.frame)
    steps<-do.call(rbind,steps)
    nSteps<- nrow(steps)

    # Create a data frame for ERGProtocol slots and repeat values based on nChannels
    protocol_df <- data.frame(
      ProtocolName = rep(as.character(x@Name), each = nSteps),
      Export_Date = rep(as.character(x@Export_Date), each = nSteps),
      row.names = 1:nSteps
    )

    # Combine the ERGProtocol data frame with the step data frame
    result_df <- cbind(protocol_df, steps)

    return(result_df)
  }
)
