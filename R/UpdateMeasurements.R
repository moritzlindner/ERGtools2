#' UpdateMeasurements Method for ERGExam Class
#'
#' This method updates the measurements slot in an \link[=ERGExam]{ERGExam} object with new data.
#'
#' @inheritParams Subset-method
#' @param values A data.frame containing new measurement values with columns 'Recording', 'Name', 'Voltage', and 'Time'.
#'
#' @return An updated \link[=ERGExam]{ERGExam} object with the measurements replaced or added based on the data.frame provided by the \code{values} attribute.
#'
#' @details This method is used to update the measurements of an \link[=ERGExam]{ERGExam} object with new data. The method first checks that the 'values' data.frame contains the required columns. It then merges the existing measurements with the new data based on the 'Recording' and 'Name' columns. If a matching entry is found, the 'Voltage' and 'Time' values in the 'ERGExam' object are updated with the corresponding values from 'values'. If no matching entry is found, a new entry is added to the measurements.
#' @name UpdateMeasurements
#'
NULL

#' @describeIn UpdateMeasurements Flexibly updates the Measurements slot of an \link[=ERGExam]{ERGExam} object. Replaces marker times and values where already existent and adds them where not.
#' @exportMethod UpdateMeasurements
setGeneric(
  name = "UpdateMeasurements",
  def = function(X,values) {
    standardGeneric("UpdateMeasurements")
  }
)

#' @noMd
setMethod(
  "UpdateMeasurements",
  signature = "ERGExam",
  definition = function(X,values) {
    if (!all(c("Recording", "Name", "Voltage", "Time", "Relative") %in% colnames(values))) {
      stop("'Values' must be a data.frame with the columns 'Recording', 'Name', 'Voltage', 'Time', 'Relative'.")
    }
    Marker.df <-
      merge(
        X@Measurements,
        values,
        by = c("Recording", "Name"),
        all = T,
        suffixes = c("", ".new")
      )

    Marker.df$Voltage[!is.na(Marker.df$Time.new)]<-Marker.df$Voltage.new[!is.na(Marker.df$Time.new)]
    Marker.df$Time[!is.na(Marker.df$Time.new)]<-Marker.df$Time.new[!is.na(Marker.df$Time.new)]

    if ("Relative.new" %in% colnames(Marker.df)){
      Marker.df$Relative[!is.na(Marker.df$Relative.new)]<-Marker.df$Relative.new[!is.na(Marker.df$Relative.new)]
    }

    Marker.df$Voltage.new<-NULL
    Marker.df$Time.new<-NULL
    X@Measurements<-Marker.df
    X@Measurements.imported<-FALSE
    if (validERGExam(X)) {
      return(X)
    }
  }
)

#' @describeIn UpdateMeasurements Update parts of the Measurements slot in an ERGExam object: Sets a new time for a given marker, the corresponding value is extracted automatically from the trace.
#' @importFrom units deparse_unit set_units
setMethod("[<-",
          "ERGExam",
          function(x, Marker.Name, Step, Eye, Channel, Result, Time) {
            Md <- Metadata(x)
            which <-
              which(Md$Step == Step &
                 Md$Eye == Eye &
                 Md$Channel == Channel &
                 Md$Result == 1)

            if(length(which)!=1){
              stop("Exactly one Marker must be selected by providing a unique combination of 'Marker.Name', 'Step', 'Eye', 'Channel' and 'Result.'")
            }

            Time<-as_units(Time, deparse_unit(x@Measurements$Time), mode = "standard")

            x@Measurements$Time[x@Measurements$Recording==which & x@Measurements$Name==Marker.Name]<-Time

            sel<-x[[which]]
            value <- GetData(sel, Time = Time, Raw = F)
            if(length(value)>1){
              stop("'AverageFunction' does not return a single value per time point. Is Averaging function set correctly? Try AverageFunction(x)<-mean")
            }
            x@Measurements$Voltage[x@Measurements$Recording==which & x@Measurements$Name==Marker.Name]<-value
          })
