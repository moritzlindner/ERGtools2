#' UpdateMeasurements Method for ERGExam Class
#'
#' This method updates the measurements slot in an 'ERGExam' object with new data.
#'
#' @inheritParams EPhysData::Subset
#' @param values A data.frame containing new measurement values with columns 'Recording', 'Name', 'Voltage', and 'Time'.
#'
#' @return An updated 'ERGExam' object with the measurements replaced or added based on the 'values' data.frame.
#'
#' @details This method is used to update the measurements of an 'ERGExam' object with new data. The method first checks that the 'values' data.frame contains the required columns. It then merges the existing measurements with the new data based on the 'Recording' and 'Name' columns. If a matching entry is found, the 'Voltage' and 'Time' values in the 'ERGExam' object are updated with the corresponding values from 'values'. If no matching entry is found, a new entry is added to the measurements.
#' @name UpdateMeasurements-method
#'
NULL

#' @noMd
setGeneric(
  name = "UpdateMeasurements",
  def = function(X,values) {
    standardGeneric("UpdateMeasurements")
  }
)
#' @rdname UpdateMeasurements-method
#' @exportMethod UpdateMeasurements
setMethod(
  "UpdateMeasurements",
  signature = "ERGExam",
  definition = function(X,values) {
    if (!all(c("Recording", "Name", "Voltage", "Time") %in% colnames(values))) {
      stop("'Values' must be a data.frame with the columns 'Recording', 'Name', 'Voltage', 'Time' ")
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
    Marker.df$Voltage.new<-NULL
    Marker.df$Time.new<-NULL
    X@Measurements<-Marker.df
    X@Measurements.imported<-FALSE
    if (validERGExam(X)) {
      return(X)
    }
  }
)

