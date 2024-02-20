#' #' UpdateMeasurements Method for ERGExam Class
#' #'
#' #' This method updates the measurements slot in an \link[=ERGExam]{ERGExam} object with new data.
#' #'
#' #' @inheritParams Subset-method
#' #' @param Time The time on trace at which the measurement was taken
#' #' @param values FIXME A data.frame containing new measurement values with columns 'Recording', 'Name', 'Voltage', and 'Time'.
#' #' @param ... Additional parameters.
#' #'     These additional parameters provide flexibility for users to specify the Recording the measurement should be assigned to by either giving the index, or parameters defining the recording.
#' #'     - \code{Marker.Name}: A character string specifying the marker name. Mandatory for setting the marker value.
#' #'     - \code{Recording}: The index of the recording where the marker value will be set. If not provided, the parameters \code{Step}, \code{Eye}, \code{Channel}, and \code{Result} must be provided instead.
#' #'     - \code{Step}: The step index. Required if \code{Recording} is not provided.
#' #'     - \code{Eye}: The eye identifier. Required if \code{Recording} is not provided.
#' #'     - \code{Channel}: The channel identifier. Required if \code{Recording} is not provided.
#' #'     - \code{Result}: The result index. Required if \code{Recording} is not provided.
#' #' @param value The value to set. This can be either a numeric value giving the time position of the measurement or a pair list containing 'Voltage', 'Time', and optionally 'Relative' entries.
#' #'
#' #' @return An updated \link[=ERGExam]{ERGExam} object with the measurements replaced or added based on the data.frame provided by the \code{values} attribute.
#' #'
#' #' @details This method is used to update the measurements of an \link[=ERGExam]{ERGExam} object with new data. The method first checks that the 'values' data.frame contains the required columns. It then merges the existing measurements with the new data based on the 'Recording' and 'Name' columns. If a matching entry is found, the 'Voltage' and 'Time' values in the 'ERGExam' object are updated with the corresponding values from 'values'. If no matching entry is found, a new entry is added to the measurements.
#' #' @name UpdateMeasurements
#' #'
#' NULL
#'
#' #' @describeIn UpdateMeasurements Flexibly updates the Measurements slot of an \link[=ERGExam]{ERGExam} object. Replaces marker times and values where already existent and adds them where not.
#' #' @exportMethod UpdateMeasurements
#' setGeneric(
#'   name = "UpdateMeasurements",
#'   def = function(X,values) {
#'     standardGeneric("UpdateMeasurements")
#'   }
#' )
#'
#' #' @noMd
#' setMethod(
#'   "UpdateMeasurements",
#'   signature = "ERGExam",
#'   definition = function(X,values) {
#'     if (!all(c("Recording", "Name", "Voltage", "Time", "Relative") %in% colnames(values))) {
#'       stop("'Values' must be a data.frame with the columns 'Recording', 'Name', 'Voltage', 'Time', 'Relative'.")
#'     }
#'     Marker.df <-
#'       merge(
#'         X@Measurements,
#'         values,
#'         by = c("Recording", "Name"),
#'         all = T,
#'         suffixes = c("", ".new")
#'       )
#'
#'     Marker.df$Voltage[!is.na(Marker.df$Time.new)]<-Marker.df$Voltage.new[!is.na(Marker.df$Time.new)]
#'     Marker.df$Time[!is.na(Marker.df$Time.new)]<-Marker.df$Time.new[!is.na(Marker.df$Time.new)]
#'
#'     if ("Relative.new" %in% colnames(Marker.df)){
#'       Marker.df$Relative[!is.na(Marker.df$Relative.new)]<-Marker.df$Relative.new[!is.na(Marker.df$Relative.new)]
#'     }
#'
#'     Marker.df$Voltage.new<-NULL
#'     Marker.df$Time.new<-NULL
#'     X@Measurements<-Marker.df
#'     X@Measurements.imported<-FALSE
#'     if (validERGExam(X)) {
#'       return(X)
#'     }
#'   }
#' )
#'
#' #' @describeIn UpdateMeasurements Update or add a line to the Measurements slot in an ERGExam object: Either a new time for a given marker can be provided and the corresponding value is extracted automatically from the trace or a combination of time, voltage and information on whether the measurement was performed relative to another marker can be provided.
#' #' @importFrom units deparse_unit set_units as_units
#' #' @importFrom EPhysData GetData
#' setMethod("[<-",
#'           "ERGMeasurement",
#'           function(x, Marker, Recording, value) {
#'             if(length(Marker)==1){
#'               if(!(Marker %in% MarkerNames(x))){
#'                 stop("Marker provided does not exist. Create Marker first.")
#'               }
#'               if(sum(MarkerNames(x) == Marker)>1){
#'                 stop("Marker Name ambigous. Provide Marker and ChannelBinding .")
#'               }
#'             }
#'             if(length(Marker)==0){
#'               stop("No Marker provided")
#'             }
#'             if(length(Marker)==2){
#'               ...
#'             }
#'           })
#' setMethod("[<-",
#'           "ERGExam",
#'           function(x,
#'                    ...,
#'                    value) {
#'             # Check completeness and correctness of indexing params
#'             additional_args <- list(...)
#'             Relative <- ""
#'             if (!"Marker.Name" %in% names(additional_args)) {
#'               stop("A marker name must be provided.")
#'             }
#'             Marker.Name<-additional_args$Marker.Name
#'
#'             if (!is.character(Marker.Name)) {
#'               stop("Marker.name must be a character string.")
#'             }
#'
#'             if (length(Marker.Name) != 1) {
#'               stop("Marker.name must be a single value.")
#'             }
#'
#'             if (!"Recording" %in% names(additional_args)) {
#'               if(!all(c("Step","Eye","Channel") %in% names(additional_args))){
#'                 stop("Either 'Recording' or 'Step','Eye', and 'Channel' must be provided ")
#'               }
#'               Step<-additional_args$Step
#'               Eye<-additional_args$Eye
#'               Channel<-additional_args$Channel
#'               Result<-additional_args$Result
#'               if (length(Step) != 1 ||
#'                   length(Eye) != 1 ||
#'                   length(Channel) != 1 || length(Result) != 1) {
#'                 stop("'Step','Eye', and 'Channel' must have length 1. Or only Recording must be provided")
#'               }
#'               if (!is.character(Eye) || !is.character(Channel)) {
#'                 stop("Eye and Channel must be characters.")
#'               }
#'               if (!is.numeric(Step) || !is.numeric(Result)) {
#'                 stop("Step and Result must be numeric.")
#'               }
#'               Recording <-
#'                 IndexOf(
#'                   x,
#'                   Step = Step,
#'                   Eye = Eye,
#'                   Channel = Channel,
#'                   Result = Result
#'                 )
#'               if(length(Recording)==0){
#'                 stop("No valid recording could be identified for the given parameters")
#'               }
#'             } else {
#'               Recording<-additional_args$Recording
#'               if (!is.numeric(Recording)) {
#'                 stop("Recording must be numeric.")
#'               }
#'               if (nrow(Metadata(x))<Recording){
#'                 stop("'Recording' out of range.")
#'               }
#'             }
#'             # Check completeness and correctness of value, fetch voltage if not provided
#'             if (!(is.pairlist(value) || is.numeric(value))) {
#'               stop(
#'                 "'value' must be a single numeric giving the time position of the measurement or pair list. See function help for details on the requirements for a pair list."
#'               )
#'             } else {
#'               if (is.numeric(value)) {
#'                 # value is a numeric
#'                 if (length(value) == 1) {
#'                   Time <- value
#'                   if (!("units" %in% class(Time))) {
#'                     Time <-
#'                       as_units(Time, deparse_unit(TimeTrace(x[[1]])), mode = "standard")
#'                   }
#'                   if (nrow(Metadata(x))<Recording){
#'                     stop("'Recording' out of range.")
#'                   }
#'                   sel <- x[[Recording]]
#'                   Voltage <- GetData(sel, Time = Time, Raw = F)
#'                   if (length(Voltage) > 1) {
#'                     stop(
#'                       "'AverageFunction' does not return a single value per time point. Is Averaging function set correctly? Try AverageFunction(x)<-mean"
#'                     )
#'                   }
#'                 } else {
#'                   stop ("'value' is not of length 1")
#'                 }
#'               } else {
#'                 # value is a pair list
#'                 if (!(all(names(value) %in% c("Voltage", "Time", "Relative")))) {
#'                   stop(
#'                     "If 'value' is a pair list it must have at least the two entries: 'Time' and 'Relative' and optionally a third entry 'Voltage'. 'Voltage' and 'Time' must be numeric, 'Relative' a character vector"
#'                   )
#'                 } else {
#'                   # pair list in correct format
#'                   if ("Time" %in% names(value)) {
#'                     if (is.numeric(value$Time) && length(value$Time) == 1) {
#'                       Time <- Time
#'                       if (!("units" %in% class(Time))) {
#'                         Time <-
#'                           as_units(Time, deparse_unit(TimeTrace(x[[1]])), mode = "standard")
#'                       }
#'                     } else {
#'                       stop ("'Time' entry in 'value' is not of a numeric of length 1")
#'                     }
#'                   } else {
#'                     stop ("No entry for Time provided in 'value'")
#'                   }
#'                   if ("Relative" %in% names(value)) {
#'                     if (is.character(value$Relative) &&
#'                         length(value$Relative) == 1)
#'                     {
#'                       Relative <- value$Relative
#'                     } else {
#'                       stop ("'Relative' entry in 'value' is not of a character vector of length 1")
#'                     }
#'                   }
#'                   if ("Voltage" %in% names(value)) {
#'                     if (is.numeric(value$Time) && length(value$Time))
#'                     {
#'                       Voltage <- value$Voltage
#'                       if (!("units" %in% class(Voltage))) {
#'                         Voltage <-
#'                           as_units(Voltage, deparse_unit(x[[1]]@Data), mode = "standard")
#'                       }
#'
#'                     } else {
#'                       stop ("'Voltage' entry in 'value' is not of a numeric of length 1")
#'                     }
#'                   } else {
#'                     # get voltage from trace if not provided
#'                     sel <- x[[Recording]]
#'                     Voltage <- GetData(sel, Time = Time, Raw = F)
#'                     if (length(Voltage) > 1) {
#'                       stop(
#'                         "'AverageFunction' does not return a single value per time point. Is Averaging function set correctly? Try AverageFunction(x)<-mean"
#'                       )
#'                     }
#'                     # if Relative, substract the value from the reference marker
#'                     if (Relative!="") {
#'                       if (nrow(x[Recording = Recording, Marker.Name = Relative]$Voltage) > 0) {
#'                         Voltage <- Voltage - x[Recording = Recording, Marker.Name = Relative]$Voltage
#'                       } else {
#'                         stop("Reference marker for relative measurement not found.")
#'                       }
#'                     }
#'                   }
#'                 }
#'               }
#'             }
#'
#'             # Check if entry already exists, then update; if not, add to table
#'             replace.idx <-
#'               which(x@Measurements$Recording == Recording &
#'                       x@Measurements$Name == Marker.Name)
#'             if (is.null(replace.idx)) {
#'               new_measurement <-
#'                 data.frame(
#'                   Recording = Recording,
#'                   Name = Marker.Name,
#'                   Voltage = Voltage,
#'                   Time = Time,
#'                   Relative = Relative
#'                 )
#'               x@Measurements <-
#'                 rbind(x@Measurements, new_measurement)
#'             } else{
#'               x@Measurements[x@Measurements$Recording == Recording &
#'                                x@Measurements$Name == Marker.Name, "Voltage"] <-
#'                 Voltage
#'               x@Measurements[x@Measurements$Recording == Recording &
#'                                x@Measurements$Name == Marker.Name, "Time"] <-
#'                 Time
#'               x@Measurements[x@Measurements$Recording == Recording &
#'                                x@Measurements$Name == Marker.Name, c("Relative")] <-
#'                 Relative
#'             }
#'
#'             return(x)
#'           })
#'
