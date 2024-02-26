#' #' Modify the Measurements slot of an ERGExam or ERGMeasurements object
#' #'
#' #' These methods enable modification of the Measurements slot of an \linkS4class{ERGExam} object or the underlying \linkS4class{ERGMeasurements} object directly.
#' #' \strong{Note:} the \code{Measurements(X, ...) <- value} methods require that all arguments are provided. Set to NULL if not needed.
#' #'
#' #' @param X An \linkS4class{ERGExam} or  \linkS4class{ERGMeasurements} object.
#' #' @param Marker Name of the marker.
#' #' @param ChannelBinding For AddMarker and DropMarker only. Channel to which the marker belongs. Usually defined by the Parent \linkS4class{ERGExam} object. Set to NA if not required.
#' #' @param Relative For AddMarker and Measurements<- only. Index of the marker the new marker is relative to.
#' #' @param Recording Numeric value specifying the recording index.
#' #' @param Step,Eye,Channel,Result For Measurements and Measurements<- only. Vector of values for Steps, Eyes, Channels, and Result to select.
#' #' @param value For Measurements<- only. Plain numeric value or value of class units (\link[units:units]{units::units} with a time unit set. If \code{NULL} it will remove the indicated row.
#' #' @param data For newERGMeasurements only. A data frame containing measurements data with columns:
#' #'   \code{Channel}, \code{Name}, \code{Recording}, \code{Time}, and \code{Relative}.
#' #' @param drop.dependent For DropMarker and DropMeasurement only. Logical. If TRUE, dependent markers will also be dropped. If FALSE, fails if there are markes dependent on that removed.
#' #' @param create.marker.if.missing Logical. If TRUE and the marker does not exist, it will be created.
#' #' @param quiet For Measurements only. Logical. If TRUE, suppresses warnings.
#' #' @return An updated version of the object, except for Measurements, which returns a data.frame representing the Measurements stored in the Measurements slot of the \linkS4class{ERGExam} object or the \linkS4class{ERGMeasurements} object-
#' #' @seealso \link[EPhysData:EphysSet-class]{EPhysData::ERGMeasurements-class} \link{Measurements-Methods} \link{Get}
#' #' @examples
#' #' # Load example data
#' #' data(ERG)
#' #' data(Measurements)
#' #' stop("Further examples required")
#' #'
#' #' @name Measurements-Methods
#' NULL
#'
#' #' Add a new Marker
#' #' @describeIn Measurements-Methods Add a new Marker
#' #' @examples
#' #' # Add a new Marker to an ERGMeasurements object
#' #' new_erg_measurements <- AddMarker(Measurements, Marker = "Marker1", Relative = NA, ChannelBinding = "ChannelA")
#' #' new_erg_measurements
#' #' Markers(new_erg_measurements)
#' #'
#' #' # Add a new Marker to an ERGMeasurements object
#' #' new_erg <- AddMarker(ERG, Marker = "Marker1", Relative = NA, ChannelBinding = "ERG_auto")
#' #' # but this would fail:
#' #' # new_erg <- AddMarker(ERG, Marker = "Marker1", Relative = NA, ChannelBinding = "ChannelA")
#' #' new_erg
#' #' new_erg<-SetStandardFunctions(new_erg) # to set an averaging function
#' #' Markers(new_erg)
#' #' @exportMethod AddMarker
#' setGeneric(
#'   name = "AddMarker",
#'   def = function(X,
#'                  Marker,
#'                  Relative,
#'                  ChannelBinding) {
#'     standardGeneric("AddMarker")
#'   }
#' )
#'
#' #' @noMd
#' setMethod("AddMarker",
#'           "ERGMeasurements",
#'           function(X, Marker, Relative, ChannelBinding) {
#'             # Check if the new marker is valid
#'             existing_markers <- unique(X@Marker$Name[X@Marker$ChannelBinding==ChannelBinding])
#'             if(length(existing_markers)>0){
#'               if (Marker %in% existing_markers) {
#'                 stop("Marker name already exists in the Marker slot")
#'               }
#'             }
#'             if(is.null(Relative)){
#'               stop("'Relative' must be set, either to a valid marker name or to NA, if the new marker is not a relative marker." )
#'             }
#'             if(is.null(ChannelBinding)){
#'               stop("'ChannelBinding' should be set to any valid channel of the parent object. However, can be set to NA, if not required." )
#'             }
#'
#'             if (!is.na(Relative) &&
#'                 !(Relative %in% 1:length(MarkerNames(X)))) {
#'               stop("'Relative' must point to an existing marker (by index) in the Marker slot")
#'             }
#'             if (!is.na(Relative)) {
#'               if(!is.numeric(Relative))
#'               stop("'Relative' must be a numeric pointing to an existing marker in the Marker slot")
#'             }
#'             new_row <-
#'               data.frame(Name = Marker,
#'                          Relative = Relative,
#'                          ChannelBinding = ChannelBinding)
#'
#'
#'             # Add the new row to the Marker slot
#'             X@Marker <- rbind(X@Marker, new_row)
#'
#'             # Return the updated object
#'             if(validObject(X)){
#'               return(X)
#'             }
#'           })
#' #' @noMd
#' setMethod("AddMarker", "ERGExam",
#'           function(X, Marker, Relative, ChannelBinding) {
#'             if (!(ChannelBinding %in% Channels(X))){
#'               stop("'ChannelBinding' must describe a valid channel contained in X.")
#'             }
#'             X@Measurements <- AddMarker(X@Measurements, Marker, Relative, ChannelBinding)
#'             validObject(X)
#'             return(X)
#'           })
#'
#' #' Drop marker by name and channel
#' #'
#' #' @describeIn Measurements-Methods Drop marker by name and channel
#' #' @examples
#' #' # Drop a Marker from an ERGMeasurements object
#' #' data(Measurements.data)
#' #' Measurements.data
#' #' Markers(Measurements.data)
#' #'
#' #' new_erg_measurements <- DropMarker(Measurements.data, Marker = "N1", ChannelBinding = "VEP", drop.dependent = TRUE)
#' #' new_erg_measurements
#' #' Markers(new_erg_measurements)
#' #'
#' #' @exportMethod DropMarker
#' setGeneric(
#'   name = "DropMarker",
#'   def = function(X, Marker, ChannelBinding, drop.dependent = FALSE) {
#'     standardGeneric("DropMarker")
#'   }
#' )
#'
#' #' @noMd
#' setMethod("DropMarker",
#'           "ERGMeasurements",
#'           function(X, Marker, ChannelBinding = NULL, drop.dependent = FALSE) {
#'             if (length(Marker) != 1 || !is.character(Marker)) {
#'               stop("Multiple marker names provided in 'Marker' argument.")
#'             }
#'
#'             # Check if the marker exists in the Marker slot
#'             if( is.null(ChannelBinding)){
#'               marker_index <-
#'                 which(X@Marker$Name == Marker)
#'             } else {
#'               marker_index <-
#'                 which(X@Marker$Name == Marker & X@Marker$ChannelBinding == ChannelBinding)
#'             }
#'
#'             if (length(marker_index) == 0) {
#'               warning("Marker not found in the Marker slot")
#'               return(X)
#'             }
#'             if (length(marker_index) > 1) {
#'               stop("Multiple markers found for the Name given by 'Marker'.")
#'             }
#'
#'             # Find dependent markers recursively
#'             findDependentMarkers <-
#'               function(marker_index, dependent) {
#'                 dependent <-
#'                   which(X@Marker$Relative %in% marker_index  & X@Marker$ChannelBinding %in% X@Marker$ChannelBinding[marker_index])
#'                 if (length(dependent) == 0) {
#'                   return(marker_index)
#'                 } else {
#'                   c(marker_index,
#'                     findDependentMarkers(dependent, dependent))
#'                 }
#'               }
#'
#'             dependent <- findDependentMarkers(marker_index, NULL)
#'             if (!drop.dependent
#'                 && length(dependent) > 1) { # because dependent includes the index marker
#'               stop(
#'                 "Other markers are relative to the marker that you are trying to delete. Consider using drop.dependent = TRUE."
#'               )
#'             }
#'
#'             # Create a vector to store indices to remove
#'             indices_to_remove <-
#'               sort(unique(c(marker_index, dependent)), decreasing = T)
#'             X@Marker <- X@Marker[-indices_to_remove,]
#'             rownames(X@Marker)<-NULL
#'
#'             # Remove the corresponding rows from the Measurements slot
#'             measurements_to_remove <-
#'               which(X@Measurements$Marker %in% indices_to_remove)
#'             if(length(measurements_to_remove)>0){
#'               X@Measurements <- X@Measurements[-measurements_to_remove, ]
#'             }
#'             rownames(X@Measurements)<-NULL
#'
#'             # Move the pointers in relative accordingly
#'             for (i in indices_to_remove) {
#'               X@Marker$Relative[which(X@Marker$Relative > i)] <-
#'                 X@Marker$Relative[which(X@Marker$Relative > i)] - 1
#'               X@Measurements$Marker[which(X@Measurements$Marker > i)] <-
#'                 X@Measurements$Marker[which(X@Measurements$Marker > i)] - 1
#'             }
#'
#'             if(!validObject(X)){
#'               stop("Dropping markers failed for unknown reason.")
#'             }
#'             # Return the updated object
#'             return(X)
#'           })
#'
#' #' @noMd
#' setMethod("DropMarker",
#'           "ERGExam",
#'           function(X, Marker, ChannelBinding = Channels(X), drop.dependent = FALSE) {
#'             X@Measurements <-
#'               DropMarker(X@Measurements , Marker, ChannelBinding, drop.dependent)
#'             if(validObject(X)){
#'               return(X)
#'             } else {
#'               stop("Object validation failed after dropping Markers")
#'             }
#'           })
#'
#' #' @describeIn Measurements-Methods Get Markers.
#' #' @exportMethod Markers
#' #' @noMd
#' setGeneric(
#'   name = "Markers",
#'   def = function(X)
#'   {
#'     standardGeneric("Markers")
#'   }
#' )
#' #' @noMd
#' setMethod("Markers",
#'           "ERGMeasurements",
#'           function(X) {
#'             df<-X@Marker
#'             df$Relative<-df$Name[df$Relative]
#'             return(df)
#'           })
#' #' @noMd
#' setMethod("Markers",
#'           "ERGExam",
#'           function(X) {
#'             return(Markers(X@Measurements))
#'           })
#'
#' #' Get Measurements table
#' #'
#' #' @describeIn Measurements-Methods Get Measurements table
#' #' @examples
#' #' # Get Measurements table for specific recording and marker in ERGMeasurements object
#' #' Markers(Measurements)
#' #' Measurements(X = Measurements, Recording = 1, Marker = "a", quiet = TRUE)
#' #' Measurements(X = Measurements, Recording = 1, Marker = "X", quiet = F)
#' #'
#' #'
#' #' # Get Measurements table for specific recording and marker in ERGExam object
#' #' ERG<-SetStandardFunctions(ERG)
#' #' Measurements(ERG)
#' #' @exportMethod Measurements
#' setGeneric(
#'   name = "Measurements",
#'   def = function(X,
#'                  ...)
#'   {
#'     standardGeneric("Measurements")
#'   }
#' )
#'
#' #' @describeIn Measurements-Methods Get Measurements table from an  \linkS4class{ERGMeasurements} object
#' #' @noMd
#' setMethod("Measurements", "ERGMeasurements",
#'           function(X,
#'                    Recording = NULL,
#'                    Marker =  NULL,
#'                    quiet = F) {
#'             # integrity checks
#'             if (is.null(Recording)) {
#'               Recording <- unique(X@Measurements$Recording)
#'             }
#'             if (is.null(Marker)) {
#'               Marker <-  MarkerNames(X)
#'             }
#'
#'
#'             if (!is.character(Marker)) {
#'               stop("'Marker' must be a character string.")
#'             }
#'             emptyobject <- !nrow(X@Marker) > 0
#'             if (!emptyobject) {
#'               if (!all(Marker %in% MarkerNames(X)) && !quiet) {
#'                 warning("'Marker' is not a valid marker name of a marker contained in the object.")
#'               }
#'             }
#'
#'             # Merge Marker and Measurements data frames
#'             mark <- X@Marker
#'             mark$Relative <- mark$Name[mark$Relative]
#'
#'             merged_df <-
#'               merge(
#'                 mark,
#'                 X@Measurements,
#'                 by.x = 0,
#'                 by.y = "Marker",
#'                 all.x = TRUE,
#'                 all.y = TRUE
#'               )
#'             merged_df <-
#'               merged_df[c("Recording", "Name", "ChannelBinding", "Relative", "Time")]
#'
#'             merged_df <-
#'               merged_df[with(merged_df,
#'                              order(Recording, ChannelBinding, Relative, na.last =
#'                                      F)),]
#'             rownames(merged_df) <- NULL
#'
#'             if (!emptyobject) {
#'               if(length(Recording)!=0){
#'                 if (!all(Recording %in% merged_df$Recording) && !quiet) {
#'                   warning("'Recording' is not a valid Index of a Measurement already contained in the object.")
#'                 }
#'               }
#'             }
#'
#'             return(merged_df[merged_df$Recording %in% Recording &
#'                                merged_df$Name %in% Marker,])
#'           })
#'
#'
#' #' @describeIn Measurements-Methods Get Measurements table from an \linkS4class{ERGExam} object
#' #' @importFrom utils txtProgressBar setTxtProgressBar
#' #' @importFrom EPhysData Metadata AverageFunction
#' #' @noMd
#' setMethod("Measurements",
#'           "ERGExam",
#'           function(X,
#'                    Recording = NULL,
#'                    Step = Steps(X),
#'                    Eye = Eyes(X),
#'                    Channel = Channels(X),
#'                    Result = Results(X),
#'                    Marker =  NULL,
#'                    quiet = F) {
#'
#'             if (any(Step != Steps(X),
#'                     Eye != Eyes(X),
#'                     Channel != Channels(X),
#'                     Result != Results(X))) {
#'               if (!is.null(Recording)) {
#'                 stop(
#'                   "If 'Recording' is provided, none of 'Step', 'Channel', 'Result' or 'Eye' may be provided as well."
#'                 )
#'               }
#'             } else{
#'               if (is.null(Recording)) {
#'                 Recording <- IndexOf(X, Step, Eye, Channel, Result)
#'               }
#'             }
#'
#'             measurements <- Measurements(X@Measurements, Recording, Marker, quiet)
#'             if (nrow(measurements) != 0) {
#'               extracols<-ExtraMetaColumns(X)
#'               measurements <-
#'                 merge(measurements,
#'                       Metadata(X)[, c("Step", "Channel", "Result",  "Eye")],
#'                       by =
#'                         "Recording",
#'                       by.y = 0)
#'               measurements <-
#'                 merge(measurements, StimulusTable(X), by = "Step")
#'               measurements <-
#'                 measurements[, c(
#'                   "Recording",
#'                   "Step",
#'                   "Description",
#'                   "Channel",
#'                   "Result",
#'                   "Eye",
#'                   "Name",
#'                   "Relative",
#'                   "Time"
#'                 )]
#'
#'               measurements <-
#'                 measurements[with(measurements,
#'                                   order(Recording, Step, Channel, Eye, Relative, na.last =
#'                                           F)), ]
#'               measurements$Voltage <- as_units(NA, "uV")
#'
#'               message("Retrieving record values for the given time points.")
#'               pb = txtProgressBar(min = 0, max = nrow(measurements), initial = 0)
#'               for (i in 1:nrow(measurements)) {
#'                 sel <- X[[measurements$Recording[i]]]
#'                 if (length(AverageFunction(sel)(1:3))!=1 && dim(sel)[2]!=1){
#'                   print(AverageFunction(sel))
#'                   print(dim(sel))
#'                   stop(
#'                     "You need to set an averaging function before performing Marker and Measuremnt operations. E.g. run 'SetStandardFunctions(X)'"
#'                   )
#'                 }
#'                 Voltage <-
#'                   GetData(sel, Time = TimeTrace(sel)[which.min(abs(TimeTrace(sel)- measurements$Time[i]))], Raw = F)
#'                 if(!is.na(measurements$Relative[i])){
#'                   rel.marker.time<-measurements$Time[measurements$Recording == measurements$Recording[i] &
#'                                                        measurements$Name == measurements$Relative[i]]
#'                   Voltage <- Voltage - GetData(sel,
#'                                                Time = TimeTrace(sel)[which.min(abs(TimeTrace(sel)- rel.marker.time))],
#'                                                Raw = F)
#'                 }
#'                 if (all(dim(Voltage) == 1)) {
#'                   measurements$Voltage[i] <- set_units(Voltage[1, 1], "uV")
#'                 } else{
#'                   stop("GetData returns multiple values.")
#'                 }
#'                 setTxtProgressBar(pb,i)
#'               }
#'               close(pb)
#'               measurements<-merge(measurements,Metadata(X)[,c(extracols),drop=F],by.x = "Recording", by.y = 0)
#'
#'
#'             } else {
#'               measurements <-
#'                 data.frame(
#'                   Recording = numeric(),
#'                   Step = character(),
#'                   Description = character(),
#'                   Channel = character(),
#'                   Result = numeric(),
#'                   Eye = character(),
#'                   Name = character(),
#'                   Relative = character(),
#'                   Time = as_units(numeric(), "ms"),
#'                   Voltage = as_units(numeric(), "uV")
#'                 )
#'             }
#'             return(measurements)
#'           })
#'
#' #' Update or add a line to the Measurements slot
#' #'
#' #' @describeIn Measurements-Methods Update, add, or remove a line to the Measurements slot
#' #' @examples
#' #' # Set Measurements for specific recording and marker in ERGMeasurements object
#' #' Markers(Measurements)
#' #' Measurements(X = Measurements, Marker = "B" ,Recording = 1)<-99
#' #' Measurements(X = Measurements, Marker = "c" ,Recording = 1,Relative = "a")<-150
#' #'
#' #'
#' #' # Set Measurements table for specific recording and marker in ERGExam object
#' #' ERG<-SetStandardFunctions(ERG)
#' #' Measurements(ERG)
#' #'
#' #'
#' #' @exportMethod Measurements<-
#' setGeneric(
#'   name = "Measurements<-",
#'   def = function(X,
#'                  ...,
#'                  value)
#'   {
#'     standardGeneric("Measurements<-")
#'   }
#' )
#'
#' #' @describeIn Measurements-Methods Update, add, or remove a measurement from an \linkS4class{ERGMeasurements} object
#' #' @noMd
#' #' @importFrom units deparse_unit set_units as_units units_options
#' setMethod("Measurements<-",
#'           signature = "ERGMeasurements",
#'           function(X,
#'                    Marker,
#'                    Recording,
#'                    create.marker.if.missing = T,
#'                    Relative = NULL,
#'                    ChannelBinding = NULL,
#'                    value) {            #validity checks and marker Marker conversion
#'
#'             # internal functions
#'             get.marker.idx<-function(cb,X){
#'               if(!is.null(ChannelBinding)){
#'                 marker.idx <- which(Markers(X)$Name == Marker & Markers(X)$ChannelBinding == ChannelBinding)
#'               } else {
#'                 marker.idx <- which(Markers(X)$Name == Marker)
#'               }
#'               if(length(marker.idx)!=1){
#'                 stop("Marker is not unabiguously defined by the given paramaters ('Marker' and possibly 'ChannelBinding').")
#'               }
#'               return(marker.idx)
#'             }
#'
#'             #validity checks
#'             if (any(length(Marker) != 1, length(Recording) != 1, length(value) != 1)) {
#'               stop ("'Marker', 'Recording', and 'value' must contain a single value.")
#'             }
#'             if (!is.character(Marker)) {
#'               stop("'Marker' must be a character string.")
#'             }
#'             if (!is.numeric(Recording)) {
#'               stop("'Recording' must be numeric.")
#'             }
#'             if(!is.null(value)){
#'               # if the aim is not to delete a measurement (value!=NULL)
#'               if (!is.numeric(value)) {
#'                 stop("'value' must be numeric.")
#'               }else{
#'                 refunit<-X@Measurements$Time[1]
#'                 refunit<-deparse_unit(refunit)
#'                 if (!("units" %in% class(value))) {
#'                   value <-
#'                     as_units(value, refunit, mode = "standard")
#'                 } else{
#'                   if(deparse_unit(value)!=refunit){
#'                     units_options(set_units_mode = "standard")
#'                     value <- set_units(value, refunit)
#'                   }
#'                 }
#'               }
#'
#'               nrow.same<-nrow(Measurements(X, Recording = Recording, Marker = Marker,quiet = T))
#'
#'               if (nrow.same ==
#'                   1) {    # Update Measurement
#'                 marker.idx<-get.marker.idx(ChannelBinding,X)
#'
#'                 if (length(X@Measurements$Time[X@Measurements$Recording == Recording &
#'                                                X@Measurements$Marker %in% marker.idx]) != 1) {
#'                   stop("Unexpected error during marker selection.")
#'                 }
#'                 X@Measurements$Time[X@Measurements$Recording == Recording &
#'                                       X@Measurements$Marker %in% marker.idx] <- value
#'               }
#'               if (nrow.same ==
#'                   0) {   # add Measurement
#'                 if (!(Marker %in% MarkerNames(X))) {
#'                   if (create.marker.if.missing) {
#'                     X <-
#'                       AddMarker(
#'                         X,
#'                         Marker = Marker,
#'                         Relative = Relative,
#'                         ChannelBinding = ChannelBinding
#'                       )
#'                   } else{
#'                     stop("Marker does not exist in object.")
#'                   }
#'                 }
#'
#'                 marker.idx<-get.marker.idx(ChannelBinding,X)
#'                 new.measurement <- data.frame(Recording = Recording,
#'                                               Marker = marker.idx,
#'                                               Time = value)
#'                 X@Measurements <-
#'                   rbind(X@Measurements, new.measurement)
#'
#'               }
#'               if (nrow(Measurements(X, Recording = Recording, Marker = Marker)) ==
#'                   2) {
#'                 stop(
#'                   "Malformed ERGMeasurements object. Multiple entries found for the given combination of 'Recording' and 'Marker'"
#'                 )
#'               }
#'             }else{
#'               # if value is NULL --> delete the selected measurement
#'               marker.idx<-get.marker.idx(ChannelBinding,X)
#'               X@Measurements <-
#'                 X@Measurements[X@Measurements$Recording == Recording &
#'                                  X@Measurements$Marker %in% marker.idx,]
#'               rownames(X@Measurements)<-NULL
#'
#'             }
#'
#'             if (validObject(X)) {
#'               return(X)
#'             }else{
#'               stop("object validation failed")
#'             }
#'           })
#'
#' #' @describeIn Measurements-Methods Update, add, or remove a measurement from the Measurements slot of an  \linkS4class{ERGExam} object
#' #' @noMd
#' #' @importFrom units deparse_unit set_units as_units
#' setMethod("Measurements<-",
#'           signature = "ERGExam",
#'           function(X,
#'                    Marker,
#'                    Recording = NULL,
#'                    Step = NULL,
#'                    Eye = NULL,
#'                    Channel = NULL,
#'                    Result = NULL,
#'                    create.marker.if.missing = T,
#'                    Relative = NULL,
#'                    ChannelBinding = NULL,
#'                    value){
#'
#'
#'             if (any(!is.null(Step),
#'                     !is.null(Eye),
#'                     !is.null(Channel),
#'                     !is.null(Result))) {
#'               if (!is.null(Recording)) {
#'                 stop("If 'Recording' is provided, none of 'Step', 'Channel', 'Result' or 'Eye' may be provided as well.")
#'               }
#'               Recording<-IndexOf(X,Step,Eye,Channel,Result)
#'             }
#'
#'             Measurements(X@Measurements,
#'                          Marker = Marker,
#'                          Recording = Recording,
#'                          create.marker.if.missing = create.marker.if.missing,
#'                          Relative = Relative,
#'                          ChannelBinding = ChannelBinding
#'                          ) <- value
#'
#'             if (validObject(X)) {
#'               return(X)
#'             }else{
#'               stop("Object validation failed for unknown reason.")
#'             }
#'
#'           })
#'
#' #' Create an ERGMeasurements object from data
#' #'
#' #' @describeIn Measurements-Methods #' Create an ERGMeasurements object from data
#' #' @examples
#' #' # Example data frame
#' #' data <- data.frame(
#' #'   Channel = c("ERG", "ERG", "ERG", "ERG"),
#' #'   Name = c("Marker1", "Marker2", "Marker1", "Marker2"),
#' #'   Recording = c(1, 1, 2, 2),
#' #'   Time = c(10, 12, 15, 18),
#' #'   Relative = c(NA, "Marker1", NA, "Marker1")
#' #' )
#' #' # Create ERGMeasurements object
#' #' newERGMeasurements(data)
#' #'
#' #' @export newERGMeasurements
#' newERGMeasurements <- function(data) {
#'
#'   if (!is.data.frame(data)) {
#'     stop("'data' must be a data frame.")
#'   }
#'   required_columns <- c("Channel", "Name", "Recording", "Time", "Relative")
#'   if (!all(required_columns %in% names(data))) {
#'     stop("Data frame must contain all required columns: ", paste(required_columns, collapse = ", "))
#'   }
#'
#'   M <- new("ERGMeasurements")
#'   # first add independent markers
#'   for (c in unique(data$Channel[is.na(data$Relative)])) {
#'     for (m in unique(data$Name[data$Channel == c &
#'                                        is.na(data$Relative)])) {
#'       rel <- NA
#'       # data[data$Channel == c &
#'       #        data$Name == m, ]
#'       M <- AddMarker(M, m, rel, c)
#'       for (r in data$Recording[data$Channel == c &
#'                                data$Name == m]) {
#'         suppressWarnings({
#'           Measurements(
#'             X = M,
#'             Marker = m,
#'             Recording = r,
#'             Relative = rel,
#'             create.marker.if.missing = F,
#'             ChannelBinding = c
#'           ) <-
#'             data$Time[data$Channel == c &
#'                         data$Name == m &
#'                         data$Recording == r]
#'         })
#'         }
#'     }
#'   }
#'   #then dependent markers
#'   for (c in unique(data$Channel[!is.na(data$Relative)])) {
#'     for (m in unique(data$Name[data$Channel == c &
#'                                        !is.na(data$Relative)])) {
#'       print(m)
#'       rel <-
#'         unique(data$Relative[data$Channel == c &
#'                                data$Name == m])
#'       rel.idx<-which(Markers(M)==rel)
#'       M <- AddMarker(M, m, rel.idx, c)
#'       for (r in data$Recording[data$Channel == c &
#'                                        data$Name == m]) {
#'         Measurements(
#'           M,
#'           Marker = m,
#'           Recording = r,
#'           create.marker.if.missing = F,
#'           ChannelBinding = c
#'         ) <-
#'           data$Time[data$Channel == c &
#'                               data$Name == m &
#'                               data$Recording == r]
#'       }
#'     }
#'   }
#'   if (validObject(M)) {
#'     return(M)
#'   }
#' }
#'
#' #' @describeIn Measurements-Methods Clears the Measurements slots in an  link[=ERGExam]{ERGExam} object (both, markers and measurements.
#' #' @importFrom methods validObject
#' #' @exportMethod ClearMeasurements
#' setGeneric(
#'   name = "ClearMeasurements",
#'   def = function(X) {
#'     standardGeneric("ClearMeasurements")
#'   }
#' )
#' #' @noMd
#' setMethod("ClearMeasurements",
#'           signature(X = "ERGExam"),
#'           function(X) {
#'             X@Measurements <- new("ERGMeasurements")
#'             if (validObject(X)) {
#'               return(X)
#'             }
#'           })
#'
#'
#' #' @describeIn Measurements-Methods Converts the output of the Measurements method from relative to absolute amplitudes
#' #' @keywords Internal
#' #' @noMd
#' ConvertMeasurementsToAbsolute <- function(data) {
#'   for (i in seq_along(data$Voltage)) {
#'     if (!is.na(data$Relative[i])) {
#'       match_row <- which(data$Description == data$Description[i] &
#'                            data$Eye == data$Eye[i] &
#'                            data$Channel == data$Channel[i] &
#'                            data$Name == data$Relative[i])
#'
#'       if (length(match_row) > 0) {
#'         data$Voltage[i] <- data$Voltage[i] + data$Voltage[match_row]
#'       }
#'     }
#'   }
#'   #colnames(data)[colnames(data)=="Relative"]<-".Relative"
#'   return(data)
#' }
