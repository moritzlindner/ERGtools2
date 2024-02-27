#' Access and Modify the Markers Stored in an ERGExam or ERGMeasurements Object
#'
#' These methods enable accession and  modification of the Markers stored in an  \linkS4class{ERGExam} object or the underlying \linkS4class{ERGMeasurements} object directly.
#'
#' @param X An \linkS4class{ERGExam} or \linkS4class{ERGMeasurements} object.
#' @param Marker Name of the marker.
#' @param ChannelBinding For AddMarker and DropMarker only. Channel to which the marker belongs. Usually defined by the Parent \linkS4class{ERGExam} object. Set to NA if not required.
#' @param Relative For AddMarker and Measurements<- only. Index of the marker the new marker is relative to.
#' @param drop.dependent For DropMarker and DropMeasurement only. Logical. If TRUE, dependent markers will also be dropped. If FALSE, fails if there are markes dependent on that removed.
#' @return An updated version of the object, except for Markers, which returns a data.frame representing the Markers stored in the the \linkS4class{ERGExam} or the \linkS4class{ERGMeasurements} object.
#' @seealso \link[EPhysData:EPhysSet-class]{EPhysData::EPhysSet-class} \link{Measurements-Methods} \link{Get}
#' @examples
#' # Add a new Marker to an ERGMeasurements object#'
#' data(Measurements.data)
#' new_erg_measurements <- AddMarker(Measurements.data, Marker = "Marker1", Relative = NA, ChannelBinding = "ChannelA")
#' new_erg_measurements
#' Markers(new_erg_measurements)
#'
#' # Add a new Marker to an ERGMeasurements object
#' data(ERG)
#' new_erg <- AddMarker(ERG, Marker = "Marker1", Relative = NA, ChannelBinding = "ERG_auto")
#' # but this would fail:
#' # new_erg <- DropMarker(ERG, Marker = "Marker1")
#' Markers(new_erg)
#'
#' # Drop a Marker from an ERGMeasurements object
#' data(Measurements.data)
#' Measurements.data
#' Markers(Measurements.data)
#'
#' new_erg_measurements <- DropMarker(Measurements.data, Marker = "N1", ChannelBinding = "VEP", drop.dependent = TRUE)
#' new_erg_measurements
#' Markers(new_erg_measurements)
#'
#' @name Marker-Methods
NULL

#' Add a new Marker
#' @describeIn Marker-Methods Add a new Marker

#' @exportMethod AddMarker
setGeneric(
  name = "AddMarker",
  def = function(X,
                 Marker,
                 Relative,
                 ChannelBinding) {
    standardGeneric("AddMarker")
  }
)

#' @noMd
setMethod("AddMarker",
          "ERGMeasurements",
          function(X, Marker, Relative, ChannelBinding) {
            # Check if the new marker is valid
            existing_markers <- unique(X@Marker$Name[X@Marker$ChannelBinding==ChannelBinding])
            if(length(existing_markers)>0){
              if (Marker %in% existing_markers) {
                stop("Marker name already exists in the Marker slot")
              }
            }
            if(is.null(Relative)){
              stop("'Relative' must be set, either to a valid marker name or to NA, if the new marker is not a relative marker." )
            }
            if(is.null(ChannelBinding)){
              stop("'ChannelBinding' should be set to any valid channel of the parent object. However, can be set to NA, if not required." )
            }

            if (!is.na(Relative) &&
                !(Relative %in% 1:length(MarkerNames(X)))) {
              stop("'Relative' must point to an existing marker (by index) in the Marker slot")
            }
            if (!is.na(Relative)) {
              if(!is.numeric(Relative))
                stop("'Relative' must be a numeric pointing to an existing marker in the Marker slot")
            }
            new_row <-
              data.frame(Name = Marker,
                         Relative = Relative,
                         ChannelBinding = ChannelBinding)


            # Add the new row to the Marker slot
            X@Marker <- rbind(X@Marker, new_row)

            # Return the updated object
            if(validObject(X)){
              return(X)
            }
          })
#' @noMd
setMethod("AddMarker", "ERGExam",
          function(X, Marker, Relative, ChannelBinding) {
            if (!(ChannelBinding %in% Channels(X))){
              stop("'ChannelBinding' must describe a valid channel contained in X.")
            }
            X@Measurements <- AddMarker(X@Measurements, Marker, Relative, ChannelBinding)
            validObject(X)
            return(X)
          })

#' Drop marker by name and channel
#'
#' @describeIn Marker-Methods Drop marker by name and channel
#' @exportMethod DropMarker
setGeneric(
  name = "DropMarker",
  def = function(X, Marker, ChannelBinding, drop.dependent = FALSE) {
    standardGeneric("DropMarker")
  }
)

#' @noMd
setMethod("DropMarker",
          "ERGMeasurements",
          function(X, Marker, ChannelBinding = NULL, drop.dependent = FALSE) {
            if (length(Marker) != 1 || !is.character(Marker)) {
              stop("Multiple marker names provided in 'Marker' argument.")
            }

            # Check if the marker exists in the Marker slot
            if( is.null(ChannelBinding)){
              marker_index <-
                which(X@Marker$Name == Marker)
            } else {
              marker_index <-
                which(X@Marker$Name == Marker & X@Marker$ChannelBinding == ChannelBinding)
            }

            if (length(marker_index) == 0) {
              warning("Marker not found in the Marker slot")
              return(X)
            }
            if (length(marker_index) > 1) {
              stop("Multiple markers found for the Name given by 'Marker'.")
            }

            # Find dependent markers recursively
            findDependentMarkers <-
              function(marker_index, dependent) {
                dependent <-
                  which(X@Marker$Relative %in% marker_index  & X@Marker$ChannelBinding %in% X@Marker$ChannelBinding[marker_index])
                if (length(dependent) == 0) {
                  return(marker_index)
                } else {
                  c(marker_index,
                    findDependentMarkers(dependent, dependent))
                }
              }

            dependent <- findDependentMarkers(marker_index, NULL)
            if (!drop.dependent
                && length(dependent) > 1) { # because dependent includes the index marker
              stop(
                "Other markers are relative to the marker that you are trying to delete. Consider using drop.dependent = TRUE."
              )
            }

            # Create a vector to store indices to remove
            indices_to_remove <-
              sort(unique(c(marker_index, dependent)), decreasing = T)
            X@Marker <- X@Marker[-indices_to_remove,]
            rownames(X@Marker)<-NULL

            # Remove the corresponding rows from the Measurements slot
            measurements_to_remove <-
              which(X@Measurements$Marker %in% indices_to_remove)
            if(length(measurements_to_remove)>0){
              X@Measurements <- X@Measurements[-measurements_to_remove, ]
            }
            rownames(X@Measurements)<-NULL

            # Move the pointers in relative accordingly
            for (i in indices_to_remove) {
              X@Marker$Relative[which(X@Marker$Relative > i)] <-
                X@Marker$Relative[which(X@Marker$Relative > i)] - 1
              X@Measurements$Marker[which(X@Measurements$Marker > i)] <-
                X@Measurements$Marker[which(X@Measurements$Marker > i)] - 1
            }

            if(!validObject(X)){
              stop("Dropping markers failed for unknown reason.")
            }
            # Return the updated object
            return(X)
          })

#' @noMd
setMethod("DropMarker",
          "ERGExam",
          function(X, Marker, ChannelBinding = Channels(X), drop.dependent = FALSE) {
            X@Measurements <-
              DropMarker(X@Measurements , Marker, ChannelBinding, drop.dependent)
            if(validObject(X)){
              return(X)
            } else {
              stop("Object validation failed after dropping Markers")
            }
          })

#' @describeIn Marker-Methods Get Markers.
#' @exportMethod Markers
#' @noMd
setGeneric(
  name = "Markers",
  def = function(X)
  {
    standardGeneric("Markers")
  }
)
#' @noMd
setMethod("Markers",
          "ERGMeasurements",
          function(X) {
            df<-X@Marker
            df$Relative<-df$Name[df$Relative]
            return(df)
          })
#' @noMd
setMethod("Markers",
          "ERGExam",
          function(X) {
            return(Markers(X@Measurements))
          })
