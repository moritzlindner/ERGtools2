#' Set processing functions for ERGExam objects
#'
#' These methods are used to set filter, rejection and averaging functions for \link{ERGExam} objects.
#' They allow setting the functions to a subset of recordings based on the specified conditions.
#' @inheritParams EPhysData::Get_Set_EPhysData
#' @param X An \linkS4class{ERGExam} object
#' @inheritParams Where
#' @param Stimulus.type.names A \link[base:pairlist]{base::pairlist} specifying the names identifying the different stimulus types, e.g., \code{Flash="Flash"} or \code{Flash="Blitz"}.
#' @return An updated ERGExan object.
#'
#' @examples
#' data(ERG)
#' ERG<-SetStandardFunctions(ERG)
#' ggERGTrace(ERG,where=list(Intensity=1,Channel="ERG",Eye="RE"))
#'
#' AverageFunction(ERG,where=list(Intensity=1))<-min
#' ggERGTrace(ERG,where=list(Intensity=1,Channel="ERG",Eye="RE"))
#'
#'
#' @seealso \link[EPhysData:Get_Set_EPhysData]{EPhysData::Get_Set_EPhysData}, \link[EPhysMethods:autoreject.by.distance]{EPhysMethods::autoreject.by.distance}, \link[EPhysMethods:autoreject.by.signalfree]{EPhysMethods::autoreject.by.signalfree}, \link[EPhysMethods:filter.bandpass]{EPhysMethods::filter.bandpass}, \link[EPhysMethods:filter.detrend]{EPhysMethods::filter.detrend},
#' @name UpdateProcessingMethods
NULL

#' @describeIn UpdateProcessingMethods Update the FilterFunction for all Recordings in an \linkS4class{ERGExam}, or only those slected using \code{where}.
#' @exportMethod FilterFunction<-
setMethod("FilterFunction<-", signature = "ERGExam", function(X, where, value) {
  return(functionupdater(X, where, value, "FilterFunction<-"))
})

#' @describeIn UpdateProcessingMethods Update the Rejected for all Recordings in an \linkS4class{ERGExam}, or only those slected using \code{where}.
#' @exportMethod Rejected<-
setMethod("Rejected<-", signature = "ERGExam", function(X, where, value) {
  return(functionupdater(X, where, value, "Rejected<-"))
})

#' @describeIn UpdateProcessingMethods Update the AverageFunction for all Recordings in an \linkS4class{ERGExam}, or only those slected using \code{where}.
#' @exportMethod AverageFunction<-
setMethod("AverageFunction<-", signature = "ERGExam", function(X, where, value) {
  return(functionupdater(X, where, value, "AverageFunction<-"))
})

#' @describeIn UpdateProcessingMethods This method is used to set standard functions for processing \linkS4class{ERGExam} data. It defines default functions for averaging, filtering, and signal rejection based on the stimulus type.
#' @importFrom EPhysMethods filter.detrend autoreject.by.signalfree autoreject.by.distance
#' @exportMethod SetStandardFunctions
setGeneric(
  name = "SetStandardFunctions",
  def = function(X,
                 Stimulus.type.names = pairlist(Flash = "Flash",
                                           Flicker = "Flicker")) {
    standardGeneric("SetStandardFunctions")
  }
)

#' @importFrom EPhysMethods filter.lin.detrend autoreject.by.signalfree autoreject.by.distance
#' @noMd
setMethod(
  "SetStandardFunctions",
  signature = "ERGExam",
  definition = function(X,
                        Stimulus.type.names = pairlist(Flash = "Flash",
                                                  Flicker = "Flicker")) {
    Md <- merge(Metadata(X), StimulusTable(X))
    for (i in 1:nrow(Md)) {
      AverageFunction(X@Data[[i]]) <- mean
      FilterFunction(X@Data[[i]]) <- filter.lin.detrend
      if (Md$Type[i] %in% Stimulus.type.names$Flash) {
        Rejected(X@Data[[i]]) <- autoreject.by.signalfree  # select by signal amplitude???
      }
      if (Md$Type[i] %in% Stimulus.type.names$Flicker) {
        Rejected(X@Data[[i]]) <- autoreject.by.distance
      }
    }
    return(X)
  }
)

#'
#' @importFrom EPhysData Metadata AverageFunction<- Rejected<- FilterFunction<-
#' @keywords internal
#' @noMd
functionupdater<-function(X, where, value, what) {
  Md <- merge(Metadata(X),StimulusTable(X))
  sel <- Where(X,where = where)
  for (i in sel) {
    if (what == "FilterFunction<-") {
      FilterFunction(X@Data[[i]]) <- value
    }
    if (what == "Rejected<-") {
      Rejected(X@Data[[i]]) <- value
    }
    if (what == "AverageFunction<-") {
      AverageFunction(X@Data[[i]]) <- value
    }
  }
  # if (validERGExam(X)) {
     return(X)
  # }
}
