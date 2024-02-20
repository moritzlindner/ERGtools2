#' #' Clear Computed Slots from an ERGExam object
#' #'
#' #' Clears the Measurements slots in an  \link[=ERGExam]{ERGExam} object, respectively, by removing any existing data.
#' #'
#' #' @return An ERGExam object with the Results slot cleared.
#' #' @inheritParams Subset-method
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
