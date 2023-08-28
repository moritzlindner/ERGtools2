#' Clear Computed Slots from an ERGExam object
#'
#' Clears the Averaged and Measurements slots in an ERGExam object, respectiveley, by removing any existing data.
#'
#'
#' @return An ERGExam object with the Results slot cleared.
#'
#' @export
#' @examples
#' # Clear the Results slot of an ERGExam object
#' cleared_exam <- clearResults(exam)
#' @inheritParams Subset
#' @export ClearMeasurements
setGeneric(
  name = "ClearMeasurements",
  def = function(X) {
    standardGeneric("ClearMeasurements")
  }
)
#' @noMd
setMethod("ClearMeasurements",
          signature(X = "ERGExam"),
          function(X) {
            X@Measurements <- data.frame()
            X@Measurements.imported <- F
            if (validObject(X)) {
              return(X)
            }
          })

#' @export ClearAveraged
setGeneric(
  name = "ClearAveraged",
  def = function(X) {
    standardGeneric("ClearAveraged")
  }
)
#' @noMd
setMethod("ClearAveraged",
          signature(X = "ERGExam"),
          function(X) {
            X@Averaged <- data.frame()
            X@Averaged.imported <- F
            if (validObject(X)) {
              return(X)
            }
          })
