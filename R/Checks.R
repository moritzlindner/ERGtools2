#' Check if all recordings in an ERGExam object have a valid averaging function set.
#'
#' @param X An ERGExam object.
#'
#' @return Logical, TRUE if all recordings have a valid averaging function, FALSE otherwise.
#'
#' @examples
#' data(ERG)
#' CheckAvgFxSet(ERG)
#' ERG<-SetStandardFunctions(ERG)
#' CheckAvgFxSet(ERG)
#'
#' @seealso
#' \code{\link{AverageFunction<-}}, \code{\link{SetStandardFunctions}}
#'
#' @export
setGeneric(
  name = "CheckAvgFxSet",
  def = function(X)
  {
    standardGeneric("CheckAvgFxSet")
  }
)
#' @noMd
setMethod("CheckAvgFxSet",
          "ERGExam",
          function(X) {
            fx.set <- unlist(lapply(X@Data, function(x) {
              suppressWarnings({
                dat <-
                  GetData(x, Raw = FALSE, Time = c(min(TimeTrace(x)), TimeTrace(x)[min(5,length(TimeTrace(x)))]))
              })
              return(as.logical(ncol(dat) == 1))
            }))
            if (!all(fx.set)) {
                warning("No valid averaging function found for Recoding ",
                        paste0(which(!fx.set), ","))
                warning(
                  "Not all recordings in X have a valid Averaging Function set. Result of averaging Data must be a vector. See documentation for 'AverageFunction<-' or 'SetStandardFunctions'."
                )
                return(FALSE)
            } else{
              return(TRUE)
            }
          })
