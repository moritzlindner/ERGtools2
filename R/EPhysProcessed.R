#' validEPhysProcessed <- function(object) {
#'   if (!inherits(object@Raw, "EphyRAW")) {
#'     return("Raw slot must be an object of class 'EphyRAW'")
#'   }
#'
#'   if (!is.logical(object@Rejected) || length(object@Rejected) != dim(object@Raw@Data)[2]) {
#'     return("Rejected slot must be a logical vector with the length dim(Raw@Data)[2]")
#'   }
#'
#'   if (!is.function(object@filter.fx)) {
#'     return("filter.fx slot must be a function")
#'   }
#'
#'   if (!is.function(object@average.fx)) {
#'     return("average.fx slot must be a function")
#'   }
#'
#'   return(TRUE)  # Object is valid
#' }
#'
#' #' @exportClass
#' EPhysProcessed<-setClass(
#'   "EPhysProcessed",
#'   slots = list(
#'     Raw = "EphyRAW",
#'     Rejected = "logical",
#'     filter.fx = "function",
#'     average.fx = "function"
#'   ),
#'   prototype = list(
#'     Raw = NULL,
#'     Rejected = logical(0),
#'     filter.fx = function(x){x},
#'     average.fx = mean
#'   ),
#'   validity = validEPhysProcessed
#' )
#'
