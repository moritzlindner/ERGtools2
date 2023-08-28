#' #' @exportMethod AutoAverage
#' setGeneric(
#'   name = "TypeOf",
#'   def = function(X)
#'   {
#'     standardGeneric("TypeOf")
#'   }
#' )
#'
#'
#' setMethod("TypeOf",
#'           "ERGExperiment",
#'           function(X) {
#'             if (length(X@Steps_RAW)!=0){
#'               type="RAW"
#'             }
#'             if (length(X@Steps_RAW)==0 && length(X@Steps_AVG)!=0){
#'               type="AVG"
#'             }
#'             if (length(X@Steps_RAW)==0 && length(X@Steps_AVG)==0 && length(X@Steps_Measurements)!=0 ){
#'               type="Measurements"
#'             }
#'             type
#'           }
#' )
