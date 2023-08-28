#' #' Show
#' #'
#' #' `r lifecycle::badge("stable")` \cr
#' #' Default method \code{show} for \link[=ERGExam]{ERGExam}.
#' #'
#' #' @importMethodsFrom methods show
#' #' @param object An S4 object of type \link[=ERGExam]{ERGExam}
#' #' @importFrom knitr kable
#' #' @exportMethod show
#' #' @name show
#' NULL
#'
#' #' @noMd
#' setMethod("show",
#'           "ERGExperiment",
#'           function(object) {
#'             cat("An object of class ERGExperiment \n")
#'             cat("Protocol:", object@ProtocolName, "Version: ", object@Version, " \n")
#'             cat("From", object@Patient, ", DOB:", as.character(object@DOB),"\n")
#'             cat("Recorded", as.character(object@Created),"\n")
#'             cat("Steps",object@Descriptions,sep="\n\t")
#'             cat("Source", object@Filename, " Impoted as ", TypeOf(object), "\n")
#'           })
