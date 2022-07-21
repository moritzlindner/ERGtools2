#' Show
#'
#' `r lifecycle::badge("stable")` \cr
#' Default method \code{show} for \link[=ERGExam]{ERGExam}.
#'
#' @importMethodsFrom methods show
#' @param object An S4 object of type \link[=ERGExam]{ERGExam}
#' @importFrom knitr kable
#' @exportMethod show
#' @name show
NULL

#' @noMd
setMethod("show",
          "ERGExperiment",
          function(object) {
            cat("An object of class ERGExperiment \n")
            cat("Protocol:", object@ProtocolName, "Version: ", object@Version, " \n")
            cat("From", object@Patient, ", DOB:", as.character(object@DOB),"\n")
            cat("Recorded", as.character(object@Created),"\n")
            cat("Steps",object@Descriptions,sep="\n\t")
            cat("Source", object@Filename, " Impoted as ", TypeOf(object), "\n")
          })

setMethod("show",
          "ERGExam",
          function(object) {

            content<-character()
            if(max(dim(object@Measurements))>0){
              content<-"Measurements"
            }
            if(length(object@Steps_AVG)>0){
              content<-paste0(content,", average traces")
            }
            if(length(object@Steps_RAW)>0){
              content<-paste0(content,", raw traces")
            }


            cat("An object of class ERGExam. \n")
            cat("Protocol:", object@ProtocolName, ", Version: ", object@Version, " \n")
            cat("From: ", object@Patient, ", DOB:", as.character(object@DOB),", Group:", as.character(object@Group),"\n")
            cat("Recorded: ", as.character(object@ExamDate),"\n")
            cat("Steps:")
            print(kable(object@Stimulus))
            cat("\nChannels:",object@Channels,sep="\n\t")
            cat("Content: ",content,"\n")
            cat("Source:", object@Filename)
          })
