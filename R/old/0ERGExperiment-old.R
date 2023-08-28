#' validPCollection<-function(object) {
#'   ret=0
#'   if ((!(length(object@Steps_RAW) == length(object@Descriptions)) && length(object@Steps_RAW)!=0) ||
#'       (!(length(object@Steps_AVG) == length(object@Descriptions))) && length(object@Steps_AVG)!=0 ||
#'       (!(length(object@Steps_Measurements) == length(object@Descriptions))) && length(object@Steps_Measurements)!=0
#'       ){
#'     ret<-ret+1
#'     stop("Descriptions list incompatible to items in Steps")
#'   }
#'   if(any(duplicated(object@Descriptions))){
#'     ret<-ret+1
#'     stop(paste("Descriptions provided not unique: ", object@Descriptions[duplicated(Descriptions)]))
#'   }
#'   if(!length(object@Steps_AVG)==0){
#'     if (!length(object@Steps_AVG) == length(object@Steps_Excl)){
#'       ret<-ret+1
#'       stop("List of Average Traces and Excluded Traces not compatible")
#'     }
#'   }
#'
#'   if(length(object@Steps_RAW)!=0 && length(object@Steps_AVG)!=0 ){
#'     if (!length(object@Steps_RAW) == length(object@Steps_AVG)){
#'       ret<-ret+1
#'       stop("List of Raw Traces and Average Traces not compatible")
#'     }
#'   }
#'
#'   if(length(object@Steps_RAW)!=0 && length(object@Steps_Measurements)!=0 ){
#'     if (!length(object@Steps_RAW) == length(object@Steps_Measurements)){
#'       ret<-ret+1
#'       stop("List of Raw Traces and Stats not compatible")
#'     }
#'   }
#'
#'   if (!all(unlist(lapply(object@Steps_RAW,function(x) GetRecParam(x,"Filename"))) %in% object@Filename)){
#'     ret<-ret+1
#'     print(object@Filename)
#'     stop("Unequal file names")
#'   }
#'   if(ret==0) {TRUE} else {FALSE}
#' }
#'
#' #' An S4 class storing an ERG Exam/Experiment
#' #'
#' #' TBC
#' #'
#' #' @seealso \linkS4class{ERGExperiment}
#' #' @importFrom methods setClass new
#' #' @import PatchR
#' #' @exportClass ERGExperiment
#' ERGExperiment <- setClass(
#'   Class = "ERGExperiment",
#'   slots =  list(
#'     Steps_RAW = "list",
#'     Steps_AVG = "list",
#'     Steps_Excl = "list",
#'     Steps_Measurements = "list",
#'     Descriptions = "character",
#'     ProtocolName = "character",
#'     Version = "character",
#'     Created = "POSIXct",
#'     Filename = "character",
#'     RecMode = "character",
#'     Patient = "character",
#'     DOB = "character",
#'     Imported = "POSIXct"
#'   ),
#'   prototype = list("Imported" = Sys.time()),
#'   validity = validPCollection
#' )
