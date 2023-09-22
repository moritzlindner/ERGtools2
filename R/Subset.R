#' Subset from ERGExam Object
#'
#' This method subsets an \code{ERGExam} object into a new object of the same class.
#'
#' @inheritParams EPhysData::Subset
#' @param Step,Eye,Channel Vector of values for Steps, Eyes, and Channels to subset
#' @param ByRecordingIndex Subset by recording index instead of \code{Step}, \code{Eye} and \code{Channel}. Default is \code{FALSE}. If set to a numeric vector,  \code{Step}, \code{Eye} and \code{Channel} will be ignored and only the recording with the given indices will be kept.
#' @details The \code{Subset} function creates a new \code{ERGExam}  object containing a subset of the data from the original object, based on the provided parameters.
#' @seealso \link[EPhysData:Subset]{EPhysData::Subset}
#' @importFrom EPhysData Subset newEPhysSet Metadata
#' @name Subset
#' @exportMethod Subset
setMethod("Subset",
          signature(X = "ERGExam"),
          function(X,
                   Time = NULL,
                   TimeExclusive = FALSE,
                   Repeats = NULL,
                   Raw = TRUE,
                   Step = Steps(X),
                   Eye = Eyes(X),
                   Channel = Channels(X),
                   ByRecordingIndex = FALSE,
                   ...) {

            if(is.logical(ByRecordingIndex)){
              if(ByRecordingIndex){
                stop("'ByRecordingIndex' can only be 'false' or a numeric vector.")
              }
            }else{
              if(is.numeric(ByRecordingIndex)){
                if(!all(ByRecordingIndex %in% 1:nrow(Metadata(X)))){
                  stop("Not all values in 'ByRecordingIndex' ar valid indices for data in 'X'.")
                }
              }else{
                stop("'ByRecordingIndex' can only be 'false' or a numeric vector.")
              }
            }


            Metadata_select <- make_metadata_parilist(X)
            if(is.logical(ByRecordingIndex)){
              Metadata_select$Step <- Step
              Metadata_select$Eye <- Eye
              Metadata_select$Channel <- Channel

              # metadata subset
              MetaSubset<-array(dim=dim(Metadata(X)))
              colnames(MetaSubset)<-colnames(Metadata(X))
              for (i in names(Metadata_select)){
                MetaSubset[,i]<-(Metadata(X)[,i] %in% Metadata_select[[i]])
              }

              MetaSubset<-apply(MetaSubset,1,all)

              # subset data
              Y <- as(X, "EPhysSet")
              Y <- Subset(
                Y,
                Time = Time,
                TimeExclusive = TimeExclusive,
                Repeats = Repeats,
                Metadata_select = Metadata_select,
                Raw = TRUE,
                Simplify = FALSE
              )
              X@Data <- Y@Data
              X@Stimulus<-X@Stimulus[X@Stimulus$Step %in% Y@Metadata$Step,]
            }else{
              X@Data<-X@Data[1:nrow(Metadata(X)) %in% ByRecordingIndex]
              MetaSubset<-1:nrow(Metadata(X)) %in% ByRecordingIndex
              X@Stimulus<-X@Stimulus[X@Stimulus$Step %in% Metadata(X)$Step[MetaSubset],]
            }


           X@Averaged <- X@Averaged

           indexupdate <- as.data.frame(cbind(cumsum(MetaSubset), 1:nrow(Metadata(X))))
           colnames(indexupdate)<-c("new","old")

           X@Measurements <-
             X@Measurements[X@Measurements$Recording %in% seq(1:dim(Metadata(X))[1])[MetaSubset],]
           X@Measurements$Recording <-
             merge(X@Measurements, indexupdate, by.x = "Recording", by.y =
                     "old")$new # update Recording indices.

           X@Metadata <- Metadata(X)[MetaSubset, , drop = F]

           if(validObject(X)){
             return(X)
           }else{
             stop("No valid ERGExam object obtained.")
           }
          })
