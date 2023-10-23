#' Subset from ERGExam Object
#'
#' This method subsets an \code{ERGExam} object into a new object of the same class.
#'
#' @inheritParams EPhysData::Subset
#' @param Step,Eye,Channel Vector of values for Steps, Eyes, and Channels to subset
#' @param ExamItem Subset by exam item index instead of \code{Step}, \code{Eye} and \code{Channel}. Default is \code{NULL}. If set to a numeric vector or a logical vector of same length as rows in Metadata,  \code{Step}, \code{Eye} and \code{Channel} will be ignored and only the recording with the given indices will be kept.
#' @details The \code{Subset} function creates a new \code{ERGExam}  object containing a subset of the data from the original object, based on the provided parameters.
#' @seealso \link[EPhysData:Subset]{EPhysData::Subset}
#' @importFrom EPhysData Subset newEPhysSet Metadata
#' @importFrom methods validObject
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
                   ExamItem = NULL,
                   ...) {

            if (!is.null(ExamItem)) {
              # ExamItem is in use
              if (any(Step != Steps(X)) |
                  any(Eye != Eyes(X)) |
                  any(Channel != Channels(X))) {
                stop("'Step','Eye' and 'Channel' cant be used together with 'ExamItem'")
              }
              if (is.numeric(ExamItem)) {
                if (!all(ExamItem %in% 1:nrow(Metadata(X)))) {
                  stop("Not all values in 'ExamItem' ar valid indices for data in 'X'.")
                }
                ExamItem <-
                  (1:length(X) %in% ExamItem) # transform to logical index
              }else{
                if (length(ExamItem) != length(X)) {
                  # length does not match
                  stop(
                    "'ExamItem' can only be 'NULL' or a numeric or logical vector of same length as items in 'Metadata(X)'."
                  )
                }
              }
            }

            if (!all(Step %in% Steps(X)) |
                !all(Eye %in% Eyes(X)) | !all(Channel %in% Channels(X))) {
              stop("Values for 'Step','Eye' and 'Channel' must exist in the metadata.")
            }

            if (is.null(ExamItem)) {
              metadata <- Metadata(X)
              ExamItem <- array(dim = dim(metadata))
              colnames(ExamItem) <- colnames(metadata)
              ExamItem <- apply(ExamItem, 2, as.logical)

              ExamItem[, "Step"] <- (metadata$Step %in% Step)
              ExamItem[, "Channel"] <-
                (metadata$Channel %in% Channel)
              ExamItem[, "Eye"] <- (metadata$Eye %in% Eye)

              ExamItem <-
                apply(ExamItem, 1, function(x) {
                  all(x, na.rm = T)
                })
            }

            # subset measurements slot
            indexupdate <-
              as.data.frame(cbind(cumsum(ExamItem)[ExamItem], which(ExamItem)))
            colnames(indexupdate) <- c("new", "old")

            measurements <- X@Measurements
            measurements <-
              merge(measurements, indexupdate, by.x = "Recording", by.y = "old")
            measurements$Recording <- measurements$new
            measurements$new <- NULL

            # subset data
            Y <- as(X, "EPhysSet")
            Y <- Subset(
              Y,
              Time = Time,
              TimeExclusive = TimeExclusive,
              Repeats = Repeats,
              SetItem = ExamItem,
              Raw = Raw,
              Simplify = FALSE
            )

            # subset stimulus
           stimtab <-
              StimulusTable(X)[StimulusTable(X)$Step %in% Metadata(Y)$Step, ]

            # subset Metadata

            metadata<-Metadata(X)[ExamItem,]

            out<-new("ERGExam",
                Data = Y@Data,
                Metadata = metadata,
                Stimulus = stimtab,
                Averaged = X@Averaged,
                Measurements = measurements,
                ExamInfo = X@ExamInfo,
                SubjectInfo = X@SubjectInfo,
                Imported = X@Imported)

           if(validObject(out)){
             return(out)
           }
          })
