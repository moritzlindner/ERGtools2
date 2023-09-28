#' Merge ERGExams
#'
#' Merges two or more ERGExam objects into a single ERGExam object.
#'
#' @inheritParams Subset
#' @param examX An ERGExam object or a list of ERGExam objects to be merged with \code{X}.
#'
#' @return An ERGExam object representing the merged data.
#'
#' @export
#' @examples
#' # Merge two ERGExams
#' merged_exam <- mergeERGExams(exam1, exam2)
#'
#' # Merge a list of ERGExams
#' exam_list <- list(exam2, exam3, exam4)
#' merged_exam_list <- mergeERGExams(exam1, exam_list)
#'
setGeneric(
  name = "MergeERGExams",
  def = function(X, examX)
  {
    standardGeneric("MergeERGExams")
  }
)

#' @noMd
setMethod("MergeERGExams",
          "ERGExam",
          function(X, examX) {
            if ("ERGExam" %in% class(examX)) {
              return(merge2ERGExams(X, examX))
            }
            if ("list" %in% class(examX)) {
              out <- X
              for (ex in examX) {
                tryCatch(
                  out <- merge2ERGExams(out, ex),
                  error = function(e) {
                    stop("Mergin Exams failed for ", ProtocolName(ex), "with message: ",e)
                  }
                )
              }
              return(out)
            }
          })

#' @noMd
#' @keywords internal
merge2ERGExams <- function(exam1, exam2) {
  if ((exam1@Averaged != exam2@Averaged)) {
    stop("Objects to merge contain averaged and unaveraged data.")
  }

  # Get the maximum Step value from both objects
  maxStep <- max(exam1@Metadata$Step)
  # Combine data, metadata and stimulus
  mergedData <- c(exam1@Data, exam2@Data)
  meta2 <- Metadata(exam2)
  meta2$Step <- meta2$Step + maxStep
  mergedMetadata <- rbind(Metadata(exam1), meta2)
  stimtab1 <- StimulusTable(exam1)
  if (is.null(stimtab1$ProtocolName)) {
    stimtab1$ProtocolName <- ProtocolName(exam1)
  }
  stimtab2 <- StimulusTable(exam2)
  if (is.null(stimtab2$ProtocolName)) {
    stimtab2$ProtocolName <- ProtocolName(exam2)
  }
  stimtab2$Step <- stimtab2$Step + maxStep
  mergedStimulus <- rbind(stimtab1, stimtab2)
  measurements2 <- exam2@Measurements
  mergedMeasurements <-  rbind(exam1@Measurements, measurements2)

  examinfo <- exam1@ExamInfo
  examinfo$ExamDate <- c(ExamDate(exam1), ExamDate(exam2))
  examinfo$Filename <-
    c(exam1@ExamInfo$Filename, exam2@ExamInfo$Filename)

  # Create a new ERGExam instance with merged data and metadata
  mergedExam <- newERGExam(
    Data = c(exam1@Data, exam2@Data),
    Metadata = mergedMetadata,
    Stimulus = mergedStimulus,
    Averaged = exam1@Averaged,
    Measurements = mergedMeasurements,
    ExamInfo = examinfo,
    SubjectInfo = exam1@SubjectInfo
  )

  # Validity checks
  if (!identical(exam1@SubjectInfo, exam2@SubjectInfo)) {
    stop("SubjectInfo must be identical in both exams.")
  }

  exam1Date <- as.POSIXct(exam1@ExamInfo$ExamDate)
  exam2Date <- as.POSIXct(exam2@ExamInfo$ExamDate)
  if (max(abs(difftime(exam1Date, exam2Date, units = "hours")) > 3)) {
    stop("ExamInfo$ExamDate should differ by a maximum of 3 hours.")
  }

  if (nrow(mergedStimulus) != nrow(unique(mergedStimulus))) {
    stop("Rows of Stimulus must be unique in the merged object.")
  }
  # tmp<-merge(Metadata(mergedExam),StimulusTable(mergedExam))
  # tmp<-tmp[,c("Channel","Result","Eye","Description")]
  # if(nrow(tmp)!=nrow(unique(tmp))){
  #   stop("At least one type of recording (for the same eye and channel) is present in both files.")
  # }

  return(mergedExam)
}
