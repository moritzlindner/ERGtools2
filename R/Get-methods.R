#' Accession methods for metadata from ERGExam objects
#'
#' These methods are used to access metadata information from \linkS4class{ERGExam} objects.
#' @param X A \linkS4class{ERGExam}
#' @details These methods can be used to access metadata information stored in \linkS4class{ERGExam} or objects. \cr \cr
#' @return A vector. For 'StimulusTable()' a data.frame and a function for 'GetFilterFunction()' and 'GetAverageFunction()'.
#' @name Get
NULL

#' @describeIn Get Returns a the eyes of which the data has been recorded.
#' @exportMethod Eyes
setGeneric(
  name = "Eyes",
  def = function(X)
  {
    standardGeneric("Eyes")
  }
)
#' @noMd
setMethod("Eyes",
          "ERGExam",
          function(X) {
            unique(X@Metadata$Eye)
          })

#' @describeIn Get Returns the Channel names.
#' @exportMethod Channels
#' @noMd
setGeneric(
  name = "Channels",
  def = function(X)
  {
    standardGeneric("Channels")
  }
)
#' @noMd
setMethod("Channels",
          "ERGExam",
          function(X) {
            unique(X@Metadata$Channel)
          })

#' @describeIn Get Returns the steps of the exam
#' @exportMethod Steps
#' @noMd
setGeneric(
  name = "Steps",
  def = function(X)
  {
    standardGeneric("Steps")
  }
)
#' @noMd
setMethod("Steps",
          "ERGExam",
          function(X) {
            unique(X@Metadata$Step)
          })

#' @describeIn Get Returns the subject's name
#' @exportMethod Subject
#' @noMd
setGeneric(
  name = "Subject",
  def = function(X)
  {
    standardGeneric("Subject")
  }
)
#' @noMd
setMethod("Subject",
          "ERGExam",
          function(X) {
            X@SubjectInfo$Subject
          })

#' @describeIn Get Returns the stimulus table
#' @param full For \code{StimulusTable} only. Whether to return the full stimulus table (i.e. also any additional data that might have been added by the user or when merging single \linkS4class{ERGExam} using \linkS4class{MergeERGExams}) or only the main columns "Step", "Description", "Intensity", "Background" and "Type". Default is false.
#' @exportMethod StimulusTable
#' @noMd
setGeneric(
  name = "StimulusTable",
  def = function(X,
                 full = F)
  {
    standardGeneric("StimulusTable")
  }
)

#' @noMd
setMethod("StimulusTable",
          "ERGExam",
          function(X,
                   full = F) {
            if(!full){
              return(X@Stimulus[, c("Step", "Description", "Intensity", "Background", "Type")])
            }else{
              return(X@Stimulus)

            }
          })

#' @describeIn Get Returns the stimulus names.
#' @exportMethod StimulusNames
#' @noMd
setGeneric(
  name = "StimulusNames",
  def = function(X)
  {
    standardGeneric("StimulusNames")
  }
)
#' @noMd
setMethod("StimulusNames",
          "ERGExam",
          function(X) {
            X@Stimulus$Description
          })

#' @describeIn Get Returns the measurement parameter names (e.g: 'a','B','N1','P1').
#' @exportMethod MarkerNames
#' @noMd
setGeneric(
  name = "MarkerNames",
  def = function(X)
  {
    standardGeneric("MarkerNames")
  }
)
#' @noMd
setMethod("MarkerNames",
          "ERGExam",
          function(X) {
            unique(X@Measurements$Marker)
          })

#' @describeIn Get Returns the recording protocol name.
#' @exportMethod ProtocolName
#' @noMd
setGeneric(
  name = "ProtocolName",
  def = function(X)
  {
    standardGeneric("ProtocolName")
  }
)

#' @noMd
setMethod("ProtocolName",
          "ERGExam",
          function(X) {
            X@ExamInfo$ProtocolName
          })

#' @describeIn Get Returns the group name.
#' @exportMethod GroupName
#' @noMd
setGeneric(
  name = "GroupName",
  def = function(X)
  {
    standardGeneric("GroupName")
  }
)

#' @noMd
setMethod("GroupName",
          "ERGExam",
          function(X) {
            X@SubjectInfo$Group
          })

#' @describeIn Get Returns the exam date.
#' @exportMethod ExamDate
#' @noMd
setGeneric(
  name = "ExamDate",
  def = function(X)
  {
    standardGeneric("ExamDate")
  }
)

#' @noMd
setMethod("ExamDate",
          "ERGExam",
          function(X) {
            X@ExamInfo$ExamDate
          })

#' @describeIn Get Returns the date of birth.
#' @exportMethod DOB
#' @noMd
setGeneric(
  name = "DOB",
  def = function(X)
  {
    standardGeneric("DOB")
  }
)

#' @noMd
setMethod("DOB",
          "ERGExam",
          function(X) {
            X@SubjectInfo$DOB
          })

#' @describeIn Get Get Measurements table.
#' @exportMethod Measurements
#' @noMd
setGeneric(
  name = "Measurements",
  def = function(X)
  {
    standardGeneric("Measurements")
  }
)
#' @noMd
setMethod("Measurements",
          "ERGExam",
          function(X) {
            Measurements<-X@Measurements
            Measurements<-cbind(Measurements,Metadata(X)[Measurements$Recording,])
            Measurements<-merge(Measurements,StimulusTable(X), by="Step")
            Measurements <-
              Measurements[, c("Description", "Eye", "Channel", "Name", "Voltage", "Time", "Relative")]
            colnames(Measurements)<-c("Step", "Eye", "Channel", "Name", "Voltage", "Time", "Relative")

            return(Measurements)
          })
