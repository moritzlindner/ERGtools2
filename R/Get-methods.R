#' Accession methods for metadata from ERGExam objects
#'
#' `r lifecycle::badge("stable")` \cr
#' These methods are used to access metadata information from \linkS4class{ERGExam} objects.
#' @param X A \linkS4class{ERGExam} object
#' @details These methods can be used to access metadata information stored in \linkS4class{ERGExam} objects. \cr \cr
#' @return A character vector. For GetStimulusTable a data.frame.
#' @name Get
#'
#' @describeIn Get Returns the channel names
#' @exportMethod GetChannelNames
#' @noMd
setGeneric(
  name = "GetChannelNames",
  def = function(X)
  {
    standardGeneric("GetChannelNames")
  }
)

#' @noMd
setMethod("GetChannelNames",
          "ERGExam",
          function(X) {
            X@Channels
          })
#' ------------------
#' @describeIn Get Returns the stimulus names.
#' @exportMethod GetStimulusNames
#' @noMd
setGeneric(
  name = "GetStimulusNames",
  def = function(X)
  {
    standardGeneric("GetStimulusNames")
  }
)

#' @noMd
setMethod("GetStimulusNames",
          "ERGExam",
          function(X) {
            X@Stimulus$Description
          })

#' ------------------
#' @describeIn Get Returns the measurement parameter names (e.g: 'a','B','N1','P1').
#' @exportMethod GetMarkerNames
#' @noMd
setGeneric(
  name = "GetMarkerNames",
  def = function(X)
  {
    standardGeneric("GetMarkerNames")
  }
)

#' @noMd
setMethod("GetMarkerNames",
          "ERGExam",
          function(X) {
            unique(X@Measurements$Param)
          })

#' ------------------
#' @describeIn Get Returns the stimulus table
#' @exportMethod GetStimulusTable
#' @noMd
setGeneric(
  name = "GetStimulusTable",
  def = function(X)
  {
    standardGeneric("GetStimulusTable")
  }
)

#' @noMd
setMethod("GetStimulusTable",
          "ERGExam",
          function(X) {
            X@Stimulus
          })

#' ------------------
#' @describeIn Get Returns the recording protocol name.
#' @exportMethod GetProtocolName
#' @noMd
setGeneric(
  name = "GetProtocolName",
  def = function(X)
  {
    standardGeneric("GetProtocolName")
  }
)

#' @noMd
setMethod("GetProtocolName",
          "ERGExam",
          function(X) {
            X@ProtocolName
          })

#' ------------------
#' @describeIn Get Returns the group name.
#' @exportMethod GetGroupName
#' @noMd
setGeneric(
  name = "GetGroupName",
  def = function(X)
  {
    standardGeneric("GetGroupName")
  }
)

#' @noMd
setMethod("GetGroupName",
          "ERGExam",
          function(X) {
            X@Group
          })

#' ------------------
#' @describeIn Get Returns the subject name/ID.
#' @exportMethod GetPatientName
#' @noMd
setGeneric(
  name = "GetPatientName",
  def = function(X)
  {
    standardGeneric("GetPatientName")
  }
)

#' @noMd
setMethod("GetPatientName",
          "ERGExam",
          function(X) {
            X@Group
          })

#' ------------------
#' @describeIn Get Returns the exam date.
#' @exportMethod GetExamDate
#' @noMd
setGeneric(
  name = "GetExamDate",
  def = function(X)
  {
    standardGeneric("GetExamDate")
  }
)

#' @noMd
setMethod("GetExamDate",
          "ERGExam",
          function(X) {
            X@ExamDate
          })

#' ------------------
#' @describeIn Get Returns the date of birth.
#' @exportMethod GetDOB
#' @noMd
setGeneric(
  name = "GetDOB",
  def = function(X)
  {
    standardGeneric("GetDOB")
  }
)

#' @noMd
setMethod("GetDOB",
          "ERGExam",
          function(X) {
            X@DOB
          })

#' ------------------
#' @describeIn Get Returns the stimulus names.
#' @exportMethod GetStimulusNames
#' @noMd
setGeneric(
  name = "GetStimulusNames",
  def = function(X)
  {
    standardGeneric("GetStimulusNames")
  }
)

#' @noMd
setMethod("GetStimulusNames",
          "ERGExam",
          function(X) {
            X@Stimulus$Description
          })
