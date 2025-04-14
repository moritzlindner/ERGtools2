#' Accession methods for metadata from ERGExam, ERGMeasurements, and ERGElectrode classes
#'
#' These methods are used to access metadata information from \linkS4class{ERGExam}, \linkS4class{ERGMeasurements}, or \linkS4class{ERGElectrode} objects.
#' @param X An \linkS4class{ERGExam}, \linkS4class{ERGMeasurements}, or \linkS4class{ERGElectrode} object.
#' @return A vector (or a data.frame for \code{Electrodes()}). For some functions (e.g., \code{GetFilterFunction()} or \code{GetAverageFunction()}), a function is returned.
#' @importFrom units as_units
#'
#' @examples
#' data(ERG)
#' Amplifier(ERG)
#' Channels(ERG)
#' Details(ERG)
#' Development(ERG)
#' Disease(ERG)
#' DOB(ERG)
#' Electrodes(ERG)
#' ExamDate(ERG)
#' Eyes(ERG)
#' Filename(ERG)
#' Genotype(ERG)
#' Genus(ERG)
#' GroupName(ERG)
#' ID(ERG)
#' Investigator(ERG)
#' Label(ERG)
#' MarkerNames(ERG)
#' ProtocolName(ERG)
#' ProtocolVersion(ERG)
#' RecMode(ERG)
#' Recorder(ERG)
#' Repeats(ERG)
#' Species(ERG)
#' Steps(ERG)
#' Stimulator(ERG)
#' Subject(ERG)
#' Strain(ERG)
#' @name Get
NULL

#' @describeIn Get Returns the age of the subject.
#' @exportMethod Age
setGeneric("Age", function(X) standardGeneric("Age"))
#' @noMd
setMethod("Age", "ERGExam", function(X) {
  if (!is.na(ExamDate(X)) && !is.na(DOB(X))) {
    return(as_units(difftime(ExamDate(X), DOB(X), units = "days")))
  } else {
    return(NA)
  }
})

#' @describeIn Get Returns the Amplifier information.
#' @exportMethod Amplifier
setGeneric("Amplifier", function(X) standardGeneric("Amplifier"))
#' @noMd
setMethod("Amplifier", "ERGExam", function(X) {
  return(X@ExamInfo$Amplifier)
})

#' @describeIn Get Returns the Channel names.
#' @exportMethod Channels
setGeneric("Channels", function(X) standardGeneric("Channels"))
#' @noMd
setMethod("Channels", "ERGExam", function(X) {
  unique(X@Metadata$Channel)
})
#' @noMd
setMethod("Channels", "ERGMeasurements", function(X) {
  unique(X@Marker$Channel)
})

#' @describeIn Get Returns the Details information.
#' @exportMethod Details
setGeneric("Details", function(X) standardGeneric("Details"))
#' @noMd
setMethod("Details", "ERGExam", function(X) {
  return(X@SubjectInfo$Details)
})

#' @describeIn Get Returns the Development information.
#' @exportMethod Development
setGeneric("Development", function(X) standardGeneric("Development"))
#' @noMd
setMethod("Development", "ERGExam", function(X) {
  return(X@SubjectInfo$Development)
})

#' @describeIn Get Returns the Disease information.
#' @exportMethod Disease
setGeneric("Disease", function(X) standardGeneric("Disease"))
#' @noMd
setMethod("Disease", "ERGExam", function(X) {
  return(X@SubjectInfo$Disease)
})

#' @describeIn Get Returns the Date of Birth (DOB) information.
#' @exportMethod DOB
setGeneric("DOB", function(X) standardGeneric("DOB"))
#' @noMd
setMethod("DOB", "ERGExam", function(X) {
  return(X@SubjectInfo$DOB)
})

#' @describeIn Get Returns the Electrodes information as a combined data frame.
#' Each entry in the ExamInfo "Electrodes" list is converted using as.data.frame and the results are row-bound.
#' @exportMethod Electrodes
setGeneric("Electrodes", function(X) standardGeneric("Electrodes"))
#' @noMd
setMethod("Electrodes", "ERGExam", function(X) {
  electrodes <- X@ExamInfo$Electrodes
  if (is.null(electrodes) || length(electrodes) == 0) {
    return(data.frame())
  }
  df_list <- lapply(electrodes, as.data.frame)
  result_df <- do.call(rbind, df_list)
  return(result_df)
})

#' @describeIn Get Returns the exam date.
#' @exportMethod ExamDate
setGeneric("ExamDate", function(X) standardGeneric("ExamDate"))
#' @noMd
setMethod("ExamDate", "ERGExam", function(X) {
  X@ExamInfo$ExamDate
})

#' @describeIn Get Returns the eyes of which the data has been recorded.
#' @exportMethod Eyes
setGeneric("Eyes", function(X) standardGeneric("Eyes"))
#' @noMd
setMethod("Eyes", "ERGExam", function(X) {
  unique(X@Metadata$Eye)
})

#' @describeIn Get Returns the source filename.
#' @exportMethod Filename
setGeneric("Filename", function(X) standardGeneric("Filename"))
#' @noMd
setMethod("Filename", "ERGExam", function(X) {
  return(X@ExamInfo$Filename)
})

#' @describeIn Get Returns the Genotype information.
#' @exportMethod Genotype
setGeneric("Genotype", function(X) standardGeneric("Genotype"))
#' @noMd
setMethod("Genotype", "ERGExam", function(X) {
  return(X@SubjectInfo$Genotype)
})

#' @describeIn Get Returns the Genus information.
#' @exportMethod Genus
setGeneric("Genus", function(X) standardGeneric("Genus"))
#' @noMd
setMethod("Genus", "ERGExam", function(X) {
  return(X@SubjectInfo$Genus)
})

#' @describeIn Get Returns the group name.
#' @exportMethod GroupName
setGeneric("GroupName", function(X) standardGeneric("GroupName"))
#' @noMd
setMethod("GroupName", "ERGExam", function(X) {
  X@SubjectInfo$Group
})

#' @describeIn Get Returns the ID information.
#' @exportMethod ID
setGeneric("ID", function(X) standardGeneric("ID"))
#' @noMd
setMethod("ID", "ERGExam", function(X) {
  return(X@SubjectInfo$ID)
})

#' @describeIn Get Returns the Investigator.
#' @exportMethod Investigator
setGeneric("Investigator", function(X) standardGeneric("Investigator"))
#' @noMd
setMethod("Investigator", "ERGExam", function(X) {
  return(X@ExamInfo$Investigator)
})

#' @describeIn Get Returns the Label information.
#' @exportMethod Label
setGeneric("Label", function(X) standardGeneric("Label"))
#' @noMd
setMethod("Label", "ERGExam", function(X) {
  return(X@SubjectInfo$Label)
})

#' @describeIn Get Returns the recording protocol name.
#' @exportMethod ProtocolName
setGeneric("ProtocolName", function(X) standardGeneric("ProtocolName"))
#' @noMd
setMethod("ProtocolName", "ERGExam", function(X) {
  X@ExamInfo$ProtocolName
})

#' @describeIn Get Returns the Protocol Version.
#' @exportMethod ProtocolVersion
setGeneric("ProtocolVersion", function(X) standardGeneric("ProtocolVersion"))
#' @noMd
setMethod("ProtocolVersion", "ERGExam", function(X) {
  return(X@ExamInfo$Version)
})

#' @describeIn Get Returns the Recording Mode.
#' @exportMethod RecMode
setGeneric("RecMode", function(X) standardGeneric("RecMode"))
#' @noMd
setMethod("RecMode", "ERGExam", function(X) {
  return(X@ExamInfo$RecMode)
})

#' @describeIn Get Returns the Recorder information.
#' @exportMethod Recorder
setGeneric("Recorder", function(X) standardGeneric("Recorder"))
#' @noMd
setMethod("Recorder", "ERGExam", function(X) {
  return(X@ExamInfo$Recorder)
})

#' @describeIn Get Returns the indices of individual repeats contained in an ERG exam.
#' @exportMethod Repeats
setGeneric(
  name = "Repeats",
  def = function(X)
  {
    standardGeneric("Repeats")
  }
)
#' @noMd
setMethod("Repeats",
          "ERGExam",
          function(X) {
            res<-unique(X@Metadata$Repeat)
            if(length(res)!=1 || res!=1){
              warning("Using multiple Repeats is still experimental. When creating the ERGExam, set Repeats to 1, if not needed.")
            }
            return(res)
          })

#' @describeIn Get Returns the Species information.
#' @exportMethod Species
setGeneric("Species", function(X) standardGeneric("Species"))
#' @noMd
setMethod("Species", "ERGExam", function(X) {
  return(X@SubjectInfo$Species)
})

#' @describeIn Get Returns the steps of the exam.
#' @exportMethod Steps
setGeneric("Steps", function(X) standardGeneric("Steps"))
#' @noMd
setMethod("Steps", "ERGExam", function(X) {
  unique(X@Metadata$Step)
})

#' @describeIn Get Returns the Stimulator information.
#' @exportMethod Stimulator
setGeneric("Stimulator", function(X) standardGeneric("Stimulator"))
#' @noMd
setMethod("Stimulator", "ERGExam", function(X) {
  return(X@ExamInfo$Stimulator)
})

#' @describeIn Get Returns the subject's name.
#' @exportMethod Subject
setGeneric("Subject", function(X) standardGeneric("Subject"))
#' @noMd
setMethod("Subject", "ERGExam", function(X) {
  X@SubjectInfo$Subject
})

#' @describeIn Get Returns the Strain information.
#' @exportMethod Strain
setGeneric("Strain", function(X) standardGeneric("Strain"))
#' @noMd
setMethod("Strain", "ERGExam", function(X) {
  return(X@SubjectInfo$Strain)
})



#' @describeIn Get Returns the measurement parameter names (e.g., 'a', 'B', 'N1', 'P1'). For \linkS4class{ERGMeasurements} objects.
#' @exportMethod MarkerNames
setGeneric("MarkerNames", function(X) standardGeneric("MarkerNames"))
#' @noMd
setMethod("MarkerNames", "ERGMeasurements", function(X) {
  unique(X@Marker$Name)
})
#' @noMd
setMethod("MarkerNames", "ERGExam", function(X) {
  MarkerNames(X@Measurements)
})
