#' Set Methods for Exam and Subject information for the \linkS4class{ERGExam} object
#'
#' These methods are used to set exam and subject information from \linkS4class{ERGExam} or \linkS4class{ERGMeasurements} objects.
#' @inheritParams Get
#' @param value The new value to be set for the specified field.
#' @return An updated version of the \linkS4class{ERGExam} object.
#'
#' @examples
#' data(ERG)  # assuming data(ERG) gives a valid ERGExam object
#'
#' # Set Amplifier
#' Amplifier(ERG) <- "Amp_001"
#' Amplifier(ERG)
#'
#' # Set Disease
#' Disease(ERG) <- "None"
#' Disease(ERG)
#'
#' # Set Development
#' Development(ERG) <- "Adult"
#' Development(ERG)
#'
#' # Set Electrodes
#' # (Assume electrodes_list is a named list of ERGElectrode objects where names match each element's Name(ERGElectrode))
#' # Electrodes(ERG) <- electrodes_list
#'
#' # Set Genotype
#' Genotype(ERG) <- "Wild-type"
#' Genotype(ERG)
#'
#' # Set Genus (using a recognized genus, e.g., "Homo")
#' Genus(ERG) <- "Mus"
#' Genus(ERG)
#'
#' # Set ID
#' ID(ERG) <- "Subject_001"
#' ID(ERG)
#'
#' # Set Investigator
#' Investigator(ERG) <- "Dr. Smith"
#' Investigator(ERG)
#'
#' # Set Label (if applicable)
#' Label(ERG) <- "Fluorescent"
#' Label(ERG)
#'
#' # Set ProtocolName
#' ProtocolName(ERG) <- "ERG_Protocol_1"
#' ProtocolName(ERG)
#'
#' # Set ProtocolVersion
#' ProtocolVersion(ERG) <- "1.0"
#' ProtocolVersion(ERG)
#'
#' # Set RecMode
#' RecMode(ERG) <- "ModeA"
#' RecMode(ERG)
#'
#' # Set Recorder
#' Recorder(ERG) <- "Rec_001"
#' Recorder(ERG)
#'
#' # Set Species (using a recognized species, e.g., "sapiens")
#' Species(ERG) <- "musculus"
#' Species(ERG)
#'
#' # Set Stimulator
#' Stimulator(ERG) <- "Stim_001"
#' Stimulator(ERG)
#'
#' # Set Strain
#' Strain(ERG) <- "C57BL/6"
#' Strain(ERG)
#' @name Set
NULL

#' @describeIn Set Sets the Amplifier field.
#' @exportMethod Amplifier<-
setGeneric("Amplifier<-", function(X, value) standardGeneric("Amplifier<-"))
#' @noMd
setMethod("Amplifier<-", "ERGExam", function(X, value) {
  X@ExamInfo$Amplifier <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the Disease information.
#' @exportMethod Disease<-
setGeneric("Disease<-", function(X, value) standardGeneric("Disease<-"))
#' @noMd
setMethod("Disease<-", "ERGExam", function(X, value) {
  X@SubjectInfo$Disease <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the Development information.
#' @exportMethod Development<-
setGeneric("Development<-", function(X, value) standardGeneric("Development<-"))
#' @noMd
setMethod("Development<-", "ERGExam", function(X, value) {
  X@SubjectInfo$Development <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the Electrodes field.
#' @exportMethod Electrodes<-
setGeneric("Electrodes<-", function(X, value) standardGeneric("Electrodes<-"))
#' @noMd
setMethod("Electrodes<-", "ERGExam", function(X, value) {
  # Check that 'value' is a list of ERGElectrode objects.
  if (!is.list(value))
    stop("Value must be a list of ERGElectrode objects.")

  # Validate each element in the list is an ERGElectrode object.
  for (i in seq_along(value)) {
    if (!is(value[[i]], "ERGElectrode"))
      stop(sprintf("Element %d is not an ERGElectrode object.", i))
  }

  # Extract the names from each ERGElectrode object.
  electrode_names <- sapply(value, function(el) el@Name)

  # Check that the list entries are named and that names match the ERGElectrode@Name values.
  if (is.null(names(value)) || !all(names(value) == electrode_names))
    stop("List entries must be named exactly as the corresponding ERGElectrode@Name values.")

  X@ExamInfo$Electrodes <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the Genotype information.
#' @exportMethod Genotype<-
setGeneric("Genotype<-", function(X, value) standardGeneric("Genotype<-"))
#' @noMd
setMethod("Genotype<-", "ERGExam", function(X, value) {
  X@SubjectInfo$Genotype <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the Genus information.
#' Uses \code{validateTaxa} to ensure that the provided value is a recognized genus.
#' @exportMethod Genus<-
setGeneric("Genus<-", function(X, value) standardGeneric("Genus<-"))
#' @noMd
setMethod("Genus<-", "ERGExam", function(X, value) {
  valid <- validateTaxa(value,"genus")
  if (identical(valid, FALSE) || (is.character(valid) && valid != "genus")) {
    Notice(X, what ="W", notice_text = "Potentially invalid Genus provided. The provided value does not correspond to a recognized species or the taxize-package is not installed.")
  }
  X@SubjectInfo$Genus <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the ID information.
#' @exportMethod ID<-
setGeneric("ID<-", function(X, value) standardGeneric("ID<-"))
#' @noMd
setMethod("ID<-", "ERGExam", function(X, value) {
  X@SubjectInfo$ID <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the Investigator.
#' @exportMethod Investigator<-
setGeneric("Investigator<-", function(X, value) standardGeneric("Investigator<-"))
#' @noMd
setMethod("Investigator<-", "ERGExam", function(X, value) {
  X@ExamInfo$Investigator <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the Label information.
#' @exportMethod Label<-
setGeneric("Label<-", function(X, value) standardGeneric("Label<-"))
#' @noMd
setMethod("Label<-", "ERGExam", function(X, value) {
  X@SubjectInfo$Label <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the ProtocolName.
#' @exportMethod ProtocolName<-
setGeneric("ProtocolName<-", function(X, value) standardGeneric("ProtocolName<-"))
#' @noMd
setMethod("ProtocolName<-", "ERGExam", function(X, value) {
  X@ExamInfo$ProtocolName <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the ProtocolVersion.
#' @exportMethod ProtocolVersion<-
setGeneric("ProtocolVersion<-", function(X, value) standardGeneric("ProtocolVersion<-"))
#' @noMd
setMethod("ProtocolVersion<-", "ERGExam", function(X, value) {
  X@ExamInfo$Version <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the Recording Mode.
#' @exportMethod RecMode<-
setGeneric("RecMode<-", function(X, value) standardGeneric("RecMode<-"))
#' @noMd
setMethod("RecMode<-", "ERGExam", function(X, value) {
  X@ExamInfo$RecMode <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the Recorder field.
#' @exportMethod Recorder<-
setGeneric("Recorder<-", function(X, value) standardGeneric("Recorder<-"))
#' @noMd
setMethod("Recorder<-", "ERGExam", function(X, value) {
  X@ExamInfo$Recorder <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the Species information.
#' Uses \code{validateTaxa} to ensure that the provided value is a recognized species.
#' @exportMethod Species<-
setGeneric("Species<-", function(X, value) standardGeneric("Species<-"))
#' @noMd
setMethod("Species<-", "ERGExam", function(X, value) {

  if(lengths(gregexpr("\\W+", value)) <= 1){
    value<-paste(Genus(X),value)
  }
  valid <- validateTaxa(value,"species")
  if (identical(valid, FALSE) || (is.character(valid) && valid != "species")) {
    Notice(X, what ="W", notice_text = "Potentially invalid Species provided. The provided value does not correspond to a recognized species or the taxize-package is not installed.")
  }
  X@SubjectInfo$Species <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the Stimulator information.
#' @exportMethod Stimulator<-
setGeneric("Stimulator<-", function(X, value) standardGeneric("Stimulator<-"))
#' @noMd
setMethod("Stimulator<-", "ERGExam", function(X, value) {
  X@ExamInfo$Stimulator <- value
  X<-LogChange(X)
  return(X)
})

#' @describeIn Set Sets the Strain information.
#' @exportMethod Strain<-
setGeneric("Strain<-", function(X, value) standardGeneric("Strain<-"))
#' @noMd
setMethod("Strain<-", "ERGExam", function(X, value) {
  X@SubjectInfo$Strain <- value
  X<-LogChange(X)
  return(X)
})
