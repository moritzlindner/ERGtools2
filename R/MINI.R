#' Report MINI Information for an ERGExam object
#'
#' This method extracts relevant MINI information as proposed by Gibson et al (https://doi.org/10.1038/npre.2009.1720.2) from the \code{ExamInfo} and \code{SubjectInfo} slots of an \code{ERGExam} object.
#' It returns a one-row \code{data.frame} containing exam and subject details.
#' Additionally, if both \code{ExamDate} (from \code{ExamInfo}) and \code{DOB} (from \code{SubjectInfo}) are available,
#' the subject's \code{Age} is computed as the difference between these dates in years.
#'
#' @inheritParams Where
#' @param ValidateTaxa Should taxonomy data be validated? Default: \code{TRUE}.
#' @param tidy Logical. Should the output be returned in a tidy vertical format? Default: \code{FALSE}.
#' @return A \code{data.frame} with one row, comprising the extracted MINI exam and subject information,
#'         including a computed \code{Age} field.
#' @importFrom cli cli_warn
#' @examples
#' data(ERG)
#' MINI(ERG)
#' @export
setGeneric("MINI", function(X, ValidateTaxa = TRUE, tidy = FALSE) standardGeneric("MINI"))

#' @noMd
setMethod("MINI", "ERGExam", function(X, ValidateTaxa = TRUE, tidy = FALSE) {
  examInfo <- X@ExamInfo
  subjectInfo <- X@SubjectInfo

  # Define the relevant MINI fields for ExamInfo and SubjectInfo
  examFields <- c("ProtocolName", "Version", "ExamDate", "Filename", "RecMode",
                  "Investigator", "Stimulator", "Amplifier", "Recorder")
  subjectFields <- c("Genus", "Species", "Strain", "Genotype", "Disease",
                     "Development", "Lable", "ID", "Details", "DOB")

  # Helper function to safely extract a field from a list
  extractField <- function(info, field) {
    if (is.null(info) || !(field %in% names(info))) NA else info[[field]]
  }

  examData <- lapply(examFields, function(f) extractField(examInfo, f))
  names(examData) <- examFields

  subjectData <- lapply(subjectFields, function(f) extractField(subjectInfo, f))
  names(subjectData) <- subjectFields

  if(ValidateTaxa){
    if (!validateTaxa(subjectData$Genus,"genus")) {
      Notice(X, what ="W", notice_text = "Genus validation failed.")
    }

    if(lengths(gregexpr("\\W+", subjectData$Species)) <= 1){
      subjectData$Species<-paste(subjectData$Genus,subjectData$Species)
    }

    if (!validateTaxa(subjectData$Species,"species")) {
      Notice(X, what ="W", notice_text = "Species validation failed.")
    }
  }
  # Calculate Age
  Age <- Age(X)

  electrodes<-list()
  for (i in 1:length(X@ExamInfo$Electrodes)){
    curr<-as.data.frame(X@ExamInfo$Electrodes[[i]])
    colnames(curr)<-paste0("Electrode", i,"_", colnames(curr))
    electrodes<-c(electrodes,curr)
  }
  electrodes<-as.data.frame(electrodes)


  # Combine ExamInfo and SubjectInfo data into a single one-row data.frame, adding the Age field
  carmenData <- c(subjectData, examData, electrodes)
  carmenData[["Age"]] <- Age

  df <- as.data.frame(carmenData, stringsAsFactors = FALSE)

  if (tidy) {
    df <- data.frame(Variable = names(carmenData),
                     Value = unlist(carmenData, use.names = FALSE),
                     stringsAsFactors = FALSE)
  }

  return(df)
})

#' Validate Genus/Species combination using taxize (internal)
#'
#' This function checks whether the provided Genus and Species correspond to a recognized taxon.
#' If the \pkg{taxize} package is available, it uses \code{taxize::get_uid} to perform the lookup.
#' Otherwise, it issues a CLI warning and skips the taxonomic validation.
#'
#' @param genus A character string specifying the genus.
#' @param species A character string specifying the species.
#'
#' @return \code{TRUE} if the taxon is recognized, \code{FALSE} if not, and \code{NA} if \pkg{taxize} is not installed.
#'
#' @importFrom cli cli_warn
#' @keywords internal
validateTaxa <- function(variable,rank_query) {
  if (requireNamespace("taxize", quietly = TRUE)) {
    uid <- taxize::get_uid(variable, messages = FALSE, ask =F, rank_query = rank_query)
    if (is.na(uid[1])) {
      cli_warn("The provided data '{variable}' does not correspond to a recognized taxon.")
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    cli_warn("The 'taxize' package is not installed. Skipping taxonomic validation of Genus/Species.")
    return(NA)
  }
}
