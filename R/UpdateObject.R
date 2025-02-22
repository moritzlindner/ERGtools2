#' Update an old (v. 0.7 / 0.8) ERGExam object
#'
#' This method can be used to update an \linkS4class{ERGExam} object created in older versions of ERGtools2.
#' @inheritParams Subset-method
#' @return An updated  \linkS4class{ERGExam} object.
#' @name UpdateERGExam
#' @exportMethod UpdateERGExam
setGeneric(
  name = "UpdateERGExam",
  def = function(X) {
    standardGeneric("UpdateERGExam")
  }
)
#' @noMd
setMethod("UpdateERGExam",
          "ERGExam",
          function(X) {
            if ("Intensity" %in% colnames(X@Stimulus)) {
              Notice(X, what = "I", notice_text = "i Old Stimulus table column name 'Intensity detected. Will be updated to 'StimulusEnergy'")
              colnames(X@Stimulus)[colnames(X@Stimulus) %in% "Intensity"] <-
                "StimulusEnergy"
            }
            if (!("Stimulator" %in% names(X@ExamInfo))) {
              Notice(X, what = "I", notice_text = "i New ExamInfo fields (Stimulator, Amplifier, Recorder) missing. Adding default empty values.")
              X@ExamInfo$Stimulator <- ""
              X@ExamInfo$Amplifier  <- ""
              X@ExamInfo$Recorder   <- ""
              X@ExamInfo$Electrodes <- list()
            }
            if (!("Genus" %in% names(X@SubjectInfo))) {
              Notice(X, what = "I", notice_text = "i New SubjectInfo fields (Genus, Species, Strain, Genotype, Disease, Development, Lable, ID, Details) missing. Adding default empty values.")
              X@SubjectInfo$Genus       <- ""
              X@SubjectInfo$Species     <- ""
              X@SubjectInfo$Strain      <- ""
              X@SubjectInfo$Genotype    <- ""
              X@SubjectInfo$Disease     <- ""
              X@SubjectInfo$Development <- ""
              X@SubjectInfo$Lable       <- ""
              X@SubjectInfo$ID          <- ""
              X@SubjectInfo$Details     <- ""
            }

            X<-tryCatch(slot(X,"Changelog"), error= function(e){
              Notice(X, what = "I", notice_text = "i Changelog added")
              X@Changelog<-paste0(format(Sys.time(), "%Y%m%d %H:%M:%S"), " - ","Object Updated to v0.85 (Changelog introduced)")
              X
            })

            X
          })
