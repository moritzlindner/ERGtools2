#' Electrode Management Methods for ERGExam Objects
#'
#' These functions manage electrodes of an \linkS4class{ERGExam} object.
#' They provide the following operations:
#'
#' @inheritParams Where
#' @param newElectrode An \linkS4class{ERGElectrode} object (used in \code{AddElectrode} and \code{UpdateElectrode}).
#' @param electrodeName A character string specifying the name of the electrode to be dropped or updated.
#' @param addIfNew for \code{UpdateElectrode} Should the electrode be added as new electrode if an electrode with the same name does not yet exist? Default: \code{TRUE}.
#' @importFrom cli cli_abort cli_warn
#' @details
#' The electrode name is always determined by calling \code{Name(newElectrode)}.
#' @examples
#' data(ERG)
#' Electrodes(ERG)
#' e1 <- newERGElectrode("GND", "Platinum", "Tail", as_units(100, "Ohm"))
#' e1
#' \dontrun{ERG<-AddElectrode(ERG,e1) # this fails, because an electrode of the same name already exists}
#' ERG<-UpdateElectrode(ERG,e1)
#' Electrodes(ERG)
#' e1 <- newERGElectrode("NotInObject", "Platinum", "Tail", as_units(100, "Ohm"))
#' e1
#' ERG<-UpdateElectrode(ERG,e1)
#' Electrodes(ERG)
#' \dontrun{ERG<-UpdateElectrode(ERG, e1, addIfNew = F) # This will fail as adding is not permitted}
#'
#' ERG<-DropElectrode(ERG, "NotInObject")
#' Electrodes(ERG)
#'
#' @return The modified \linkS4class{ERGExam} object.
#'
#' @name ElectrodeManagement
NULL

#' @describeIn ElectrodeManagement Adds an \linkS4class{ERGElectrode}. Electrodes with identical names are not permitted within one \linkS4class{ERGExam} object.
#' @exportMethod AddElectrode
setGeneric("AddElectrode", function(X, newElectrode) standardGeneric("AddElectrode"))
#' @noMd
setMethod("AddElectrode",
          signature(X = "ERGExam", newElectrode = "ERGElectrode"),
          function(X, newElectrode) {
            if (!inherits(newElectrode, "ERGElectrode")) {
              cli_abort("newElectrode must be an ERGElectrode object")
            }

            # Extract electrode name using Name()
            electrodeName <- Name(newElectrode)
            if (is.null(electrodeName) || electrodeName == "") {
              Notice(X, what ="E", notice_text = "The electrode name obtained from Name(newElectrode) is empty.")
            }

            # Initialize the Electrodes list if it doesn't exist
            if (is.null(X@ExamInfo$Electrodes)) {
              X@ExamInfo$Electrodes <- list()
            }

            # Check for duplicate names
            if (electrodeName %in% names(X@ExamInfo$Electrodes)) {
              Notice(X, what ="E", notice_text = paste0("An electrode with name '", electrodeName, "' already exists. Duplicate names are not permitted."))
            }

            # Add the electrode under the name derived from Name()
            X@ExamInfo$Electrodes[[electrodeName]] <- newElectrode
            X<-LogChange(X)
            return(X)
          })

#' @describeIn ElectrodeManagement Removes an electrode from an \linkS4class{ERGExam} object by its name.
#' @exportMethod DropElectrode
setGeneric("DropElectrode", function(X, electrodeName) standardGeneric("DropElectrode"))
#' @noMd
setMethod("DropElectrode",
          signature(X = "ERGExam", electrodeName = "character"),
          function(X, electrodeName) {
            if (is.null(X@ExamInfo$Electrodes) || !(electrodeName %in% names(X@ExamInfo$Electrodes))) {
              Notice(X, what ="W", notice_text = paste0("No electrode found with the name '", electrodeName, "'."))
              return(X)
            }
            X@ExamInfo$Electrodes[[electrodeName]] <- NULL
            X<-LogChange(X)
            return(X)
          })

#' @describeIn ElectrodeManagement Replaces/updates an existing electrode (identified by its name) with a new \linkS4class{ERGElectrode} object.
#'  The matching electrode is found by the name stored in the \linkS4class{ERGElectrode} object.
#' @exportMethod UpdateElectrode
setGeneric("UpdateElectrode", function(X, newElectrode, addIfNew = T)
  standardGeneric("UpdateElectrode"))
#' @noMd
setMethod("UpdateElectrode",
          signature(X = "ERGExam", newElectrode = "ERGElectrode"),
          function(X, newElectrode, addIfNew) {
            if (!inherits(newElectrode, "ERGElectrode")) {
              cli_abort("newElectrode must be an ERGElectrode object")
            }

            if (is.null(X@ExamInfo$Electrodes) || !(Name(newElectrode) %in% names(X@ExamInfo$Electrodes))) {
              if(addIfNew) {
                Notice(X, what ="I", notice_text = paste0("No electrode found with the name '", Name(newElectrode), "'. Adding as new electrode."))
                return(AddElectrode(X, newElectrode))
              } else {
                Notice(X, what ="E", notice_text = paste0("No electrode found with the name '", Name(newElectrode), "'. "))
              }
            }
            X@ExamInfo$Electrodes[[Name(newElectrode)]] <- newElectrode
            X<-LogChange(X)
            return(X)
          })
