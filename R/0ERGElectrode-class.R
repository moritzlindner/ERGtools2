#' @title The ERGElectrode Class
#' @description An S4 class representing an electrode entry.
#' @slot Name Character. The identifier of the electrode (e.g. "Ground").
#' @slot Type Character. The type of the electrode (e.g. "Platinum").
#' @slot Position Character. The position of the electrode ("E.g. Tail").
#' @slot Impedance Numeric of class units. The impedance of the electrode. Unit must be convertible to Ohm.
#' @importFrom cli cli_abort
#' @examples
#' e1 <- newERGElectrode("Ground", "Platinum", "Tail", as_units(100, "Ohm"))
#' e1
#' @export
setClass("ERGElectrode",
         slots = list(
           Name = "character",
           Type = "character",
           Position = "character",
           Impedance = "units"
         ),
         validity = function(object) {
           if (!inherits(object@Impedance, "units")) {
             cli_abort("x Impedance must have class 'units'.")
           }
           if (!units.is.convertible(object@Impedance, "Ohm")) {
             cli_abort("x Impedance unit must be convertible to 'Ohm'.")
           }

           if (object@Position %in% od_str()) {
             object@Position <- "RE"
           }
           if (object@Position %in% os_str()) {
             object@Position <- "LE"
           }
           return(TRUE)
         })

#' @noMd
setMethod("show", "ERGElectrode", function(object) {
  cli_text("{.strong Electrode Details:}")
  cli_text("  {.bold Name:} {object@Name}")
  cli_text("  {.bold Type:} {object@Type}")
  cli_text("  {.bold Position:} {object@Position}")
  cli_text("  {.bold Impedance:} {object@Impedance} {units(object@Impedance)}")
})

#' Create an instance of the ERGElectrode class
#'
#' @description This function creates an instance of the \linkS4class{ERGElectrode} class.
#'
#' @param Name Character. The identifier of the electrode (e.g. "Ground").
#' @param Type Character. The type of the electrode (e.g. "Platinum").
#' @param Position Character. The position of the electrode ("E.g. Tail").
#' @param Impedance Numeric of class units. The impedance of the electrode. Unit must be convertible to Ohm.
#' @return An object of class \code{ERGElectrode}.
#' @seealso \linkS4class{ERGElectrode}
#'
#' @export
newERGElectrode <- function(Name, Type, Position, Impedance) {
  Impedance <- set_units(Impedance, "Ohm")
  new_obj <- new("ERGElectrode", Name = Name, Type = Type, Position = Position, Impedance = Impedance)
  validObject(new_obj)
  return(new_obj)
}

#' @describeIn Get Returns the Name of the electrode. For \linkS4class{ERGElectrode} objects.
#' @exportMethod Name
#' @noRd
setGeneric("Name", function(x) standardGeneric("Name"))
setMethod("Name", "ERGElectrode", function(x) {
  x@Name
})

#' @describeIn Get Returns the type of the electrode. For \linkS4class{ERGElectrode} objects.
#' @exportMethod Type
#' @noRd
setGeneric("Type", function(x) standardGeneric("Type"))
setMethod("Type", "ERGElectrode", function(x) {
  x@Type
})

#' @describeIn Get Returns the Position of the electrode. For \linkS4class{ERGElectrode} objects.
#' @exportMethod Position
#' @noRd
setGeneric("Position", function(x) standardGeneric("Position"))
setMethod("Position", "ERGElectrode", function(x) {
  x@Position
})

#' @describeIn Get Returns the Impedance of the electrode. For \linkS4class{ERGElectrode} objects.
#' @exportMethod Impedance
#' @noRd
setGeneric("Impedance", function(x) standardGeneric("Impedance"))
setMethod("Impedance", "ERGElectrode", function(x) {
  x@Impedance
})

#' @describeIn Set Sets the Name of the electrode. For \linkS4class{ERGElectrode} objects.
#' @exportMethod Name<-
#' @noRd
setGeneric("Name<-", function(x, value) standardGeneric("Name<-"))
setMethod("Name<-", "ERGElectrode", function(x, value) {
  x@Name <- value
  return(x)
})

#' @describeIn Set Sets the type of the electrode. For \linkS4class{ERGElectrode} objects.
#' @exportMethod Type<-
#' @noRd
setGeneric("Type<-", function(x, value) standardGeneric("Type<-"))
setMethod("Type<-", "ERGElectrode", function(x, value) {
  x@Type <- value
  return(x)
})

#' @describeIn Set Sets the Position of the electrode. For \linkS4class{ERGElectrode} objects.
#' @exportMethod Position<-
#' @noRd
setGeneric("Position<-", function(x, value) standardGeneric("Position<-"))
setMethod("Position<-", "ERGElectrode", function(x, value) {
  x@Position <- value
  return(x)
})

#' @describeIn Set Sets the Impedance of the electrode. For \linkS4class{ERGElectrode} objects.
#' @exportMethod Impedance<-
#' @noRd
setGeneric("Impedance<-", function(x, value) standardGeneric("Impedance<-"))
setMethod("Impedance<-", "ERGElectrode", function(x, value) {
  x@Impedance <- value
  return(x)
})
