#' Display Changelog for an ERGExam Object
#'
#' This method returns the formatted changelog for an ERGExam object. It uses
#' the cli package to format the output. If no changelog entries are present,
#' an informational message is displayed.
#'
#' @param X An object of class \code{ERGExam}.
#' @param return should the changelog be returned as a variable?
#'
#' @return Invisibly returns the original \code{ERGExam} object.
#'
#' @examples
#' \dontrun{
#'   # Assuming exam is an instance of ERGExam with a changelog slot
#'   Changelog(exam)
#' }
#'
#' @export
#' @importFrom cli cli_text cli_alert_info col_green
setGeneric("Changelog", function(X, return = F) standardGeneric("Changelog"))
#' @noMd
setMethod("Changelog", "ERGExam",
          function(X, return = F) {
            if (length(X@Changelog) == 0) {
              cli::cli_alert_info("No changelog entries available.")
            } else {
              # Display a colored header (cyan)
              cli::cli_text("{col_green('Changelog:')}")
              # Define a regex pattern to capture the date/time and the message parts
              pattern <- "^([0-9]{8} [0-9]{2}:[0-9]{2}:[0-9]{2})\\s*-\\s*(.*)$"
              for (entry in X@Changelog) {
                # Remove leading/trailing whitespace
                entry <- trimws(entry)
                matches <- regexec(pattern, entry)
                capture <- regmatches(entry, matches)
                if (length(capture[[1]]) > 0) {
                  datetime <- capture[[1]][2]
                  msg <- capture[[1]][3]
                  # Format date/time in blue and display the message normally
                  cli::cli_text("{col_green({datetime})} {msg}")
                } else {
                  # Fallback: if entry doesn't match expected format, print it as is
                  cli::cli_text(entry)
                }
              }
            }
            if (return) {X@Changelog}
          }
)

#' Log a Change for an ERGExam Object
#'
#' This internal method appends a new change entry to the \code{changelog} slot of an
#' \code{ERGExam} object. It checks if the function was called directly by the user
#' (i.e. \code{sys.nframe() == 1}). If true, it constructs a new entry with the current
#' date and time (formatted as "YYYYMMDD HH:MM:SS") followed by the supplied message and
#' appends it to the changelog.
#'
#' @param X An object of class \code{ERGExam}.
#'
#' @return Invisibly returns the modified \code{ERGExam} object.
#'
#' @keywords internal
#'
setGeneric("LogChange", function(X) standardGeneric("LogChange"))

#' @describeIn LogChange Append a new log entry to the changelog slot if called directly by the user.
setMethod("LogChange", "ERGExam",
          function(X) {
            if (sys.nframe() == 5) {
              # Format the current date/time as "YYYYMMDD HH:MM:SS"
              datetime <- format(Sys.time(), "%Y%m%d %H:%M:%S")
              # Construct the new log entry
              call_str<-deparse(sys.call(1))
              new_entry <- paste(datetime, "-", call_str)
              # Append the new entry to the changelog slot
              X@Changelog <- c(X@Changelog, new_entry)
            }
            return(X)
          }
)
