#' @describeIn ImportEspion Read the Stimulus information stored in an Espion Exam file
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' # Import stimulus information from a *.csv file exported from the Diagnosys Espion™ software.
#' ImportEspionInfo("test.txt")
#' }
#'
#' @return For ImportEspionStimTab: A \link[data.frame]{data.frame()} containing the stimulus information.
ImportEspionStimTab <- function(filename,
                         sep = "\t",
                         Protocol = NULL) {
  # Checks
  if (!is.null(Protocol)) {
    if (!inherits(Protocol, "ERGProtocol")) {
      if (is.list(Protocol)) {
        if (!(all(unlist(lapply(Protocol, function(x) {
          inherits(x, "ERGProtocol")
        }))))) {
          stop("'Protocol' must be an object of class 'Protocol' or a list thereof.")
        }
      } else{
        stop("'Protocol' must be an object of class 'Protocol' or a list thereof.")
      }
    }
  }

  if (!file.exists(filename)) {
    stop("File ", filename, " does not exist")
  }
  if (sep != "\t") {
    message("Import using fiels separators other than '\t' untested.")
  }

  # load
  if (read.csv(filename,
               header = F,
               sep = sep,
               nrow = 1)[[1]] != "Contents Table") {
    stop(paste(filename, " does not begin with a table of content."))
  }
  # get Table of content
  toc <- get_toc(filename, sep = sep)

  if (!all(c(
    "Header Table",
    "Stimulus Table"
  ) %in% (rownames(toc)))) {
    stop(
      "'Header Table' and 'Stimulus Table' must be included in the data set (even if they should not be imported). At least one of these is missing."
    )
  }

  # get protocol info
  recording_info <- ImportEspionInfo(filename)
  if (!"Protocol" %in%  names(recording_info)) {
    stop("Table of content incomplete. Does not contain protocol name.")
  }

  # Get Protocol info
  if (!is.null(Protocol)) {
    tmp1 <- sub(" \\[.*", "", recording_info$Protocol)
    if (is.list(Protocol)) {
      idx <- which(tmp1 == unlist(lapply(Protocol, function(x) {
        x@Name
      })))
      if (length(idx) == 0) {
        stop("Required protocol not in list.")
      }
      if (length(idx) > 1) {
        stop("Duplicate protocol entry in 'Protocols' list.")
      }
      Protocol <- Protocol[[idx]]
    } else{
      if (Protocol@Name != tmp1) {
        stop("Provided protocol is not the required.")
      }
    }
  }

  # Get stimulus information
  stim_info <- get_stim_info(filename, toc, sep)

  if (!is.null(Protocol)) {
    stim_info$PreTriggerTime<-as_units(NA,"s")
    stim_info$PostTriggerTime<-as_units(NA,"s")
    stim_info$InterSweepDelay<-as_units(NA,"s")
    stim_info$Baselinerange<-as_units(NA,"s")
    for (i in 1:nrow(stim_info)) {
      stim_info$Background[i] <-
        Protocol@Step[[stim_info$Step[i]]]@Adaptation
      stim_info$PreTriggerTime[i] <-
        str_to_unit(Protocol@Step[[stim_info$Step[i]]]@PreTriggerTime)

      stim_info$PostTriggerTime[i] <-
        str_to_unit(Protocol@Step[[stim_info$Step[i]]]@PostTriggerTime)

      stim_info$InterSweepDelay[i] <-
        str_to_unit(Protocol@Step[[stim_info$Step[i]]]@InterSweepDelay)
      stim_info$Driftremoval[i] <-
        Protocol@Step[[stim_info$Step[i]]]@Driftremoval
      stim_info$Baselineenabled[i] <-
        Protocol@Step[[stim_info$Step[i]]]@Baselineenabled
      stim_info$Baselinerange[i] <-
        str_to_unit(Protocol@Step[[stim_info$Step[i]]]@Baselinerange)
    }
  }

  return(stim_info)
}
