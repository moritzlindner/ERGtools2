#' Import ERG measurements from CSV file
#'
#' `r lifecycle::badge("stable")` \cr
#' This function imports ERG recordings from *.csv files exported from Diagnosys' Epsion and creates an \link[=ERGExam]{ERGExam} object.
#' @param filename Path to file. Currently supported *.csv files exported from Diagnosys' Epsion that contain at least: An area with 1) a contents table, 2) a header table, 3) a stimulus table and 4) a marker table. This function in only tested on files with the default horizontal table arrangement but should function for files with a vertical arrangement as well. Behaves otherways similar to the argument \code{file} from \link[utils:read.table]{utils::read.table()}.
#' @param sep The field separator character. Values on each line of the file are separated by this character. If sep = "" (the default for read.table) the separator is ‘white space’, that is one or more spaces, tabs, newlines or carriage returns.
#' @seealso \linkS4class{ERGExam}, \link[utils:read.table]{utils::read.table()}
#' @examples
#' \dontrun{
#' # import a *.csv files exported from Diagnosys' Epsion
#' ERG_Experiment<-ImportEpsionMeasures("test.csv")
#' }
#' @return A \linkS4class{ERGExam} object
#' @importFrom data.table fread
#' @importFrom utils read.csv
#' @importFrom ggBiosci si_to_exponent
#' @name ImportEpsionMeasures
#' @export
#'
ImportEpsionMeasures <- function(filename,
                                 sep = "\t") {
  message(paste("Importing", filename))
  if (!file.exists(filename)) {
    stop("File ", filename, " does not exist")
  }
  if (sep != "\t") {
    message("Import using fiels separators other than '\t' not tested.")
  }
  if (read.csv(filename,
               header = F,
               sep = sep,
               nrow = 1)[1] == "Contents Table") {
    # get Table of content
    toc <- get_toc(filename, sep = sep)

    # get protocol info
    recording_info <-
      get_content(filename, toc, "Header Table", sep = sep)

    # Get stimulus information
    stim_info <-
      get_content(filename, toc, "Stimulus Table", sep = sep)
    colnames(stim_info)[colnames(stim_info) == "cd.s.m."] <-
      "Intensity"
    stim_info$Background <- NA
    stim_info$Background[grepl("LA", stim_info[, "Description"], fixed = TRUE, useBytes = TRUE)] <-
      "LA"
    stim_info$Background[grepl("DA", stim_info[, "Description"], fixed = TRUE, useBytes = TRUE)] <-
      "DA"
    stim_info$Type <- NA
    stim_info$Type[stim_info$Background == "DA"] <- "Flash"
    stim_info$Type[grepl("Flash",
                         stim_info[, "Description"],
                         fixed = TRUE,
                         useBytes = TRUE)] <- "Flash"
    stim_info$Type[grepl("Flicker",
                         stim_info[, "Description"],
                         fixed = TRUE,
                         useBytes = TRUE)] <- "Flicker"

    # Get Measurements
    measurements <-
      get_content(filename, toc, "Marker Table", sep = sep)
    measurements <-
      measurements[, c("Group", "S", "C", "R", "Eye", "Name.1", "uV", "ms")]
    colnames(measurements)[colnames(measurements) == "S"] <- "Step"
    colnames(measurements)[colnames(measurements) == "C"] <-
      "Channel"
    colnames(measurements)[colnames(measurements) == "Name.1"] <-
      "Marker"
    colnames(measurements)[colnames(measurements) == "R"] <-
      "Repeat"

    timeunit <-
      colnames(measurements)[colnames(measurements) == "ms"]
    measurements$ms <-
      measurements$ms * si_to_exponent(substr(timeunit, 1, 1))
    colnames(measurements)[colnames(measurements) == "ms"] <- "Time"

    voltageunit <-
      colnames(measurements)[colnames(measurements) == "uV"]
    measurements$uV <-
      measurements$uV * si_to_exponent(substr(voltageunit, 1, 1))
    colnames(measurements)[colnames(measurements) == "uV"] <-
      "Voltage"

    # Define Channels
    channels <-
      character(l = as.numeric(as.character(recording_info["Channels",])))
    tmp <- unique(measurements[, c("Channel", "Eye")])
    channels[tmp$C] <- tmp$Eye

    # Transfer Group info to recording_info table

    recording_info["Group", 1] <- unique(measurements$Group)

    # Drop further duplicated info from measurements
    measurements <-
      measurements[, c("Step", "Channel", "Repeat", "Marker", "Voltage", "Time")]
    DOB <-
      as.Date(as.character(recording_info["DOB", 1]), format =    "%d/%m/%Y")

    ExamDate <-
      as.POSIXct(strptime(recording_info["Date performed", 1], format =
                            "%d/%m/%Y %H:%M:%S"))
    ExamDate <-
      as.POSIXct.numeric(as.numeric(ExamDate), origin = "1970-01-01 00:00.00 UTC")

    ERGExam(
      Measurements = measurements,
      Channels = channels,
      Stimulus = stim_info,
      ProtocolName = recording_info["Protocol", 1],
      Version = recording_info["Version", 1],
      ExamDate = as.POSIXct(strptime(recording_info["Date performed", 1], format =
                                       "%d/%m/%Y %H:%M:%S")),
      Filename = filename,
      RecMode = recording_info["Test method", 1],
      Patient = recording_info["Animal #", 1],
      DOB = as.Date(as.character(recording_info["DOB", 1]), format =    "%d/%m/%Y"),
      Gender = recording_info["Gender", 1],
      Investigator = recording_info["Investigator", 1]
    )
  } else{
    stop(paste(filename, " does not contain a Contents Table in first cell."))
  }
}


get_toc <- function(filename, sep = "\t") {
  toc <- fread(
    filename,
    sep = sep,
    data.table = F,
    skip = 1,
    header = T,
    blank.lines.skip = T,
    select = c("Table", "Left", "Right", "Top", "Bottom")
  )

  HeaderTabPos <-
    which(toc$Table == "Header Table") # if vertical table, chop off after end of Header
  if (length(HeaderTabPos) > 1) {
    # "Header Table" found twice, so its a vertical table
    EndOfTOC <-
      min(which(toc$Table == "")[which(toc$Table == "") > HeaderTabPos[1]]) # this is the first blank line after TOC
    toc <- toc[1:EndOfTOC,]
  }

  toc = toc[!toc$Table == "",]
  tmp <- toc$Table
  toc$Table <- NULL
  toc <-
    data.frame(apply(toc, 2, function(x)
      as.numeric(as.character(x))))
  rownames(toc) <- tmp

  # check if Header and Stimulus Info present
  if (!sum(rownames(toc) %in% c("Header Table", "Stimulus Table")) == 2) {
    stop("Table of content incomplete.")
  }

  return(toc)
}

get_content <- function(filename, toc, what, sep = "\t") {
  Sys.setlocale("LC_ALL", "C")
  recording_info <- fread(
    filename,
    sep = sep,
    select = toc[what, "Left"]:toc[what, "Right"],
    nrows = toc[what, "Bottom"] - toc[what, "Top"] +
      1,
    skip = toc[what, "Top"] - 1,
    data.table = F,
    header = F
  )

  if (!(what %in% (rownames(toc)))) {
    stop("'", what, "' not fount in Table of Content.")
  }
  colnames(recording_info) <- make.unique(make.names(
    fread(
      filename,
      sep = sep,
      select = toc[what, "Left"]:toc[what, "Right"],
      nrows = 1,
      skip = toc[what, "Top"] -
        3,
      data.table = F,
      header = F
    )
  ))
  try({
    rownames(recording_info) <- recording_info[, 1]
    recording_info[, 1] <- NULL
  }, silent = T)
  return(recording_info)
}
