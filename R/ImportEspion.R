#' Import ERG data and measurements from an Espion CSV file
#'
#'
#' This function imports ERG recordings from *.csv files exported from the Diagnosys Espion™ software and creates an \link[=ERGExam]{ERGExam} object.
#'
#' @param filename Path to the file. Currently supported \code{'.csv'} or \code{'.txt'} files exported from the Diagnosys Espion™ software that contain at least:
#'   An area with 1) a contents table, 2) a header table, 3) a stimulus table, and 4) a marker table.
#'   This function is only tested on files with the default horizontal table arrangement but should function for files with a vertical arrangement as well.
#'   Behaves similarly to the argument \code{file} from \link[utils:read.table]{utils::read.table()}.
#' @param sep The field separator character. Values on each line of the file are separated by this character.
#'   If sep = "" (the default for read.table), the separator is ‘white space’, which includes spaces, tabs, newlines, or carriage returns.
#' @param Import A list of character vectors specifying which parts of the data to import.
#'   Possible elements are "Raw" - for importing the raw recordings, "Averaged" - for importing the averaged recordings ("Results"), and "Measurements" - for importing the measured markers. Either of "Raw" or "Averaged" must be selected.
#' @param Protocol An S4 object of class \link[Protocol]{ERGProtocol()} or a list thereof.
#' @inheritParams Where
#' @seealso \linkS4class{ERGExam} \link[Protocol]{ERGProtocol()}
#'
#' @examples
#' \dontrun{
#' # Import a *.csv file exported from the Diagnosys Espion™ software.
#' ERG_Experiment <- ImportEspion("test.csv")
#' }
#'
#' @return For ImportEspion: A \linkS4class{ERGExam} object.
#'
#' @importFrom data.table fread
#' @importFrom units as_units
#' @importFrom utils read.csv txtProgressBar setTxtProgressBar
#' @importFrom stats na.exclude
#' @importFrom stringr str_detect str_remove str_trim str_replace_all regex
#' @importFrom EPhysData newEPhysData
#' @name ImportEspion
#' @export
ImportEspion <- function(filename,
                         sep = "\t",
                         Import = list ("Averaged", "Measurements"),
                         Protocol = NULL,
                         where = NULL) {
  #(NA.EXCLUDE REMOVED!!!! DOES THAT WORK??)
  message(paste("Importing", filename))

  # Checks

  if (!file.exists(filename)) {
    stop("File ", filename, " does not exist")
  }
  if (sep != "\t") {
    message("Import using fiels separators other than '\t' untested.")
  }

  if (is.null(Protocol)) {
  } else{
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

  contains_raw <- "Raw" %in% Import
  contains_averaged <- "Averaged" %in% Import
  if (!((contains_raw ||
         contains_averaged) &&
        !(contains_raw && contains_averaged))) {
    stop("Exactly one of 'Raw' and 'Averaged' must be selected for import.")
  }

  if (!(all(Import %in% list ("Raw", "Averaged", "Measurements")))) {
    stop("'Import' must be one or several of the following: 'Raw','Averaged','Measurements'")
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

  if (!all(c("Header Table",
             "Marker Table",
             "Stimulus Table",
             "Data Table") %in% (rownames(toc)))) {
    stop(
      "'Header Table', 'Marker Table', 'Stimulus Table' and 'Data Table' must all be included in the data set (even if they should not be imported). At least one of these is missing."
    )
  }


  # get protocol info
  recording_info <- ImportEspionInfo(filename)
  if (!all(
    c(
      "Protocol",
      "Version",
      "Dateperformed",
      "Testmethod",
      "Animal",
      "DOB"
    ) %in%  names(recording_info)
  )) {
    stop(
      "Table of content incomplete. Have these data been exported as anonymous? This is currently unsupported"
    )
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
  stim_info <- ImportEspionStimTab(filename, sep, Protocol)

  # get Metadata
  Metadata <-
    ImportEspionMetadata(filename, sep = sep, Protocol = Protocol)

  Metadata$Channel_idx<-Metadata$Channel
  Metadata$Channel<-Metadata$Channel_Name

  # prepare for loading only requested relevant
  if(!is.null(where)){
    where.idx <- Where.generic(Metadata,
                               nrow(Metadata),
                               stim_info,
                               where = where)
    if(length(where.idx)==0){
      stop("'where' selection does not return any data for the given file. Try 'ImportEspionMetadata()' and 'ImportEspionStimTab()' to find available key-value pairs for 'where")
    }
  }else{
    where.idx<-1:nrow(Metadata)
  }

  # Get Data
  if (("Data Table" %in% rownames(toc))) {
    tmp <- toc # modify to only get header of data table
    tmp$Right <-
      tmp$Left + 5 # maximum width, if results are included
    Data_Header <-
      na.exclude(get_content(filename, tmp, "Data Table", sep = sep))
    Data_Header$Eye <- tmp$Eye
    rownames(Data_Header)<-NULL
    #Data_Header<-Data_Header[where.idx,]
    STEPS = vector("list", nrow(Data_Header))
  }

  if (("Data Table" %in% rownames(toc)) &&
      any(c("Raw", "Averaged") %in% Import)) {
    pb = txtProgressBar(min = 0,
                        max = dim(Data_Header)[1],
                        initial = 0)
    for (i in 1:dim(Data_Header)[1]) {
      setTxtProgressBar(pb, i)
      tryCatch({
        if (as.numeric(Data_Header$Chan[i]) == 1) {
          #if(i==1 || (Data_Header$Step[i]!=Data_Header$Step[i-1])){ # only the first for new step contains a time frame
          # get time trace
          TimeTrace <- get_trace(filename, toc, Data_Header, i, "TimeTrace")
          #}
        }
        if (i %in% where.idx){ # only import, if selected by where
          if (("Result" %in% colnames(Data_Header)) &&
              ("Averaged" %in% Import)) {
            # get Averages / "Results"
            resulttrace <-
              get_trace(filename, toc, Data_Header, i, "ResultTrace")

            STEPS[[i]] <-
              newEPhysData(Data = resulttrace,
                           TimeTrace = TimeTrace)
          }

          if ("Trials" %in% colnames(Data_Header) &&
              ("Raw" %in% Import)) {
            trialtraces <- get_trace(filename, toc, Data_Header, i, "TrialTrace")
            STEPS[[i]] <-
              newEPhysData(Data = trialtraces,
                           TimeTrace = TimeTrace)
          }
        }
      }, error = function (e) {
        stop(
          "Importing Data failed for ",
          basename(filename),
          " with error message: ",
          e,
          " for Step '",
          Data_Header$Step[i],
          "' Channel '",
          Data_Header$Channel[i],
          "' Repeat '",
          Data_Header$Repeat[i],
          "'."
        )
      })
    }
    close(pb)
  }
  STEPS<-STEPS[where.idx]

  #subset Metadata and stim_info to required
  Metadata<-Metadata[where.idx,]
  rownames(Metadata)<-NULL
  stim_info<-stim_info[stim_info$Step %in% Metadata$Step,]

  # Get Measurements
  if ("Measurements" %in% Import) {
    measurements <- get_measurements(filename, toc, sep)
    measurements$Relative <- NA
    md.tmp<-Metadata
    md.tmp$Recording <- 1:nrow(md.tmp)
    md.tmp$Channel<-md.tmp$Channel_idx
    measurements <-
      merge(measurements,
            md.tmp[, c("Step", "Channel", "Repeat", "Eye", "Recording","Channel_Name")],
            by = c("Step", "Channel", "Repeat", "Eye"))
    measurements$Channel<-measurements$Channel_Name
    measurements$Channel_Name<-NULL
    measurements$Channel_idx<-NULL
    rm(md.tmp)

    if (all(is.null(unique(measurements$Group)))) {
      measurements$Group <- NULL
    }
    if(length(unique(measurements$Group))==1){
      recording_info$Group <- unique(measurements$Group)
    }

    colnames(measurements)[colnames(measurements) == "Marker"] <-
      "Name"

    M <- newERGMeasurements(measurements, update.empty.relative = T)

  } else{
    M <- newERGMeasurements(data.frame(Channel=character(),Name=character(),Recording=numeric(),Time=numeric(),Relative=character()))
  }

  # Dump empty traces

  EmptyTraces<-unlist((lapply(STEPS, function(x) {
    sum(drop_units(x@Data))
  })))==0

  STEPS<-STEPS[!EmptyTraces]
  Metadata<-Metadata[!EmptyTraces,]
  rownames(Metadata)<-NULL
  stim_info<-stim_info[stim_info$Step %in% unique(Metadata$Step),]
  rownames(stim_info)<-NULL
  Metadata$Channel_Name<-NULL
  Metadata$Channel_idx<-NULL

  # Subject and Exam data

  DOB <-
    as.Date(as.character(recording_info$DOB), format =    "%d/%m/%Y", origin = "1970-01-01")

  ExamDate <-
    as.POSIXct(strptime(recording_info$Dateperformed, format =
                          "%d/%m/%Y %H:%M:%S"))
  ExamDate <-
    as.POSIXct.numeric(as.numeric(ExamDate), origin = "1970-01-01 00:00.00 UTC")


  # data strangely pooled, every second from other channel.
  # is this in file or import mistake?
  #   how to make fail proof?
  # ggEPhysData(Subset(STEPS[[5]],Trials=seq(1,50,2)))
  # Warnmeldungen:
  #   1: In GetData(x, Raw = Raw) :
  #   Averaging function function (x) {    x} returns more than a single value per time point. Has a valid function been set? Try e.g.: AverageFunction(X)<-mean
  # 2: In ggEPhysData(Subset(STEPS[[5]], Trials = seq(1, 50, 2))) :
  #   No averaging function set.
  # > ggEPhysData(Subset(STEPS[[5]],Trials=seq(2,50,2)))


  # return the object
  newERGExam(
    Data = STEPS,
    Metadata = Metadata,
    Stimulus = stim_info,
    Measurements = M,
    ExamInfo = list(
      ProtocolName = recording_info$Protocol,
      Version = recording_info$Version,
      ExamDate = as.POSIXct(
        strptime(recording_info$Dateperformed, format =
                   "%d/%m/%Y %H:%M:%S")
      ),
      Filename = filename,
      RecMode = recording_info$Testmethod,
      Investigator = recording_info$Investigator
    ),
    SubjectInfo = list(
      Subject = recording_info$Animal,
      DOB = as.Date(as.character(recording_info$DOB), format =    "%d/%m/%Y", origin = "1970-01-01"),
      Gender = recording_info$Gender,
      Group = unique(measurements$Group)
    )
  )
}
