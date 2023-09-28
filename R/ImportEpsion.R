#' Import ERG data and measurements from an Eosion CSV file
#'
#'
#' This function imports ERG recordings from *.csv files exported from Diagnosys' Epsion and creates an \link[=ERGExam]{ERGExam} object.
#'
#' @param filename Path to the file. Currently supported \code{'.csv'} or \code{'.txt'} files exported from Diagnosys' Epsion that contain at least:
#'   An area with 1) a contents table, 2) a header table, 3) a stimulus table, and 4) a marker table.
#'   This function is only tested on files with the default horizontal table arrangement but should function for files with a vertical arrangement as well.
#'   Behaves similarly to the argument \code{file} from \link[utils:read.table]{utils::read.table()}.
#' @param sep The field separator character. Values on each line of the file are separated by this character.
#'   If sep = "" (the default for read.table), the separator is ‘white space’, which includes spaces, tabs, newlines, or carriage returns.
#' @param Import A list of character vectors specifying which parts of the data to import.
#'   Possible elements are "Raw" - for importing the raw recordings, "Averaged" - for importing the averaged recordings ("Results"), and "Measurements" - for importing the measured markers. Either of "Raw" or "Averaged" must be selected.
#' @param Protocol An S4 object of class \link[Protocol]{Protocol()} or a list thereof.
#' @seealso \linkS4class{ERGExam} \link[Protocol]{Protocol()}
#'
#' @examples
#' \dontrun{
#' # Import a *.csv file exported from Diagnosys' Epsion
#' ERG_Experiment <- ImportEpsion("test.csv")
#' }
#'
#' @return A \linkS4class{ERGExam} object.
#'
#' @importFrom data.table fread
#' @importFrom units as_units
#' @importFrom utils read.csv txtProgressBar setTxtProgressBar
#' @importFrom stats na.exclude
#' @importFrom stringr str_detect str_remove str_trim
#' @importFrom EPhysData newEPhysData
#' @name ImportEpsion
#' @export
#'
ImportEpsion <- function(filename,
                         sep = "\t",
                         Import = list ("Averaged", "Measurements"),
                         Protocol = NULL) {
  message(paste("Importing", filename))

  # Checks

  if (is.null(Protocol)) {
    warning(
      "Provision of the protocol is recommended and may be essential if marker table is not provided or does not include markers for each step and channel of the recording."
    )
  }else{
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
  if (!((contains_raw || contains_averaged) && !(contains_raw && contains_averaged))) {
    stop("Exactly one of 'Raw' and 'Averaged' must be selected for import.")
  }

  if (!file.exists(filename)) {
    stop("File ", filename, " does not exist")
  }
  if (sep != "\t") {
    message("Import using fiels separators other than '\t' not tested.")
  }

  if (!(all(Import %in% list ("Raw", "Averaged", "Measurements")))) {
    stop("'Import' must be one or several of the following: 'Raw','Averaged','Measurements'")
  }

  # load
  if (read.csv(filename,
               header = F,
               sep = sep,
               nrow = 1)[1] == "Contents Table") {

    # get Table of content
    toc <- get_toc(filename, sep = sep)

    if (!all(c("Header Table", "Marker Table", "Stimulus Table", "Data Table") %in% (rownames(toc)))) {
      stop(
        "'Header Table', 'Marker Table', 'Stimulus Table' and 'Data Table' must all be included in the data set (even if they should not be imported). At least one of these is missing."
      )
    }

    # get protocol info
    recording_info <-
      get_content(filename, toc, "Header Table", sep = sep)
    rownames(recording_info)<-recording_info$Parameter
    recording_info$Parameter<-NULL
    if (!all(c("Protocol", "Version", "Date performed","Test method","Animal #","DOB","Gender","Investigator") %in%  rownames(recording_info))) {
      stop(
        "Table of content incomplete. Have these data been exported as anonymous? This is currently unsupported"
      )
    }

    # Get Protocol info
    if (!is.null(Protocol)) {
      tmp1<-sub(" \\[.*", "", recording_info["Protocol","Value"])
      if(is.list(Protocol)){
        idx<-which(tmp1==unlist(lapply(Protocol,function(x){x@Name})))
        if(is.null(idx)){
          stop("Required protocol not in list.")
        }
        if(length(idx)>1){
          stop("Duplicate protocol entry in 'Protocols' list.")
        }
        Protocol<-Protocol[[idx]]
      }else{
        if(Protocol@Name!=tmp){
          stop("Provided protocol is not the required.")
        }
      }
    }

    # Get stimulus information
    stim_info<-get_stim_info(filename, toc, sep)

    if(!is.null(Protocol)){
      for (i in 1:nrow(stim_info)){
        stim_info$Background[i]<-Protocol@Step[[stim_info$Step[i]]]@Adaptation
      }

    }

    # Get Measurements
    if ("Measurements" %in% Import) {
      measurements <- get_measurements(filename, toc, sep)
      measurements$Recording<--1
    } else{
      measurements <-  data.frame()
    }
    if ("Data Table" %in% rownames(toc)) {
      tmp <- toc # modify to only get header of data table
      tmp$Right <- tmp$Left + 5 # maximum width, if results are included
      Data_Header <-
        na.exclude(get_content(filename, tmp, "Data Table", sep = sep))
      Data_Header$Eye <- tmp$Eye
    }

    Metadata <- Data_Header[, c("Step", "Chan","Result")]
    colnames(Metadata)[colnames(Metadata) == "Chan"] <- "Channel"

    Metadata$Eye<-"Unspecified"

    # Define channel types
    if (!is.null(Protocol)) {
      Metadata$Channel_Name <- "Unknown"
      for (i in 1:nrow(Metadata)) {
        Metadata$Channel_Name[i] <-
          Protocol@Step[[Metadata$Step[i]]]@Channels[[Metadata$Channel[i]]]@Name
        Metadata$Eye[i] <-
          Protocol@Step[[Metadata$Step[i]]]@Channels[[Metadata$Channel[i]]]@Eye

        inchanneldesc <-
          str_detect(Metadata$Channel_Name[i], c("OD", "OS", "RE", "LE"))
        if (!any(c("OD", "OS", "RE", "LE") %in% Metadata$Eye[i])) {
          if (sum(inchanneldesc) == 1) {
            Metadata$Eye[i] <- c("OD", "OS", "RE", "LE")[inchanneldesc]
          }
        }
        if (sum(inchanneldesc) == 1) {
          Metadata$Channel_Name[i] <-
            str_remove(Metadata$Channel_Name[i], c("OD", "OS", "RE", "LE")[inchanneldesc])
          Metadata$Channel_Name[i] <-
            str_trim(Metadata$Channel_Name[i])
        }
        if (Metadata$Channel_Name[i] == "") {
          curr_markers <-
            unlist(lapply(Protocol@Step[[Metadata$Step[i]]]@Channels[[Metadata$Channel[i]]]@Markers, function(x) {
              x@Name
            }))
          if (all(c("a", "B") %in% curr_markers)) {
            Metadata$Channel_Name[i] <- "ERG_auto"
          }
          if (all(c("OP1", "OP2", "OP3") %in% curr_markers)) {
            Metadata$Channel_Name[i] <- "OP_auto"
          }
          suppressWarnings({
            if (all(c("N1", "P1") == curr_markers)) {
              Metadata$Channel_Name[i] <- "Flicker_auto"
            }
            if (all(c("N1", "P1", "N2") == curr_markers)) {
              Metadata$Channel_Name[i] <- "VEP_auto"

            }
          })
          if(Metadata$Channel_Name[i]==""){
            Metadata$Channel_Name[i]<-"Unknown"
          }
        }

      }
      Metadata$Recording<-1:nrow(Metadata)
      measurements <- merge(measurements, Metadata, by = c("Step", "Channel"))
      measurements$Eye<-measurements$Eye.y
      measurements$Eye.x<-NULL
      measurements$Eye.y<-NULL
      measurements$Recording<-measurements$Recording.y
      measurements$Recording.x<-NULL
      measurements$Recording.y<-NULL
      measurements$Channel<-measurements$Channel_Name
      measurements$Result<-NULL
      measurements$Channel_Name<-NULL

      Metadata$Channel<-Metadata$Channel_Name
      Metadata$Channel_Name<-NULL

    }else{
      if ("Measurements" %in% Import) {
        tmp <- unique(measurements[, c("Step", "Eye", "Channel","Repeat")])
        for (s in unique(measurements$Step)) {
          for (c in unique(measurements$Channel[measurements$Step == s])) {
            measurements[measurements$Step == s &
                           measurements$Channel == c, "Recording"] <-
              which(Metadata$Step == s & Metadata$Channel == c)
            curr_markers <- measurements[measurements$Step == s &
                                           measurements$Channel == c, "Marker"]
            Metadata$Eye[Metadata$Step == s &
                           Metadata$Channel == c] <-
              tmp$Eye[tmp$Step == s &
                        tmp$Channel == c]
            if (all(c("a", "B") %in% curr_markers)) {
              Metadata$Channel[Metadata$Step == s &
                                 Metadata$Channel == c] <-
                "ERG_auto"

              measurements$Channel[measurements$Step == s &
                                     measurements$Channel == c] <-
                "ERG_auto"
            }
            if (all(c("OP1", "OP2", "OP3") %in% curr_markers)) {
              Metadata$Channel[Metadata$Step == s &
                                 Metadata$Channel == c] <- "OP_auto"

              measurements$Channel[measurements$Step == s &
                                     measurements$Channel == c] <-
                "OP_auto"
            }
            suppressWarnings({
              if (all(c("N1", "P1") == curr_markers)) {
                Metadata$Channel[Metadata$Step == s &
                                   Metadata$Channel == c] <-
                  "ERG_Flicker"

                measurements$Channel[measurements$Step == s &
                                       measurements$Channel == c] <-
                  "ERG_Flicker"
              }
              if (all(c("N1", "P1", "N2") == curr_markers)) {
                Metadata$Channel[Metadata$Step == s &
                                   Metadata$Channel == c] <-
                  "VEP_auto"

                measurements$Channel[measurements$Step == s &
                                       measurements$Channel == c] <-
                  "VEP_auto"
              }
            })
          }
        }
      } else {
        warning(
          "No information on type of recordings can be retrieved, consider including the 'Measurements' table from the raw file."
        )
      }
    }

    if (("Data Table" %in% rownames(toc))) {
      tmp <- toc # modify to only get header of data table
      tmp$Right <- tmp$Left + 5 # maximum width, if results are included
      Data_Header <-
        na.exclude(get_content(filename, tmp, "Data Table", sep = sep))
      Data_Header$Eye <- tmp$Eye
      STEPS = vector("list", nrow(Data_Header))
    }

    # Get Data

    if (("Data Table" %in% rownames(toc)) &&
        any(c("Raw", "Averaged") %in% Import)) {

      pb = txtProgressBar(min = 0, max = dim(Data_Header)[1], initial = 0)
      for (i in 1:dim(Data_Header)[1]){
        setTxtProgressBar(pb,i)
        if(as.numeric(Data_Header$Chan[i])==T){ # get time trace
          TimeTrace<-(na.exclude(fread(filename,
                                       select = Data_Header[i,"Column"],
                                       nrows = toc["Data Table","Bottom"]-toc["Data Table","Top"],
                                       skip = toc["Data Table","Top"]-1,
                                       data.table = F,
                                       header = F)))[,1]
          TimeUnit<-fread(filename,
                          select = Data_Header[i,"Column"],
                          nrows = 1,
                          skip = toc["Data Table","Top"]-2,
                          data.table = F,
                          header = F)[1,1]

          TimeUnit<-gsub("[\\(\\)]", "", regmatches(TimeUnit, gregexpr("\\(.*?\\)", TimeUnit))[[1]])
          TimeTrace<-as_units(TimeTrace,TimeUnit)
          TimeTrace<-TimeTrace[!is.na(TimeTrace)]
        }

        if (("Result" %in% colnames(Data_Header)) &&
            ("Averaged" %in% Import)) {

          # get Averages / "Results"
          resulttrace<-(na.exclude(fread(filename,
                                         select = Data_Header[i,"Column.1"],
                                         nrows = toc["Data Table","Bottom"]-toc["Data Table","Top"],
                                         skip = toc["Data Table","Top"]-1,
                                         data.table = F,
                                         header = F)))[,1]
          if(all(resulttrace==0)){
            stop("Raw trace is empty. Re-export table or run ImportEpsionMeasures instead, to only import measures.")
          }

          resultunit<-fread(filename,
                            select = Data_Header[i,"Column.1"],
                            nrows = 1,
                            skip = toc["Data Table","Top"]-2,
                            data.table = F,
                            header = F)[1,1]

          resultunit<-gsub("[\\(\\)]", "", regmatches(resultunit, gregexpr("\\(.*?\\)", resultunit))[[1]])
          resulttrace<-as_units(resulttrace,resultunit)
          resulttrace<-resulttrace[!is.na(resulttrace)]

          STEPS[[i]] <-
            newEPhysData(
              Data = resulttrace,
              TimeTrace = TimeTrace
            )
        }

        if ("Trials" %in% colnames(Data_Header) &&
            ("Raw" %in% Import)) {
          trialtraces<-(na.exclude(fread(filename,
                                         select = c((Data_Header[i,"Column.1"]+1):(Data_Header[i,"Column.1"]+Data_Header[i,"Trials"])),
                                         nrows = toc["Data Table","Bottom"]-toc["Data Table","Top"],
                                         skip = toc["Data Table","Top"]-1,
                                         data.table = F,
                                         header = F)))
          trialunits<-fread(filename,
                            select = c((Data_Header[i,"Column.1"]+1):(Data_Header[i,"Column.1"]+Data_Header[i,"Trials"])),
                            nrows = 1,
                            skip = toc["Data Table","Top"]-2,
                            data.table = F,
                            header = F)

          trialunits<-unique(gsub("[\\(\\)]", "", regmatches(trialunits, gregexpr("\\(.*?\\)", trialunits))[[1]]))
          if (length(trialunits) > 1) {
            stop("Error importing individual trials. distict units detected.")
          }

          trialtraces <-
            as.matrix(trialtraces[apply(trialtraces, 1, function(x) {
              all(!is.na(x))
            }), ])
          trialtraces<-as_units(trialtraces,trialunits)
          STEPS[[i]] <-
            newEPhysData(
              Data = trialtraces,
              TimeTrace = TimeTrace
            )
        }
      }
      close(pb)
    }

    if ("Measurements" %in% Import) {
      # Transfer Group info to recording_info table
      if(is.null(unique(measurements$Group))){
        measurements$Group<-""
      }
      recording_info["Group", 1] <- unique(measurements$Group)

      # Drop further duplicated info from measurements
      measurements <-
        measurements[, c("Recording", "Marker", "Voltage", "Time")]
      colnames(measurements)[colnames(measurements)=="Marker"]<-"Name"
    }

    DOB <-
      as.Date(as.character(recording_info["DOB", 1]), format =    "%d/%m/%Y")

    ExamDate <-
      as.POSIXct(strptime(recording_info["Date performed", 1], format =
                            "%d/%m/%Y %H:%M:%S"))
    ExamDate <-
      as.POSIXct.numeric(as.numeric(ExamDate), origin = "1970-01-01 00:00.00 UTC")

    newERGExam(
      Data = STEPS,
      Metadata = Metadata,
      Stimulus = stim_info,
      Measurements = measurements,
      ExamInfo=list(ProtocolName = recording_info["Protocol", 1],
                    Version = recording_info["Version", 1],
                    ExamDate = as.POSIXct(strptime(recording_info["Date performed", 1], format =
                                                     "%d/%m/%Y %H:%M:%S")),
                    Filename = filename,
                    RecMode = recording_info["Test method", 1],
                    Investigator = recording_info["Investigator", 1]
      ),
      SubjectInfo = list(
        Subject = recording_info["Animal #", 1],
        DOB = as.Date(as.character(recording_info["DOB", 1]), format =    "%d/%m/%Y"),
        Gender = recording_info["Gender", 1],
        Group = unique(measurements$Group)
      )
    )
  }else{
    stop(paste(filename, " does not begin with a table of content."))
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
    select = c("Table", "Left", "Right", "Top", "Bottom"),
    encoding = "UTF-8"
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

#' @importFrom data.table fread
get_content <- function(filename, toc, what, sep = "\t") {
  #Sys.setlocale("LC_ALL", "C")
  recording_info <- fread(
    filename,
    sep = sep,
    select = toc[what, "Left"]:toc[what, "Right"],
    nrows = toc[what, "Bottom"] - toc[what, "Top"] +
      1,
    skip = toc[what, "Top"] - 1,
    data.table = F,
    header = F,
    encoding = "UTF-8"
  )

  if (!(what %in% (rownames(toc)))) {
    stop("'", what, "' not fount in Table of Content.")
  }
  tmp <-  fread(
    filename,
    sep = sep,
    select = toc[what, "Left"]:toc[what, "Right"],
    nrows = 1,
    skip = toc[what, "Top"] -
      3,
    data.table = F,
    header = F,
    encoding = "UTF-8"
  )

  tmp <- make.unique(make.names(iconv(tmp, "ASCII//TRANSLIT", sub = '')))

  colnames(recording_info)<-tmp

  return(recording_info)
}


get_stim_info <- function(filename, toc, sep) {
  stim_info <-
    as.data.frame(get_content(filename, toc, "Stimulus Table", sep = sep))
  colnames(stim_info)[stringr::str_detect(colnames(stim_info),"cd.")]<-"Intensity"
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

  stim_info$Description<-enc2utf8(stim_info$Description)

  return(stim_info)
}

#' @importFrom units as_units
get_measurements <- function(filename, toc, sep) {
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

  TimeUnit <-
    colnames(measurements)[colnames(measurements) == "ms"]
  colnames(measurements)[colnames(measurements) == "ms"] <- "Time"
  measurements$Time<-as_units(measurements$Time,TimeUnit)

  voltageunit <-
    colnames(measurements)[colnames(measurements) == "uV"]
  colnames(measurements)[colnames(measurements) == "uV"] <-
    "Voltage"
  measurements$Voltage<-as_units(measurements$Voltage,voltageunit)
  return(measurements)
}

