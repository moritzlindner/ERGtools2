validERGExam <- function(object) {
  warning("Storing EPsion Repeated Results not yet implemented")
  # if (!is.list(object@Data)) {
  #   stop("Data must be a list.")
  # }

  # Check if the data frame has the required columns
  required_columns <- c("Step", "Eye", "Channel")
  if (!all(required_columns %in% names(object@Metadata))) {
    stop("Metadata provided is not in correct format. Must be a data.frame with the columns 'Step', 'Eye', and 'Channel'.")
  }

  # Check if the data types of the columns are character
  # column_types <- sapply(object@Metadata, class)
  # if (!all(column_types[required_columns] == "character")) {
  #   stop("Metadata provided is not in correct format. All columns must be of type 'character'.")
  # }

  # Check if Data and MetaData are of the same length
  # if (nrow(object@Metadata) != length(object@Data)) {
  #   stop("Metadata must have one row per each list entry in data.")
  # }

  # Check if Eye entries valid
  if (!all(unique(object@Metadata$Eye) %in% c("RE","LE"))){
    stop("Only 'RE' and 'LE' are allowed as eye identifiers")
  }

  # Rejected (inEPhysRaw) - must be the same across all channels within one eye
  for (s in unique(object@Metadata$Step)) {
    for (e in unique(object@Metadata$Eye[object@Metadata$Step == s])) {
      ids.equal <-
        which(object@Metadata$Step == s & object@Metadata$Eye == e)
      feature.list <- lapply(object@Data[ids.equal], Rejected)
      feature.df <- tryCatch(
        as.data.frame(do.call(cbind, feature.list)),
        error = function(e) {
          stop("'Rejected' slots are filled with vectors of unequal length in step ",
               s,
               "eye",
               e,
               ".")
        }
      )
      if (!all(apply(feature.df, 1, function(x) {
        if (length(x) > 1) {
          var(x) == 0
        } else{
          TRUE
        }
      }))) {
        stop("'Rejected' vectors are not identical across channels in step ",
             s,
             "eye",
             e,
             ".")
      }
    }
  }

  # filter.fx (inEPhysRaw) - must be the same across all eyes within one channel and step
  for (s in unique(object@Metadata$Step)) {
    for (c in unique(object@Metadata$Channel[object@Metadata$Step == s])) {
      ids.equal <-
        which(object@Metadata$Step == s & object@Metadata$Channel == c)
      feature.list <-
        lapply(object@Data[ids.equal], function(x) {
          deparse1(FilterFunction(x))
        })
      feature.df <- tryCatch(
        as.data.frame(do.call(cbind, feature.list)),
        error = function(e) {
          stop(
            "'filter.fx' slots are filled with vectors of unequal length in step ",
            s,
            "channel",
            c,
            "."
          )
        }
      )
      if (!all(apply(feature.df, 1, function(x) {
        length(unique(x)) == 1
      }))) {
        stop(
          "'filter.fx' vectors are not identical across both eyes in step ",
          s,
          "channel",
          c,
          "."
        )
      }
    }
  }

  # average.fx (inEPhysRaw) - must be the same across all eyes within one channel and step
  for (s in unique(object@Metadata$Step)) {
    for (c in unique(object@Metadata$Channel[object@Metadata$Step == s])) {
      ids.equal <-
        which(object@Metadata$Step == s & object@Metadata$Channel == c)
      feature.list <-
        lapply(object@Data[ids.equal], function(x) {
          deparse1(AverageFunction(x))
        })
      feature.df <- tryCatch(
        as.data.frame(do.call(cbind, feature.list)),
        error = function(e) {
          stop(
            "'average.fx' slots are filled with vectors of unequal length in step ",
            s,
            "channel",
            c,
            "."
          )
        }
      )
      if (!all(apply(feature.df, 1, function(x) {
        length(unique(x)) == 1
      }))) {
        stop(
          "'average.fx' vectors are not identical across both eyes in step ",
          s,
          "channel",
          c,
          "."
        )
      }
    }
  }


  # Averaged slot
  if (length(object@Averaged) != 0) {
    if (length(object@Averaged) != length(object@Data)) {
      stop("Length of 'Averaged' slot does not match that of 'Data'.")
    }
    if (!all(unlist(lapply(object@Averaged, function(x) {
      inherits(x, "EPhysData")
    })))) {
      stop("'Averaged' slot must be a list of EPhysData objects.")
    }

   val_dim<-unique(unlist(lapply(Steps_AVG,function(x){dim(x)[2]})))
   if (!(length(val_dim) == 1 & val_dim == 1)) {
     stop("'Averaged' slot must be a list of EPhysData objects with no repeated measurements (i.e. one single data column).")
   }

    for (i in length(object)){
      if(length(object@Averaged[[i]])!=length(object@Data[[i]])){
        stop("Length of averaged data for ",Metadata(object)[i,]," does not match that of corresponding raw data.")
      }
    }
  }

  # Measurements slot
  if (length(object@Measurements) != 0) {
    required_columns <- c("Recording","Name", "Time", "Voltage")
    if (!all(required_columns %in% names(object@Measurements))) {
      stop("Measurements provided are not in correct format. Must be a data.frame with the columns 'Recording', 'Name', 'Time', and 'Voltage'.")
    }
    if (!all(object@Measurements$Recording %in% 1:nrow(Metadata(object)))) {
      stop("'Recording'(s) specified do not match Data/Metadata defined in the object. ")
    }
    if (!(deparse_unit(object@Measurements$Time) %in% c("us","ms","s", "min", "h", "d", "w"))) {
      stop("In 'Measurements', 'Time' should be either of 'us','ms','s','min','h','d','w'")
    }
  }

  # Stimulus slot
  if(!all(object@Stimulus$Step %in% unique(Metadata(object)$Step))){
    stop("All stimuli described must correspond to a Step as defined in 'Metadata'.")
  }

  #Essential fields
  if (!(
    nzchar(object@SubjectInfo$Subject) &&
    !is.na(object@SubjectInfo$DOB) &&
    !is.na(object@ExamInfo$ExamDate)
  )) {
    stop("Subject Name, DOB and ExamDate must be provided.")
  }


  # Dates
    dob <- object@SubjectInfo$DOB
    exam_date <- object@ExamInfo$ExamDate
    imported_date <- object@Imported

  if (("Date" %in% class(dob)) &&
      ("POSIXct" %in% class(exam_date)) &&
      ("POSIXct" %in% class(imported_date))) {
    if (!(as.POSIXct(dob) < exam_date && exam_date < imported_date)) {
      stop("Temporal sequence of 'DOB', 'ExamDate', and 'Imported' is impossible. ")
    }
  } else {
    stop("DOB, ExamDate, and Imported should be of class Date ('DOB'), POSIXct.")
  }

  TRUE
}

#' ERGExam Class
#'
#' A class representing ERG (Electroretinogram) steps with associated data and attributes. This class extends the \link[EPhysData:EphysSet]{EPhysData::EphysSet} object and all methods valid for \link[EPhysData:EphysSet]{EPhysData::EphysSet} can also be applied to \code{ERGExam} objects.
#'
#' @slot Data Data A list of \link{EPhysData} objects.
#' @slot Metadata  A data frame containing metadata information associated with the data, each row corresponds to one list item.
#' \describe{
#'   \item{Step}{A character vector containing the steps associated with the data.}
#'   \item{Eye}{A character vector containing the possible values "RE" (right eye) and "LE" (left eye).}
#'   \item{Channel}{A character vector containing the unique names of the third level of \code{Data}.}
#' }
#'
#' @slot Stimulus
#' A data frame containing stimulus information associated with the data.
#' \describe{
#'   \item{Step}{A character vector containing the steps associated with the stimulus.}
#'   \item{Description}{A character vector describing the stimuli.}
#'   \item{Intensity}{An integer vector representing the intensity of the stimulus (unitless).}
#'   \item{Background}{A character vector describing the adaptation state of the stimulus (DA or LA).}
#'   \item{Type}{A character vector describing the type of the stimulus (e.g. Flash or Flicker).}
#' }
#'
#' @slot Averaged
#' A list of \link{EPhysData} objects. Each element corresponds to the averaged data for a specific step, eye, and channel combination. EPhysData objects contained herein must have one single column only.
#'
#' @slot Measurements
#' A data frame containing measurements information associated with the data.
#' \describe{
#'   \item{Recording}{A numeric vector representing recording identifiers corresponding to the data.}
#'   \item{Name}{A character vector containing the names of the measurements.}
#'   \item{Time}{A numeric vector representing time measurements (seconds).}
#'   \item{Value}{A numeric vector representing measurement values (unitless).}
#' }
#'
#' @slot ExamInfo
#' A list containing exam-related information.
#' \describe{
#'   \item{ProtocolName}{A character vector indicating the name of the protocol.}
#'   \item{Version}{Optional: A character vector indicating the version of the protocol.}
#'   \item{ExamDate}{A \code{POSIXct} timestamp representing the date of the exam.}
#'   \item{Filename}{Optional: A character vector indicating the filename associated with the exam data.}
#'   \item{RecMode}{Optional: A character vector indicating the recording mode during the exam.}
#'   \item{Investigator}{Optional: A character vector indicating the name of the investigator conducting the exam.}
#' }
#'
#' @slot SubjectInfo
#' A list containing subject-related information.
#' \describe{
#'   \item{Subject}{A character vector indicating the name of the patient.}
#'   \item{DOB}{A \code{Date} object indicating the date of birth of the patient.}
#'   \item{Gender}{Optional: A character vector indicating the gender of the patient.}
#'   \item{Group}{Optional: A character vector indicating the group to which the patient belongs.}
#' }
#'
#' @slot Imported
#' A \code{POSIXct} timestamp indicating when the object was imported.
#'
#' @name ERGExam-class
#' @seealso \link[EPhysData:EPhysData-package]{EPhysData::EPhysData-package}  \link[EPhysData:EPhysData]{EPhysData::EPhysData} \link[EPhysData:EphysSet]{EPhysData::EphysSet}
#' @importClassesFrom EPhysData EPhysData EPhysSet
#' @importFrom units as_units
#' @exportClass ERGExam
ERGExam <- setClass(
  "ERGExam",
  contains = "EPhysSet",
  slots = c(
    Data = "list",  # Data is a list of EPhysData objects
    Metadata = "data.frame",  # Metadata is a data frame
    Stimulus = "data.frame",  # Stimulus is a data frame
    Averaged = "list",  # Averaged is a list
    Averaged.imported = "logical",  # Averaged.imported is a logical
    Measurements = "data.frame",  # Measurements is a data frame
    Measurements.imported = "logical",  # Measurements.imported is a logical
    ExamInfo = "list",  # ExamInfo is a list
    SubjectInfo = "list",  # SubjectInfo is a list
    Imported = "POSIXct"
  ),
  prototype = list(
    Data = list(NULL),
    Metadata = data.frame(
      Step = character(),
      Eye = character(),
      Channel = character(),
      stringsAsFactors = FALSE
    ),
    Stimulus = data.frame(
      Step = character(),
      Description = character(),
      Intensity = as_units(integer(),unitless),
      Background = character(),
      Type = character()
    ),
    Averaged = list(NULL),
    Averaged.imported = logical(),
    Measurements = data.frame(
      Recording = numeric(),
      Name = character(),
      Time = as_units(integer(),"s"),
      Value = as_units(integer(),unitless),
      stringsAsFactors = FALSE
    ),
    Measurements.imported = logical(),
    ExamInfo = list(
      ProtocolName = character(),
      Version = character(),
      ExamDate = as.POSIXct(integer()),
      Filename = character(),
      RecMode = character(),
      Investigator = character()
    ),
    SubjectInfo = list(
      Subject = character(),
      DOB = as.Date(x = integer(0), origin = "1970-01-01"),
      Gender = character(),
      Group = character()
    ),
    Imported = as.POSIXct(integer())
  ),
  validity = validERGExam
)

#' Create an instance of the ERGExam class
#'
#' @description This function creates an instance of the \code{ERGExam} class
#' with the specified data and attributes.
#'
#' @param Data A list of \linkS4class{EphysRAW} objects.
#' @param Metadata A data frame containing metadata information associated with the data, each row corresponds to one list item.
#' \describe{
#'   \item{Step}{A character vector containing the steps associated with the data.}
#'   \item{Eye}{A character vector containing the possible values "RE" (right eye) and "LE" (left eye).}
#'   \item{Channel}{A character vector containing the unique names of the third level of \code{Data}.}
#' }
#' @param Stimulus A data frame containing stimulus information associated with the data.
#' \describe{
#'   \item{Step}{A character vector specifying the steps associated with the stimulus.}
#'   \item{Name}{A character vector specifying the names of the stimuli.}
#'   \item{Description}{A character vector describing the stimuli.}
#'   \item{Adaptation}{A character vector describing the adaptation state of the stimulus.}
#'   \item{Background}{An integer vector representing the background intensity of the stimulus (unitless).}
#'   \item{Intensity}{An integer vector representing the intensity of the stimulus (unitless).}
#' }
#' @param Averaged A list of averaged data.
#' @param Measurements A data frame containing measurements information associated with the data.
#' \describe{
#'   \item{Recording}{A numeric vector specifying recording identifiers corresponding to the data.}
#'   \item{Name}{A character vector specifying the names of the measurements.}
#'   \item{Time}{A numeric vector specifying time measurements (seconds).}
#'   \item{Value}{A numeric vector specifying measurement values (unitless).}
#' }
#' @param ExamInfo A list containing exam-related information.
#' \describe{
#'   \item{ProtocolName}{A character vector indicating the name of the protocol.}
#'   \item{Version}{Optional: A character vector indicating the version of the protocol.}
#'   \item{ExamDate}{A \code{POSIXct} timestamp representing the date of the exam.}
#'   \item{Filename}{Optional: A character vector indicating the filename associated with the exam data.}
#'   \item{RecMode}{Optional: A character vector indicating the recording mode during the exam.}
#'   \item{Investigator}{Optional: A character vector indicating the name of the investigator conducting the exam.}
#' }
#' @param SubjectInfo A list containing subject-related information.
#' \describe{
#'   \item{Subject}{A character vector indicating the name of the patient.}
#'   \item{DOB}{A \code{Date} object indicating the date of birth of the patient.}
#'   \item{Gender}{Optional: A character vector indicating the gender of the patient.}
#'   \item{Group}{Optional: A character vector indicating the group to which the patient belongs.}
#' }
#' @examples
#' # Create example data and metadata
#' Data <-
#'   list(
#'     makeExampleEPhysData(nrows = 10, replicate_count = 3),
#'     makeExampleEPhysData(nrows = 10, replicate_count = 3),
#'     makeExampleEPhysData(nrows = 5, replicate_count = 6),
#'     makeExampleEPhysData(nrows = 5, replicate_count = 6)
#'   )  # List of EPhysData objects
#' Metadata <-
#'   data.frame(
#'     Step = c("A1", "A1", "A2", "A2"),
#'     Eye = c("RE", "LE", "LE", "LE"),
#'     Channel = c("Ch1", "Ch1", "Ch1", "Ch2")
#'   )
#' Stimulus <-
#'   data.frame(Step = c("A1", "A2"),
#'              Name = c("Stim1", "Stim2"))  # Example stimulus data
#' ExamInfo <-
#'   list(
#'     ProtocolName = "ERG Protocol",
#'     Version = "1.0",
#'     ExamDate = as.POSIXct("2023-08-14"),
#'     Filename = "exam_data.csv"
#'   )
#' SubjectInfo <-
#'   list(
#'     Subject = "John Doe",
#'     DOB = as.Date("1990-05-15"),
#'     Gender = "Male",
#'     Group = "Control"
#'   )
#' ergExam <-
#'   newERGExam(
#'     Data = Data,
#'     Metadata = Metadata,
#'     Stimulus = Stimulus,
#'     ExamInfo = ExamInfo,
#'     SubjectInfo = SubjectInfo
#'   )
#'
#' @return An object of class \code{ERGExam}.
#' @seealso \code{\link{ERGExam-class}}
#' @importFrom units as_units
#' @export
newERGExam <-
  function(Data,
           Metadata,
           Stimulus,
           Averaged = list(),
           Measurements =  data.frame(
             Recording = numeric(),
             Name = character(),
             Time = as_units(integer(),"s"),
             Value = as_units(integer(),unitless),
             stringsAsFactors = FALSE
           ),
           ExamInfo,
           SubjectInfo) {
    # Call the default constructor
    obj <- new("ERGExam")

    # Set the values for the slots
    obj@Data <- Data
    obj@Metadata <- Metadata
    obj@Stimulus <- Stimulus
    obj@Averaged <- Averaged
    obj@Measurements <- Measurements
    obj@ExamInfo <- ExamInfo
    obj@SubjectInfo <- SubjectInfo

    # Set default values for other slots
    obj@Averaged.imported <- length(obj@Averaged)!=0
    obj@Measurements.imported <- nrow(obj@Measurements)!=0
    obj@Imported <- as.POSIXct(Sys.time())

    # Call the validity method to check if the object is valid
    if (validObject(obj)) {
      return(obj)
    }
  }

#' @noMd
setAs("EPhysSet", "ERGExam", function(from) {
  new(to, Data = from@Data, Metadata = Metadata(from))
})

#' @noMd
#' @importFrom crayon green
setMethod("show",
          "ERGExam",
          function(object) {
            cat("An object of class ERGExam")
            cat(
              green("\nSubject:\t"),
              green(Subject(X)),
              ", ",
              as.character(DOB(X)),
              ", ",
              object@SubjectInfo$Gender,
              sep = ""
            )
            cat(green("\nExam Date:\t", as.character(ExamDate(X))))
            cat("\nProtocol:\t", object@ExamInfo$ProtocolName)
            cat("\nSteps:\t\t")
            cat(X@Stimulus$Description, sep = "\n\t\t")
            cat("Eyes:\t", Eyes(object), sep = "\t")
            cat("\nChannels:\t")
            cat(Channels(object), sep = "\n\t\t")
            if (length(object@Data) == 0) {
              cat("\nRaw data not stored in object.")
            }
            if (length(object@Averaged) == 0) {
              cat("\nNo averaged data stored in object.")
            } else {
              if(X@Averaged.imported){
                cat(green("\n*Averaged data imported from external source.*"))
              }
            }
            if (length(object@Measurements) == 0) {
              cat("\nNo measurements stored in object.")
            } else {
              if(X@Averaged.imported){
                cat(green("\n*Measurements imported from external source.*"))
              }
            }
            cat("\nSize:", format(object.size(object), "auto"))
          })
