#' Merge ERGExams
#'
#' Merges two or more ERGExam objects into a single ERGExam object.
#'
#' @inheritParams Subset-method
#' @param Y An ERGExam object or a list of ERGExam objects to be merged with \code{X}.
#' @param mergemethod will allow to specify how exactly the objects should be merged in future versions. currently only "Append" (the default) is supported.
#' @return An ERGExam object representing the merged data. the file names, protocol names and dates from the individual files merged are stored as additional columns in the Stimulus table. Use \link{StimulusTable}(X, full=T) to see those.
#' @noMd
#' @examples
#' # Merge two ERGExams
#' data(ERG)
#' # make some ERGExams objects that differ
#' exam1<-Subset(ERG,Step=c(1,2,3))
#' exam2<-Subset(ERG,Eye="RE")
#' # merge
#' merged_exam <- MergeERGExams(exam1, exam2)
#'
#' # Merge a list of ERGExams
#' exam_list <- list(exam2, exam3, exam4)
#' merged_exam_list <- mergeERGExams(exam1, exam_list)
#' @exportMethod MergeERGExams
setGeneric(
  name = "MergeERGExams",
  def = function(X, Y, mergemethod = "Append")
  {
    standardGeneric("MergeERGExams")
  }
)

#' @noMd
setMethod("MergeERGExams",
          "ERGExam",
          function(X, Y, mergemethod) {
            if ("ERGExam" %in% class(Y)) {
              return(merge2ERGExams(X, Y))
            }
            if ("list" %in% class(Y)) {
              out <- X
              for (ex in Y) {
                tryCatch(
                  out <- merge2ERGExams(out, ex),
                  error = function(e) {
                    stop("Mergin Exams failed for ", ProtocolName(ex), "with message: ",e)
                  }
                )
              }
              return(out)
            }
          })

#' @noMd
#' @keywords internal
merge2ERGExams <- function(exam1, exam2, mergemethod = "Append") {
  if ((exam1@Averaged != exam2@Averaged)) {
    stop("Objects to merge contain averaged and unaveraged data.")
  }

  # Validity checks
  if (!identical(exam1@SubjectInfo, exam2@SubjectInfo)) {
    stop("SubjectInfo must be identical in both exams.")
  }

  exam1Date <- as.POSIXct(exam1@ExamInfo$ExamDate)
  exam2Date <- as.POSIXct(exam2@ExamInfo$ExamDate)
  if (max(abs(difftime(exam1Date, exam2Date, units = "hours"))) > 3) {
    stop("ExamInfo$ExamDate should differ by a maximum of 3 hours.")
  }
  if(mergemethod=="Append"){
    # Get the maximum Step value from both objects
    maxStep <- max(Steps(exam1))
    # Combine data, metadata and stimulus
    mergedData <- c(exam1@Data, exam2@Data)
    md2 <- Metadata(exam2)
    md2$Step <- md2$Step + maxStep

    # add possible extra columns in Metadata
    if (!isTRUE(all.equal(colnames(Metadata(exam1)), colnames(Metadata(exam2))))) {
      extra.in.exam1 <- colnames(Metadata(exam1))[!(colnames(Metadata(exam1)) %in% colnames(Metadata(exam2)))]
      extra.in.exam2 <- colnames(Metadata(exam2))[!(colnames(Metadata(exam2)) %in% colnames(Metadata(exam1)))]
      if (length(extra.in.exam1) > 0) {
        for (e in extra.in.exam1) {
          exam2@Metadata[, e] <- as.numeric(NULL)
          class(exam2@Metadata[, e]) <- class(exam1@Metadata[, e])
        }
      }
      if (length(extra.in.exam2) > 0) {
        for (e in extra.in.exam2) {
          exam1@Metadata[, e] <- as.numeric(NULL)
          class(exam1@Metadata[, e]) <- class(exam2@Metadata[, e])
        }
      }
    }
    md1 <- Metadata(exam1)
    if (is.null(md1$ExamDate)){
      md1$ExamDate <- ExamDate(exam1)
      md1$Filename <- exam1@ExamInfo$Filename
    }
    if (is.null(md2$ExamDate)){
      md2$ExamDate <- ExamDate(exam2)
      md2$Filename <- exam2@ExamInfo$Filename
    }
    mergedMetadata <- rbind(md1, md2)

    stimtab1 <- StimulusTable(exam1)
    if (is.null(stimtab1$ProtocolName)) {
      stimtab1$ProtocolName <- ProtocolName(exam1)
    }
    stimtab2 <- StimulusTable(exam2)
    if (is.null(stimtab2$ProtocolName)) {
      stimtab2$ProtocolName <- ProtocolName(exam2)
    }
    stimtab2$Step <- stimtab2$Step + maxStep
    mergedStimulus <- rbind(stimtab1, stimtab2)

    # merge measurements
    mergedMeasurements <-
      merge2ERGMeasurements(
        exam1@Measurements,
        exam2@Measurements,
        old.obj2.indices = 1:length(exam2),
        new.obj2.indices = 1:length(exam2)+length(exam1)
      )

    examinfo <- exam1@ExamInfo
    examinfo$Filename <- "Merged Exam"

    # Create a new ERGExam instance with merged data and metadata
    mergedExam <- newERGExam(
      Data = c(exam1@Data, exam2@Data),
      Metadata = mergedMetadata,
      Stimulus = mergedStimulus,
      Averaged = exam1@Averaged,
      Measurements = mergedMeasurements,
      ExamInfo = examinfo,
      SubjectInfo = exam1@SubjectInfo
    )

    if (nrow(mergedStimulus) != nrow(unique(mergedStimulus))) {
      stop("Rows of Stimulus must be unique in the merged object.")
    }

    return(mergedExam)
  }
  if (mergemethod == "ByStimulusAndChannel") {
    stop("This is currently unsupported")

    # identify matching recordings (same stimulus, channel and eye)
    md1<-Metadata(exam1)
    rownames(md1)<-NULL
    md1$Recording<-1:nrow(md1)
    md2<-Metadata(exam2)
    rownames(md2)<-NULL
    md2$Recording<-1:nrow(md2)
    widemd1 <-merge(md1,StimulusTable(exam1),by = "Step")
    widemd1 <-widemd1[,c("Recording","Step","Description","Channel","Eye","Result")]
    colnames(widemd1)[colnames(widemd1)=="Recording"]<-"obj1.idx"
    colnames(widemd1)[colnames(widemd1)=="Step"]<-"obj1.step.idx"
    widemd2 <-merge(md2,StimulusTable(exam2),by = "Step")
    widemd2 <-widemd2[,c("Recording","Description","Step","Channel","Eye","Result")]
    colnames(widemd2)[colnames(widemd2)=="Recording"]<-"obj2.idx"
    colnames(widemd2)[colnames(widemd2)=="Step"]<-"obj2.step.idx"
    pointer.updates.for.obj2<-merge(widemd1,widemd2)

    # identify recordings unique in exam2
    pointer.appending.from.obj2 <-
      which(!(widemd2$obj2.idx %in% pointer.updates.for.obj2$obj2.idx))

    ## add possible extra columns in Metadata
    if (!isTRUE(all.equal(colnames(Metadata(exam1)), colnames(Metadata(exam2))))) {
      extra.in.exam1 <- colnames(Metadata(exam1))[!(colnames(Metadata(exam1)) %in% colnames(Metadata(exam2)))]
      extra.in.exam2 <- colnames(Metadata(exam2))[!(colnames(Metadata(exam2)) %in% colnames(Metadata(exam1)))]
      if (length(extra.in.exam1) > 0) {
        for (e in extra.in.exam1) {
          exam2@Metadata[, e] <- as.numeric(NULL)
          class(exam2@Metadata[, e]) <- class(exam1@Metadata[, e])
        }
      }
      if (length(extra.in.exam2) > 0) {
        for (e in extra.in.exam2) {
          exam1@Metadata[, e] <- as.numeric(NULL)
          class(exam1@Metadata[, e]) <- class(exam2@Metadata[, e])
        }
      }
    }
    md1 <- Metadata(exam1)
    if (is.null(md1$ExamDate)){
      md1$ExamDate <- ExamDate(exam1)
      md1$Filename <- exam1@ExamInfo$Filename
    }
    if (is.null(md2$ExamDate)){
      md2$ExamDate <- ExamDate(exam2)
      md2$Filename <- exam2@ExamInfo$Filename
    }

    ## for those recordings in 2 that have a match  in 1, 1) update step pointer, 2) count up result, 3) add columns exam date and filename, append md2 and data
    md2<-Metadata(exam2)
    sel<-md2$Step %in% pointer.updates.for.obj2$obj2.step.idx
    data2<-exam2@Data[sel]
    measurements2<-Measurements(exam2@Measurements,Recording=which(sel))
    colnames(measurements2)[colnames(measurements2)=="ChannelBinding"]<-"Channel"
    measurements2<-newERGMeasurements(measurements2)
    md2<-md2[sel,]
    md2<-merge(md2,pointer.updates.for.obj2,by.x = c("Step","Channel","Eye","Result"),by.y = c("obj2.step.idx","Channel","Eye","Result"))
    md2$Step<-md2$obj1.step.idx
    md2$Result<-md2$Result+1
    md2<-md2[,c("Step","Channel","Result","Eye","ExamDate","Filename")]
    warning("Extra Metadata columns will be lost")

    ## Merge Data
    mergedData <- c(exam1@Data, data2)
    mergedMetadata <- rbind(md1, md2)
    mergedMeasurements <-
      merge2ERGMeasurements(
        exam1@Measurements,
        measurements2,
        old.obj2.indices = pointer.updates.for.obj2$obj2.idx,
        new.obj2.indices = pointer.updates.for.obj2$obj2.idx+length(exam1)
      )

    examinfo <- exam1@ExamInfo
    examinfo$Filename <- "Merged Exam"

    # Create a new ERGExam instance with merged data and metadata
    mergedExam <- newERGExam(
      Data = mergedData,
      Metadata = mergedMetadata,
      Stimulus = exam1@Stimulus,
      Averaged = exam1@Averaged,
      Measurements = mergedMeasurements,
      ExamInfo = examinfo,
      SubjectInfo = exam1@SubjectInfo
    )

  #  ??????? does matching by result make sense?
  }
}


#' Merge two ERGMeasurements objects
#'
#' This function merges two ERGMeasurements objects by combining their Marker and Measurements data frames.
#' It also provides options to update recording indices and handle marker mismatches.
#'
#' @param obj1 An object of class ERGMeasurements.
#' @param obj2 Another object of class ERGMeasurements.
#' @param increment.recording.index Logical, indicating whether to increment recording indices of obj2.
#' @param old.obj2.indices Optional vector of old recording indices in obj2.
#' @param new.obj2.indices Optional vector of new recording indices to replace the old indices.
#' @noMd
#' @return An object of class ERGMeasurements, representing the merged data.
#' @keywords internal
#' @examples
#'
#' # load the example Measurements object
#' data(Measurements.data)
#'
#' # create a second Measurements object
#' # Create marker data frame
#' marker_df <- data.frame(
#'   Name = c("N1", "P1", "a", "C"),
#'   Relative = c(NA, 1, NA, 3),
#'   ChannelBinding =c("VEP","VEP","ERG","ERG")
#' )
#'
#' # Create measurements data frame
#' measurements_df <- data.frame(
#'   Recording = c(1, 1, 4, 4, 3, 3),
#'   Marker = c(1, 2, 3, 4, 1, 2),
#'   Time = as_units(c(10, 40, 19, 26, 34, 31), "ms")
#' )
#'
#' # Create ERGMeasurements object
#' Measurements.data2 <- new("ERGMeasurements", Marker = marker_df, Measurements = measurements_df)
#' # Show the object
#' Measurements.data2
#'
#' # now merge both objects
#' merge2ERGMeasurements(Measurements.data,Measurements.data2)
#'
#' # other example
#'
#' marker_df3 <- data.frame(
#' Name = c("N2", "P2", "b", "D", "E"),
#' Relative = c(NA, 1, NA, 3, 3),
#' ChannelBinding = c("VEP", "VEP", "ERG", "ERG", "ERG")
#' )
#'
#' measurements_df3 <- data.frame(
#'   Recording = c(1, 1, 4, 4, 5, 5, 3, 3),
#'   Marker = c(1, 2, 3, 4, 3, 5, 1, 2),
#'   Time = as_units(c(10, 40, 19, 26, 34, 31, 15, 25), "ms")
#' )
#'
#' Measurements.data3 <- new("ERGMeasurements", Marker = marker_df3, Measurements = measurements_df3)
#'
#' merge2ERGMeasurements(Measurements.data2,Measurements.data3)
#'
#'
merge2ERGMeasurements <-
  function(obj1,
           obj2,
           increment.recording.index = T,
           old.obj2.indices = NULL,
           new.obj2.indices = NULL) {
    if (!validObject(obj1) || !validObject(obj2)) {
      stop("One or both of the objects are not valid ERGMeasurements objects.")
    }

    obj1.m <- obj1@Marker
    rownames(obj1.m) <- NULL
    obj1.m$obj1.idx <- 1:nrow(obj1.m)
    obj2.m <- obj2@Marker
    rownames(obj2.m) <- NULL
    obj2.m$obj2.idx <- 1:nrow(obj2.m)
    pointer.updates.for.obj2 <-
      merge(obj1.m[, c("Name", "ChannelBinding","obj1.idx")], obj2.m[, c("Name", "ChannelBinding","obj2.idx")])[, c("obj1.idx", "obj2.idx")]

    #Check parent markers of relatives match
    obj2.rel<-obj2.m$Relative[pointer.updates.for.obj2$obj2.idx]
    obj2.rel<-obj2.rel[!is.na(obj2.rel)]
    pointer.pairs<-pointer.updates.for.obj2[match(obj2.rel,pointer.updates.for.obj2$obj2.idx),]
    if (!(all(is.na(pointer.pairs)))) {
      for (i in 1:nrow(pointer.pairs)) {
        if (!all(obj1.m[pointer.pairs$obj1.idx[i], c("Name", "ChannelBinding")] == obj2.m[pointer.pairs$obj2.idx[i], c("Name", "ChannelBinding")])) {
          obj1.probl <- obj1.m[obj1.m$Relative %in% pointer.pairs$obj1.idx[i], ]
          obj2.probl <-
            obj2.m[obj2.m$Relative %in% pointer.pairs$obj2.idx[i], ]
          stop(
            "At least 1 marker mismatch was detected: Marker '",
            obj1.probl[1, 1],
            "' with ChannelBinding '",
            obj1.probl[1, 3],
            "' from 'obj1' is relative to marker '",
            obj1.m$Name[pointer.pairs$obj1.idx[i]] ,
            "', while the corresponding marker in 'obj2' is relative to marker '",
            obj2.m$Name[pointer.pairs$obj2.idx[i]]  ,
            "'. "
          )
        }
      }
    }

    # for appending
    obj2.idx <-
      which(!(1:nrow(obj2.m) %in% pointer.updates.for.obj2$obj2.idx))
    if(length(obj2.idx)>0){
      obj1.idx <- nrow(obj1.m) + 1:length(obj2.idx)
      pointer.appending.for.obj2 <-
        data.frame(obj2.idx = obj2.idx, obj1.idx = obj1.idx)

      # append new and update relative column
      for (i in 1:nrow(pointer.appending.for.obj2)) {
        if (obj2.m$Relative[pointer.appending.for.obj2$obj2.idx[i]] %in% pointer.updates.for.obj2$obj2.idx) {
          obj2.m$Relative[pointer.appending.for.obj2$obj2.idx[i]] <-
            pointer.updates.for.obj2$obj1.idx[pointer.updates.for.obj2$obj2.idx == obj2.m$Relative[pointer.appending.for.obj2$obj2.idx[i]]]
        }
        if (obj2.m$Relative[pointer.appending.for.obj2$obj2.idx[i]] %in% pointer.appending.for.obj2$obj2.idx) {
          obj2.m$Relative[pointer.appending.for.obj2$obj2.idx[i]] <-
            pointer.appending.for.obj2$obj1.idx[pointer.appending.for.obj2$obj2.idx == obj2.m$Relative[pointer.appending.for.obj2$obj2.idx[i]]]
        }
        obj1.m[pointer.appending.for.obj2$obj1.idx[i],] <-
          obj2.m[pointer.appending.for.obj2$obj2.idx[i],]
      }
    }

    obj1.m$obj1.idx <- NULL

    # for updating
    obj2.measurements <- obj2@Measurements
    obj2.measurements <-
      merge(
        obj2.measurements,
        pointer.updates.for.obj2,
        by.x = "Marker",
        by.y = "obj2.idx",
        all.x = T
      )

    if (length(obj2.idx) > 0) {
      obj2.measurements <-
        merge(
          obj2.measurements,
          pointer.appending.for.obj2,
          by.x = "Marker",
          by.y = "obj2.idx",
          all.x = T
        )
    }
    obj2.measurements$Marker[!is.na(obj2.measurements$obj1.idx.x)] <-
      obj2.measurements$obj1.idx.x[!is.na(obj2.measurements$obj1.idx.x)]
    obj2.measurements$Marker[!is.na(obj2.measurements$obj1.idx.y)] <-
      obj2.measurements$obj1.idx.y[!is.na(obj2.measurements$obj1.idx.y)]

    # update recording indices.
    if (increment.recording.index) {
      obj2.measurements$Recording <-
        obj2.measurements$Recording + max(obj1@Measurements$Recording)
    } else {
      if (!is.null(old.obj2.indices)) {
        if (length(new.obj2.indices) == length(old.obj2.indices) &&
            length(old.obj2.indices) == length (unique(obj1@Measurements$Recording))) {
          if (all(old.obj2.indices) %in% obj1@Measurements$Recording) {
            obj1@Measurements$Recording <-
              new.obj2.indices[match(obj1@Measurements$Recording, old.obj2.indices)]
          } else{
            stop("All indicies in 'old.obj2.indices' must be valid recoding indices in 'X'.")
          }
        } else {
          stop("Lengths of 'old.obj2.indices' and 'new.obj2.indices' must match.")
        }
      }
    }

    obj2.measurements <-
      obj2.measurements[, c("Recording", "Marker", "Time")]

    out <- new("ERGMeasurements")
    out@Marker <- obj1.m
    out@Measurements <- rbind(obj1@Measurements, obj2.measurements)
    if (validObject(out)) {
      return(out)
    } else {
      stop("Merging ERGMeasurements object failed for unknown reaons.")
    }
  }
