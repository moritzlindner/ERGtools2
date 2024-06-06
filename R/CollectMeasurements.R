#' Get measurements for plotting
#'
#' This function extracts measurements and related information, e.g. for plotting or statistics.
#'
#' @param List A list of ERG exams.
#' @inheritParams Where
#' @inheritParams Measurements
#' @param Markers Vector of markers to include in the plot.
#'
#' @return A data frame with measurements for plotting.#'
#' @import dplyr
#' @importFrom EPhysData AverageFunction `AverageFunction<-` Rejected `Rejected<-` FilterFunction `FilterFunction<-`
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @examples
#' data(ERG)
#' ERG<-SetStandardFunctions(ERG)
#' ERG <- AutoPlaceMarkers(ERG)
#' CollectMeasurements(list(ERG,ERG), list(Background = "DA", Type = "Flash"))
#' @export
CollectMeasurements <- function(List,
                                where = list(),
                                Markers = c("a", "B", "N1", "P1"),
                                measure.absolute = F) {
  # could add compatibility w single Exam
  quiet=F
  if (!all(unlist((lapply(List, function(x) {
    inherits(x, "ERGExam")
  }))))) {
    if(inherits(List, "ERGExam")){
      Measurements(List, where = where, Marker = Markers, measure.absolute = measure.absolute)
    }
    stop("'List' is not a list of ERGExams.")
  }
  # subset object
  List <- lapply(List, function(x) {
    if(!CheckAvgFxSet(x)){
      stop("Average functions must be set for all objects in the list, but is missing for: ", Subject(x)," recorded on ", as.character(ExamDate(x)), ". ")
    }
    tryCatch({
      x <- Subset(x, where = where, Raw = T)
      return(x)
    }, error = function(e){
      stop("Fetching Metadata and Stimulus values failed for ", Subject(x)," recorded on ", as.character(ExamDate(x)), " with error message: ", e)
    })
  })
  if(!quiet){
    pb = txtProgressBar(min = 0, max = length(List), initial = 0)
  }
  results <- list()
  for (i in 1: length(List)){
    x<-List[[i]]
    tryCatch({
      df <- Measurements(x, measure.absolute = measure.absolute, quiet = T)
      markers<-unique(df$Name)
      for(m in markers){
        if(length(df$Recording[df$Name==m])!=length(unique(df$Recording))){
          warning(paste0("Marker ",m," missing for at least one of the recordings."))
        }
      }

      df<-df[,!(colnames(df) %in% ExtraMetaColumns(x))]
      if(nrow(df)==0){
        message("No measurements found for, " ,Subject(x),". Consider running 'AutoPlaceMarkers()' first.")
      }

      if(nrow(df)>0){
        if (length(x@SubjectInfo$Group)==0) {
          x@SubjectInfo$Group<-"DEFAULT"
        }
        df$Subject <- Subject(x)
        df$Group <- GroupName(x)
        df$ExamDate <- min(x@ExamInfo$ExamDate)
      }else{
        df$Subject <- character()
        df$Group <- character()
        df$ExamDate <- as.Date(x = integer(0), origin = "1970-01-01")
      }

      df <-
        merge(df,
              StimulusTable(x),
              by.x = c("Step","Description"),
              by.y = c("Step","Description"))
      results[[i]]<-df
    }, error = function(e){
      stop("Fetching Measurements failed for ", Subject(x)," recorded on ", as.character(ExamDate(x)), " with error message: ", e)
    })
    if (!quiet) {
      setTxtProgressBar(pb, i)
    }
  }

  if (!quiet) {
    close(pb)
  }
  results <- do.call(rbind.data.frame, results)
  # type conversion and column names
  colnames(results)[colnames(results) == "cd.s.m"] <- "Intensity"
  results$Step.y <- NULL
  results$Step <- iconv(results$Step, "ASCII//TRANSLIT", sub = '')

  # subsetting list
  results <-
    results[results$Name %in% Markers, ]
  return(results)
}

#' @export
#' @noRd
Collect_Measurements<-CollectMeasurements
