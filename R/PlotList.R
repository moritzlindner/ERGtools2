#' Plot intensity sequence for ERG exams
#'
#' This function generates a plot of intensity sequence data for ERG exams.
#'
#' @param List A list of ERG exams.
#' @param Background Background condition for the exams.
#' @param Type Type of exam (e.g., "Flash").
#' @param Channel The channel to plot (e.g., "ERG").
#' @param Markers Vector of markers to include in the plot.
#' @param Parameter The parameter to plot ("Amplitude" or "Time").
#' @param wrap_by Wrapping parameter for facetting ("Channel" or NULL).
#'
#' @return A ggplot2 plot object.
#'
#' @importFrom units as_units deparse_unit
#' @import ggplot2
#' @importFrom ggpubr theme_pubclean
#' @import dplyr
#'
#' @examples
#' # Example usage:
#' data <- list(ERGExam1, ERGExam2)
#' PlotIntensitySequence(data, Background = "DA", Type = "Flash", Channel = "ERG")
#'
#' @export
PlotIntensitySequence <-
  function(List,
           Background = "DA",
           Type = "Flash",
           Channel = NULL,
           Markers = c("a", "B", "N1", "P1"),
           Parameter =  "Amplitude",
           wrap_by = "Channel") {

    # Extract Measurements and related info
    results <- get_measurements_for_Plot(
      List,
      Background,
      Type,
      Channel,
      Markers
    )

    # Stats

    if (Parameter == "Amplitude") {
      results <-
        results %>% group_by(Group, Name, Intensity, Channel) %>% summarise(
          Amplitude = mean(Voltage),
          sem = sd(Voltage) / sqrt(n()),
          n =
            n()
        )
      results$sem <-
        as_units(results$sem, deparse_unit(results$Amplitude))
    }
    if(Parameter =="Time"){
      results <-
        results %>% group_by(Group, Name, Intensity, Channel) %>% summarise(
          ImplicitTime = mean(Time),
          sem = sd(Time, na.rm = T) / sqrt(n()),
          n =
            n()
        )

      results$sem <-
        as_units(results$sem, deparse_unit(results$ImplicitTime))

    }

    #results$Group<-paste0(results$Group," (n=",results$n,")")
    # PLot

    if (wrap_by == "Channel") {
      if(Parameter == "Amplitude"){
        plt <- ggplot(data = results,
                      aes(
                        x = Intensity,
                        y = Amplitude ,
                        color = Group,
                        shape = Name
                      )) +
          geom_line() +
          geom_point(size = 2) +
          geom_errorbar(aes(ymin = Amplitude - sem, ymax = Amplitude + sem), width = 0.2)

      }
      if(Parameter == "Time"){
        plt <- ggplot(data = results,
                      aes(
                        x = Intensity,
                        y = ImplicitTime,
                        color = Group,
                        shape = Name
                      )) +
          geom_line() +
          geom_point(size = 2) +
          geom_errorbar(aes(ymin = ImplicitTime - sem, ymax = ImplicitTime + sem), width = 0.2)

      }

      plt<-plt+
        facet_wrap( ~ Channel,
                    scales = "free") +
        theme_pubr(base_size = 8) +
        scale_x_log10() +
        labs(x = "Intensity [cd*s/m^2]", shape = "Marker")
    }
    return(plt)
  }

#' Plot step sequence for ERG exams
#'
#' This function generates a plot of step sequence data for ERG exams.
#'
#' @param List A list of ERG exams.
#' @param Background Background condition for the exams.
#' @param Type Type of exam (e.g., "Flash").
#' @param Channel The channel to plot (e.g., "ERG").
#' @param Markers Vector of markers to include in the plot (e.g. c("a","B)).
#' @param wrap_by Wrapping parameter for facetting ("Channel" or NULL).
#'
#' @return A ggplot2 plot object.
#'
#' @import ggplot2
#' @importFrom ggpubr theme_pubr
#' @import dplyr
#'
#' @examples
#' # Example usage:
#' data <- list(ERGExam1, ERGExam2)
#' PlotStepSequence(data, Background = "DA", Type = "Flash", Channel = "ERG")
#'
#' @export
PlotStepSequence <-
  function(List,
           Background = "DA",
           Type = "Flash",
           Channel = NULL,
           Markers = c("N1", "P1"),
           wrap_by = "Channel") {

    # Extract Measurements and related info
    results <- get_measurements_for_Plot(
      List,
      Background,
      Type,
      Channel,
      Markers
    )

    # Stats

    results <-
      results %>% group_by(Group, Name, Step , Channel) %>% summarise(Amplitude = mean(Voltage),
                                                                      sem = sd(Voltage) / sqrt(n()),
                                                                      n=n())

    results$sem <-
      as_units(results$sem, deparse_unit(results$Amplitude))

    results$Group<-paste0(results$Group," (n=",results$n,")")

    # PLot

    if (wrap_by == "Channel") {
      plt <- ggplot(data = results,
                    aes(
                      x = Step,
                      y = Amplitude,
                      color = Group,
                      shape = Name
                    )) +
        geom_errorbar(aes(ymin = Amplitude - sem, ymax = Amplitude + sem), width = 0.2) +
        geom_point(size = 2) +
        facet_wrap( ~ Channel,
                    scales = "free") +
        theme_pubr(base_size = 8) +
        labs(x = "", shape = "Marker")
    }
    return(plt)
  }

#' Plot ERG recordings
#'
#' This function generates a plot of ERG recordings.
#'
#' @param List A list of ERG exams.
#' @param Background Background condition for the exams.
#' @param Type Type of exam (e.g., "Flash").
#' @param Channel The channel to plot (e.g., "ERG").
#' @param wrap_by Wrapping parameter for facetting ("Channel" or NULL).
#'
#' @return A ggplot2 plot object.
#'
#' @importFrom ggplot2 geom_hline geom_line facet_grid scale_color_manual theme
#' @importFrom ggpubr theme_pubclean
#' @importFrom stringr str_detect
#'
#' @examples
#' # Example usage:
#' data <- list(ERGExam1, ERGExam2)
#' PlotRecordings(data, Background = "DA", Type = "Flash", Channel = "ERG")
#'
#' @export
PlotRecordings<-function(List,
                         Background = "DA",
                         Type = "Flash",
                         Channel = "ERG",
                         wrap_by = "Channel",
                         scales = "free_y"){

  results <- lapply(List, function(x) {
    df <- as.data.frame(x)
    tab <- StimulusTable(x)
    df$Subject <- Subject(x)
    df$Group <- x@SubjectInfo$Group
    df$ExamDate <- min(x@ExamInfo$ExamDate)
    tab$Description <-
      iconv(tab$Description, "ASCII//TRANSLIT", sub = '')
    df <- merge(df, tab)

  })

  results <- do.call(rbind.data.frame, results)
  # type conversion and column names
  colnames(results)[str_detect(colnames(results), "cd.s.m")] <-
    "Intensity"
  results$Step <- iconv(results$Step, "ASCII//TRANSLIT", sub = '')

  # subsetting
  if (is.null(Channel)) {
    Channel <- unique(results$Channel)
  }

  results <-
    results[results$Background %in% Background &
              results$Type %in% Type &
              results$Channel == Channel,]

  ggplot(data = results, aes(
    x = Time,
    y = Value,
    color = Eye,
    linetype = as.factor(Result)
  )) +
    geom_hline(yintercept = as_units(0,"uV"), colour = "gray") +
    geom_line() +
    facet_grid(Type+Background+Intensity+Channel~Group+Subject,
               scales = scales
    ) +
    theme_pubclean(base_size = 8) +
    #labs(x = paste0("Time [", si_x, "]"), y = paste0("Voltage [", si_y, "]")) +
    scale_color_manual(values = c("RE" = "darkred", "LE" = "darkblue","OD" = "darkred", "OS" = "darkblue")) +
    theme(panel.grid.major = element_line(size = .1))
}

#' Get measurements for plotting
#'
#' This function extracts measurements and related information for plotting.
#'
#' @param List A list of ERG exams.
#' @param Background Background condition for the exams.
#' @param Type Type of exam (e.g., "Flash").
#' @param Channel The channel to plot (e.g., "ERG").
#' @param Markers Vector of markers to include in the plot.
#'
#' @return A data frame with measurements for plotting.
#'
#' @import dplyr
#'
#' @noRd
get_measurements_for_Plot <- function(List,
                                      Background = "DA",
                                      Type = "Flash",
                                      Channel = NULL,
                                      Markers = c("a", "B", "N1", "P1")) {
  # could add compatibility w single Exam
  if (!all(unlist((lapply(List, function(x) {
    inherits(x, "ERGExam")
  }))))) {
    stop("'List' is not a list of ERGExams.")
  }
  if (is.null(Channel)) {
    Channel <- unique(unlist(lapply(List,Channels)))
  }
  # subset object
  List <- lapply(List, function(x) {
    md <- merge(Metadata(x), StimulusTable(x))
    sel <-
      md$Background %in% Background &
      md$Type %in% Type & md$Channel %in% Channel
    x <- Subset(x, ExamItem = sel)
    x
  })

  results <- lapply(List, function(x) {
    df <- Measurements(x)
<<<<<<< HEAD
    df$Subject <- Subject(x)
    df$Group <- x@SubjectInfo$Group
    df$ExamDate <- min(x@ExamInfo$ExamDate)
||||||| parent of 58d8478 (Merge corrected)
    df$Subject <- Subject(x)
    df$Group <- x@SubjectInfo$Group
    df$ExamDate <- x@ExamInfo$ExamDate
=======
    if(nrow(df)>0){
      df$Subject <- Subject(x)
      df$Group <- x@SubjectInfo$Group
      df$ExamDate <- min(x@ExamInfo$ExamDate)
    }else{
      df$Subject <- character()
      df$Group <- character()
      df$ExamDate <- as.Date(x = integer(0), origin = "1970-01-01")
    }
>>>>>>> 58d8478 (Merge corrected)
    df <-
      merge(df,
            StimulusTable(x),
            by.x = "Step",
            by.y = "Description")
    return(df)
  })
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
