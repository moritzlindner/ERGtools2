#' Uses ggplot2 to plot intensity sequence for ERG exams
#'
#' This function generates a \link[ggplot2:ggplot]{ggplot2:ggplot} plot of intensity sequence data for ERG exams.
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
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_errorbar scale_x_log10 facet_wrap labs
#' @importFrom ggpubr theme_pubr
#' @importFrom tidyr %>%
#' @importFrom dplyr summarise
#' @seealso  \link[ggplot2:ggplot]{ggplot2:ggplot}
#'
#' @examples
#' # Example usage:
#' data(ERG)
#' ERG<-SetStandardFunctions(ERG)
#' ERG <- AutoPlaceMarkers(ERG)
#' data <- list(ERG, ERG)
#' ggIntensitySequence(data, Background = "DA", Type = "Flash", Channel = "ERG")
#'
#' @export
ggIntensitySequence <-
  function(List,
           Background = "DA",
           Type = "Flash",
           Channel = NULL,
           Markers = c("a", "B", "N1", "P1"),
           Parameter =  "Amplitude",
           wrap_by = "Channel") {

    # dplyr workaround
    Group<-Name<-Intensity<-Voltage<-sd<-Time<-Amplitude<-sem<-ImplicitTime<-NULL

    # Extract Measurements and related info
    results <- get_measurements_for_Plot(
      List = List,
      Background = Background,
      Type = Type,
      Channel = Channel,
      Markers = Markers
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

#' @export
#' @noRd
PlotIntensitySequence<-ggIntensitySequence


#' Uses ggplot2 to plot step sequence for multiple ERG exams
#'
#' This function generates a \link[ggplot2:ggplot]{ggplot2:ggplot} plot of step sequence (i.e. sequential recordings within a single protocol) data for multiple ERG exams.
#'
#' @param List A list of ERG exams.
#' @param Background Background condition for the exams.
#' @param Type Type of exam (e.g., "Flash").
#' @param Channel The channel to plot (e.g., "ERG").
#' @param Markers Vector of markers to include in the plot (e.g. c("a","B)).
#' @param wrap_by Wrapping parameter for facetting ("Channel" or NULL).
#'
#' @return A \link[ggplot2:ggplot]{ggplot2:ggplot} plot object.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_errorbar scale_x_log10 facet_wrap labs
#' @importFrom ggpubr theme_pubr
#' @import dplyr
#' @seealso \link[ggplot2:ggplot]{ggplot2:ggplot}
#'
#' @examples
#' # Example usage:
#' data(ERG)
#' ERG<-SetStandardFunctions(ERG)
#' ERG <- AutoPlaceMarkers(ERG)
#' data <- list(ERG, ERG)
#' ggStepSequence(data, Background = "DA", Type = "Flash", Channel = "ERG",Markers = c("a", "B"))
#'
#' @export
ggStepSequence <-
  function(List,
           Background = "DA",
           Type = "Flash",
           Channel = NULL,
           Markers = c("N1", "P1"),
           wrap_by = "Channel") {

    #dplyr workaround
    Step<-Group<-Name<-Intensity<-Voltage<-sd<-Time<-Amplitude<-sem<-ImplicitTime<-NULL


    # Extract Measurements and related info
    results <- get_measurements_for_Plot(
      List = List,
      Background = Background,
      Type = Type,
      Channel = Channel,
      Markers = Markers
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

#' @export
#' @noRd
PlotStepSequence<-ggStepSequence


#' Uses ggplot2 to plot ERG traces from multiple ERGExam objects
#'
#' This function generates a \link[ggplot2:ggplot]{ggplot2:ggplot} plot of ERG traces from multiple ERGExam objects.
#'
#' @param List A list of ERG exams.
#' @param Background Background condition for the exams.
#' @param Type Type of exam (e.g., "Flash").
#' @param Channel The channel to plot (e.g., "ERG").
#' @param wrap_by Wrapping parameter for facetting ("Channel" or NULL).
#'
#' @return A ggplot2 plot object.
#'
#' @importFrom ggplot2 geom_hline geom_line facet_grid scale_color_manual theme geom_ribbon
#' @importFrom ggpubr theme_pubclean
#' @importFrom stringr str_detect
#' @importFrom units drop_units
#'
#' @examples
#' # Example usage:
#' data(ERG)
#' ERG<-SetStandardFunctions(ERG)
#' ERG <- AutoPlaceMarkers(ERG)
#' data <- list(ERG, ERG)
#' ggPlotRecordings(data, Background = "DA", Type = "Flash", Channel = "ERG")
#'
#' @export
ggPlotRecordings<-function(List,
                         Background = "DA",
                         Type = "Flash",
                         Channel = "ERG",
                         wrap_by = "Channel",
                         scales = "free_y"){

  # ggplot workaround
  Time<-Value<-Eye<-Result<-NULL
  commoncolnames<-c("Step","Channel","Result","Eye","Repeat","Time")

  results <- lapply(List, function(x) {

    # todo: downsample
    # x<-lapply(x,function(y){
    #   yt<-TimeTrace(y)
    #   yt[yt %in% pretty(yt,300)]
    #   Subset(y,Time)
    #   y
    # })

    # SD
    sdev <- x
    AverageFunction(sdev, where = 1:length(sdev)) <- sd
    sdev <- Subset(sdev, Raw = F)
    df.sdev <- as.data.frame(sdev)
    df.sdev<-df.sdev[,c(commoncolnames,"Value")]
    colnames(df.sdev)[colnames(df.sdev)=="Value"]<-"sdev"

    # N
    n <- x
    AverageFunction(n, where = 1:length(n)) <- length
    n <- Subset(n, Raw = F)
    df.n <- as.data.frame(n)
    df.n<-df.n[,c(commoncolnames,"Value")]
    colnames(df.n)[colnames(df.n)=="Value"]<-"n"
    df.n$n<-drop_units(df.n$n)

    # Avg as per set fx.
    x <- Subset(x, Raw = F)
    df <- as.data.frame(x)
    df<-df[,c(commoncolnames,"Value")]

    df<-merge(df,df.sdev,by=commoncolnames)
    df<-merge(df,df.n,by=commoncolnames)
    df$SEM<-df$sdev/df$n

    tab <- StimulusTable(x)
    df$Subject <- Subject(x)
    if (length(x@SubjectInfo$Group)==0) {
      x@SubjectInfo$Group<-"DEFAULT"
    }
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
    ymin = Value - SEM,
    ymax = Value + SEM,
    color = Eye,
    fill = Eye,
    linetype = as.factor(Result)
  )) +
    geom_hline(yintercept = as_units(0,"uV"), colour = "gray") +
    geom_line() +
    geom_ribbon(alpha=0.2, colour=NA) +
    facet_grid(Type+Background+Intensity+Channel~Group+Subject,
               scales = scales
    ) +
    theme_pubclean(base_size = 8) +
    #labs(x = paste0("Time [", si_x, "]"), y = paste0("Voltage [", si_y, "]")) +
    scale_color_manual(values = c("RE" = "darkred", "LE" = "darkblue","OD" = "darkred", "OS" = "darkblue")) +
    theme(panel.grid.major = element_line(size = .1))
}

#' @export
#' @noRd
PlotRecordings<-ggPlotRecordings


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
#' @return A data frame with measurements for plotting.#'
#' @import dplyr
#' @importFrom EPhysData AverageFunction `AverageFunction<-` Rejected `Rejected<-` FilterFunction `FilterFunction<-`
#' @examples
#' data(ERG)
#' ERG<-SetStandardFunctions(ERG)
#' ERG <- AutoPlaceMarkers(ERG)
#' get_measurements_for_Plot(list(ERG,ERG), Background = "DA", Type = "Flash", Channel = NULL)
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
    if(!CheckAvgFxSet(x)){
      stop("Average functions must be set for all objects in the list, but is missing for: ", Subject(x)," recorded on ", as.character(ExamDate(x)), ". ")
    }
    tryCatch({
      md <- merge(Metadata(x), StimulusTable(x))
      sel <-
        md$Background %in% Background &
        md$Type %in% Type & md$Channel %in% Channel
      avg.fx.buffer <-
        lapply(x, function(y) {
          AverageFunction(y)
        }, ReturnEPhysSet = F)
      avg.fx.buffer <- avg.fx.buffer[sel]

      rejected.buffer <-
        lapply(x, function(y) {
          Rejected(y, return.fx = T)
        }, ReturnEPhysSet = F)
      rejected.buffer <- rejected.buffer[sel]

      filter.fx.buffer <-
        lapply(x, function(y) {
          FilterFunction(y)
        }, ReturnEPhysSet = F)
      filter.fx.buffer <- filter.fx.buffer[sel]

      x <- Subset(x, where = which(sel), Raw = T)

      for (y in 1:length(x)) {
        AverageFunction(x[[y]])<-avg.fx.buffer[[y]]
        Rejected(x[[y]])<-as.vector(Rejected(x[[y]]))
        FilterFunction(x[[y]])<-filter.fx.buffer[[y]]
      }
      return(x)
    }, error = function(e){
      stop("Fetching Metadata and Stimulus values failed for ", Subject(x)," recorded on ", as.character(ExamDate(x)), " with error message: ", e)
    })
  })

  results <- lapply(List, function(x) {
    tryCatch({
      df <- Measurements(x)
      df$Subject <- Subject(x)
      if (length(x@SubjectInfo$Group)==0) {
        x@SubjectInfo$Group<-"DEFAULT"
      }
      df$Group <- x@SubjectInfo$Group
      df$ExamDate <- min(x@ExamInfo$ExamDate)
      df$Subject <- Subject(x)
      df$ExamDate <- x@ExamInfo$ExamDate
      if(nrow(df)>0){
        df$Subject <- Subject(x)
        df$Group <- x@SubjectInfo$Group
        df$ExamDate <- min(x@ExamInfo$ExamDate)
      }else{
        df$Subject <- character()
        df$Group <- character()
        df$ExamDate <- as.Date(x = integer(0), origin = "1970-01-01")
      }
      df <-
        merge(df,
              StimulusTable(x),
              by.x = "Step",
              by.y = "Step")
      return(df)
    }, error = function(e){
      stop("Fetching Measurements failed for ", Subject(x)," recorded on ", as.character(ExamDate(x)), " with error message: ", e)
    })
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
