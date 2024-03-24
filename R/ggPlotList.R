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
#' ggIntensitySequence(data, where= list(Background = "DA", Type = "Flash", Channel = "ERG"))
#'
#' @export
ggIntensitySequence <-
  function(List,
           where = list(Background = "DA",
                        Type = "Flash"),
           Markers = c("a", "B", "N1", "P1"),
           Parameter =  "Amplitude",
           wrap_by = "Channel") {

    # dplyr workaround
    Group<-Name<-Intensity<-Voltage<-sd<-Time<-Amplitude<-sem<-ImplicitTime<-NULL

    # Extract Measurements and related info
    results <- Collect_Measurements(
      List = List,
      where = where,
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
#' @inheritParams Where
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
#' ggStepSequence(data, where = list(Background = "DA", Type = "Flash", Channel = "ERG"), Markers = c("a", "B"))
#'
#' @export
ggStepSequence <-
  function(List,
           where = list(Background = "DA",
                        Type = "Flash"),
           Markers = c("N1", "P1"),
           wrap_by = "Channel")
  {

    #dplyr workaround
    Step<-Group<-Name<-Intensity<-Voltage<-sd<-Time<-Amplitude<-sem<-ImplicitTime<-NULL


    # Extract Measurements and related info
    results <- Collect_Measurements(
      List = List,
      where = where,
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
#' @inheritParams Where
#' @param wrap_by Wrapping parameter for facetting ("Channel" or NULL).
#' @param scales Passed on to \link[ggplot2:facet_grid]{ggplot2:facet_grid}.
#'
#' @return A ggplot2 plot object.
#'
#' @importFrom ggplot2 geom_hline geom_line facet_grid scale_color_manual theme geom_ribbon scale_fill_manual guides
#' @importFrom ggpubr theme_pubclean
#' @importFrom stringr str_detect
#' @importFrom units drop_units
#' @examples
#' # Example usage:
#' data(ERG)
#' ERG<-SetStandardFunctions(ERG)
#' ERG <- AutoPlaceMarkers(ERG)
#' data <- list(ERG, ERG)
#' ggPlotRecordings(data, where = list(Background = "DA", Type = "Flash", Channel = "ERG"))
#'
#' @export
ggPlotRecordings <- function(List,
                             where = list(),
                             wrap_by = "Channel",
                             scales = "free_y"){

  # ggplot workaround
  Time<-Value<-Eye<-Result<-NULL
  commoncolnames<-c("Step","Channel","Result","Eye","Repeat","Time")
  message("Running 'ggPlotRecordings()'. This may take a while for long ERGExam lists. ")

  results <- lapply(List, function(x) {
    tryCatch({

      x<-Subset(x,where=where, Raw = T)

      # downsample
      x<-lapply(x,function(y){
        yt<-TimeTrace(y)
        sample<-yt %in% pretty(yt,250)
        unitbuffer<-deparse_unit(y@Data)
        y@Data<-as_units(apply(y@Data,2,y@filter.fx)[sample,1:dim(y)[2]],unitbuffer)
        y@TimeTrace<-yt[sample]
        FilterFunction(y)<- function(z){z}
        return(y)
      })

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

      # Avg as per set fx
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
    }, error = function (e){
      stop("Gathering data failed for ", Subject(x), ", ", ProtocolName(x), " with error message: ", e)
    })

  })

  results <- do.call(rbind.data.frame, results)
  # type conversion and column names
  colnames(results)[str_detect(colnames(results), "cd.s.m")] <-
    "Intensity"
  results$Step <- iconv(results$Step, "ASCII//TRANSLIT", sub = '')


  # plot
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
    scale_fill_manual(values = c("RE" = "darkred", "LE" = "darkblue","OD" = "darkred", "OS" = "darkblue")) +
    theme(panel.grid.major = element_line(size = .1))+
    guides(fill = "none")
}

#' @export
#' @noRd
PlotRecordings<-ggPlotRecordings


#' Get measurements for plotting
#'
#' This function extracts measurements and related information, e.g. for plotting or statistics.
#'
#' @param List A list of ERG exams.
#' @inheritParams Where
#' @param Markers Vector of markers to include in the plot.
#'
#' @return A data frame with measurements for plotting.#'
#' @import dplyr
#' @importFrom EPhysData AverageFunction `AverageFunction<-` Rejected `Rejected<-` FilterFunction `FilterFunction<-`
#' @examples
#' data(ERG)
#' ERG<-SetStandardFunctions(ERG)
#' ERG <- AutoPlaceMarkers(ERG)
#' Collect_Measurements(list(ERG,ERG), list(Background = "DA", Type = "Flash"))
#' @export
Collect_Measurements <- function(List,
                                      where = list(),
                                      Markers = c("a", "B", "N1", "P1")) {
  # could add compatibility w single Exam
  if (!all(unlist((lapply(List, function(x) {
    inherits(x, "ERGExam")
  }))))) {
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

  results <- lapply(List, function(x) {
    tryCatch({
      df <- Measurements(x)
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
