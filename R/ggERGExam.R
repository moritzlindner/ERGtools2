#' Generates a ggplot2 plot from an ERGExam object.
#'
#' This function creates a \link[ggplot2:ggplot]{ggplot2:ggplot} plot of an ERGExam object, including line plots of
#' stimulus response curves for different label categories.
#'
#' @param X An ERGExam object.
#' @param return.as Whether to return as a \link[gridExtra:grid.arrange]{gridExtra::grid.arrange} grid (the default: 'return.as = "grid"') or as a list of \link[ggplot2:ggplot]{ggplot2:ggplot}s ('return.as = "list"').
#' @param show.markers Whether to return Marker Postitions as stored in the Measurements slot.
#' @param SetSIPrefix Change the SI prefix. Set to \code{keep}, for not to change anything, to \code{auto} (default) for using the \link[EPhysData:BestSIPrefix-methods]{EPhysData:BestSIPrefix-methods} to minimize the number of relevant digits or to any SI prefix to use that. Calls the \link[EPhysData:SetSIPrefix-methods]{EPhysData:SetSIPrefix-methods}.
#' @param downsample 	Integer giving the desired number of intervals for downsampling. Non-integer values are rounded down. Defaults to 250.
#'
#' @return A \link[ggplot2:ggplot]{ggplot2:ggplot} plot of the ERGExam data.
#'
#' @export
#' @examples
#' # Example usage:
#' data(ERG)
#' ERG<-SetStandardFunctions(ERG)
#' exam_plot <- ggERGExam(ERG)
#' print(exam_plot)
#' ggERGExam(ERG,SetSIPrefix="auto")
#' ggERGExam(ERG,SetSIPrefix="k")
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_grid vars ggtitle facet_wrap element_line
#' @importFrom ggpubr theme_pubr
#' @importFrom gridExtra grid.arrange
#' @importFrom grid textGrob
#' @importFrom EPhysData as.data.frame lapply
#' @importFrom stringr str_detect
#' @importFrom units set_units
#' @import dplyr
#' @seealso \linkS4class{ERGExam} \link[ggplot2:ggplot]{ggplot2:ggplot}
#'
#' @exportMethod ggERGExam
setGeneric(
  name = "ggERGExam",
  def = function(X, return.as = "grid", show.markers = T, SetSIPrefix="auto", downsample = 250) {
    standardGeneric("ggERGExam")
  }
)
#' @noMd
setMethod(
  "ggERGExam",
  signature = "ERGExam",
  definition = function(X, return.as = "grid", show.markers = T, SetSIPrefix="auto", downsample = 250) {

    # internal fx
    count_down_results <- function(df) {
      # Get all column names except "Result" and "Filename"
      column_names <- setdiff(names(df), c("Result", "Filename","Recording"))

      # Group by these columns, then mutate "Result" within each group
      df %>%
        group_by_at(column_names) %>%
        mutate(Result = row_number()) %>%
        ungroup()  # Make sure to ungroup at the end
    }

    # ggplot workaround
    Time<-Value<-Eye<-Channel<-colourby<-NULL

    stopifnot(CheckAvgFxSet(X))
    suppressMessages(X <- Downsample(X, n = 250))
    X <- Subset(X, Raw = F)
    X<-SetSIPrefix(X,SetSIPrefix)

    Metadata(X)<-count_down_results(Metadata(X))

    dat <- as.data.frame(X)
    stimtab <- StimulusTable(X)
    dat <- merge(dat, stimtab, by = "Step")

    # Measurements
    if(show.markers){
      mes<-Measurements(X, measure.absolute = T)
      colnames(mes)[colnames(mes)=="Voltage"]<-"Value"
      mes<-merge(mes,Stimulus(X))
    }

    plotrows <- list()
    for (b in unique(dat$Background)) {
      for (t in unique(dat$Type[dat$Background == b])) {
        rows_to_keep <- which(dat$Background == b & dat$Type == t)
        curr <- dat[rows_to_keep, ]
        if (nrow(curr) > 0) {
          curr.mes <-mes[mes$Background == b &
                           mes$Type == t, ]
          if (any(curr$Result != 1)) {
            for (s in unique(curr$Step[curr$Background == b &
                                       curr$Type == t])) {
              tmp <- curr[curr$Step == s,]
              if (!(length(unique(tmp$Result)) > 1)) {
                curr$Result[curr$Background == b & curr$Type == t] <- 1
              }
            }
          }
          curr <- curr[curr$Result == 1,]

          defining <-
            unique(curr[, c("Intensity", "Description")])
          defining <-
            apply(defining, 2, function(x) {
              length(unique(x))
            }) == nrow(defining)
          if (defining["Intensity"]) {
            colourby = "Intensity"
          } else{
            colourby = "Description"
          }
          curr$colourby <- as.ordered(curr[, colourby])
          ID <- paste0(b, "-", t)
          plotrows[[ID]] <- ggplot(curr,
                                   aes(
                                     x = Time,
                                     y = Value,
                                     colour = colourby
                                   )) +
            geom_line() +
            facet_grid(Channel ~ Eye, scales = "free") +
            ggtitle(paste0(Subject(X), ", ", ExamDate(X), "\n", ProtocolName(X))) +
            theme_pubr(base_size = 8) +
            ggtitle((ID))

          # add Measurements

          if(show.markers){
            if(nrow(curr.mes)>0){
              # Measurements
              if (show.markers) {
                plotrows[[ID]] <- plotrows[[ID]] +
                  # geom_line(
                  #   data = curr.mes,
                  #   aes(group = Name),
                  #   color = "black",
                  #   linewidth = 1.0675,
                  #   alpha = 0.3
                  # ) +
                  geom_point(data = curr.mes, aes(group = Name), color = "black")
              }
            }
          }
        }
      }

    }
    # repeated recordings
    repeated <-
      unique(dat[dat$Result > 1, c("Step", "Channel")])
    if (nrow(repeated) > 0) {
      message("Repeated recordings detected")
      for (r in 1:nrow(repeated)) {
        curr <-
          dat[dat$Step == repeated$Step[r] &
                dat$Channel == repeated$Channel[r],]
        if (length(unique(curr$Result)) > 1) {
          ID <-
            paste0("Repeated: ",
                   unique(curr$Description),
                   "-",
                   unique(curr$Channel))
          plotrows[[ID]] <- ggplot(curr,
                                   aes(
                                     x = Time,
                                     y = Value,
                                     colour = as.ordered(Result)
                                   )) +
            geom_line() +
            facet_grid(~ Eye, scales = "free") +
            ggtitle(paste0(Subject(X), ", ", ExamDate(X), "\n", ProtocolName(X))) +
            theme_pubr(base_size = 8) +
            ggtitle((ID))
        }
      }
    }
    if(return.as == "grid"){
      if (length(plotrows) > 1) {
        return(grid.arrange(
          grobs = plotrows,
          ncol = round(length(plotrows)),
          top = textGrob(Subject(X))
        ))
      } else {
        return(plotrows[[1]])
      }
    }
    if(return.as == "list"){
      return(plotrows)
    }
  }
)
