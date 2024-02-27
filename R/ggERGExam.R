#' Generates a ggplot2 plot from an ERGExam object.
#'
#' This function creates a \link[ggplot2:ggplot]{ggplot2:ggplot} plot of an ERGExam object, including line plots of
#' stimulus response curves for different label categories.
#'
#' @param X An ERGExam object.
#' @param return.as Whether to return as a \link[gridExtra:grid.arrange]{gridExtra::grid.arrange} grid (the default: 'return.as = "grid"') or as a list of \link[ggplot2:ggplot]{ggplot2:ggplot}s ('return.as = "list"').
#' @param show.markers Whether to return Marker Postitions as stored in the Measurements slot.
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
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_grid vars ggtitle facet_wrap element_line
#' @importFrom ggpubr theme_pubr
#' @importFrom gridExtra grid.arrange
#' @importFrom EPhysData as.data.frame lapply
#' @importFrom stringr str_detect
#' @importFrom units set_units
#' @seealso \linkS4class{ERGExam} \link[ggplot2:ggplot]{ggplot2:ggplot}
#'
#' @exportMethod ggERGExam
setGeneric(
  name = "ggERGExam",
  def = function(X, return.as = "grid", show.markers = T) {
    standardGeneric("ggERGExam")
  }
)
#' @noMd
setMethod(
  "ggERGExam",
  signature = "ERGExam",
  definition = function(X, return.as = "grid", show.markers = T) {

    # ggplot workaround
    Time<-Value<-Eye<-Channel<-colourby<-NULL

    X <- lapply(X, function(x) {
      t <- TimeTrace(x)
      interval <- round(length(t) / 300)
      keep <- t[round(t / interval) == t / interval]
      x<-Subset(x, Time = keep, TimeExclusive = T, Raw = F)
      return(x)
    })

    dat <- as.data.frame(X)

    # This is specific for Espion
    stimtab <- StimulusTable(X)
    dat <- merge(dat, stimtab, by = "Step")
    dat$Description <-
      iconv(dat$Description, "UTF-8", "UTF-8", sub = '')
    dat$Channel <-
      iconv(dat$Channel, "ASCII//TRANSLIT", sub = '')
    dat$Background <-
      iconv(dat$Background, "ASCII//TRANSLIT", sub = '')
    dat$Type <-
      iconv(dat$Type, "ASCII//TRANSLIT", sub = '')
    dat$Value <- set_units(dat$Value, "uV")
    colnames(dat)[str_detect(colnames(dat),"cd.")]<-"Intensity"

    # Measurements
    if(show.markers){
      mes<-Measurements(X)
      colnames(mes)[colnames(mes)=="Voltage"]<-"Value"
    }

    plotrows <- list()
    for (b in unique(dat$Background)) {
      for (t in unique(dat$Type[dat$Background == b])) {
        rows_to_keep <- which(dat$Background == b & dat$Type == t)
        curr <- dat[rows_to_keep, ]
        if (nrow(curr) > 0) {
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
          curr.mes <-mes[mes$Background == b &
                           mes$Type == t, ]

          if(nrow(curr.mes)>0){
            for (i in 1:nrow(curr.mes)) {
              if (!is.na(curr.mes$Relative[i])) {
                curr.mes$Value[i] <-
                  curr.mes$Value[i] + curr.mes$Value[curr.mes$Name == curr.mes$Relative[i] &
                                                       curr.mes$Recording == curr.mes$Recording[i]]
              }
            }

            # Measurements
            if (show.markers) {
              plotrows[[ID]] <- plotrows[[ID]] +
                geom_line(
                  data = curr.mes,
                  aes(group = Name),
                  color = "black",
                  size = 1.0675
                ) +
                geom_point(data = curr.mes, aes(group = Name), color = "black")
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
      return(grid.arrange(grobs = plotrows, ncol = 2))
    }
    if(return.as == "list"){
      return(plotrows)
    }
  }
)
