#' Plot an ERGExam object.
#'
#' This function creates a plot of an ERGExam object, including line plots of
#' stimulus response curves for different label categories.
#'
#' @param X An ERGExam object.
#'
#' @return A ggplot2 plot of the ERGExam data.
#'
#' @export
#' @examples
#' # Example usage:
#' exam_plot <- PlotExam(erg_exam)
#' print(exam_plot)
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_grid vars ggtitle facet_wrap element_line
#' @importFrom ggpubr theme_pubr
#' @importFrom gridExtra grid.arrange
#' @importFrom EPhysData as.data.frame
#' @seealso \linkS4class{ERGExam} \link[ggplot2:ggplot]{ggplot2:ggplot}
#'
#' @exportMethod PlotExam
setGeneric(
  name = "PlotExam",
  def = function(X) {
    standardGeneric("PlotExam")
  }
)
#' @noMd
setMethod(
  "PlotExam",
  signature = "ERGExam",
  definition = function(X) {
    dat <- as.data.frame(X)

    #crude downsampling
    dat <- dat[round(dat$Time / 2.5) == dat$Time / 2.5, ]

    dat$Recording <- NULL
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
    dat$Value <- units::set_units(dat$Value, "uV")

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

    return(grid.arrange(grobs = plotrows, ncol = 2))

  }
)
