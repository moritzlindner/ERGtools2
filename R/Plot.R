#' Plot an ERGExam object.
#'
#' This function creates a plot of an ERGExam object, including line plots of
#' stimulus response curves for different label categories.
#'
#' @param X An ERGExam object.
#' @param labelby A character string specifying the variable to color by (default: "Intensity").
#' @param facetby TBC
#'
#' @return A ggplot2 plot of the ERGExam data.
#'
#' @export
#' @examples
#' # Example usage:
#' exam_plot <- PlotExam(erg_exam)
#' print(exam_plot)
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_grid vars ggtitle facet_wrap
#' @importFrom ggpubr theme_pubr
#' @importFrom EPhysData as.data.frame
#'
#' @exportMethod PlotExam
setGeneric(
  name = "PlotExam",
  def = function(X, labelby = "Intensity",facetby="Eye+Channel") {
    standardGeneric("PlotExam")
  }
)
#' @noMd
setMethod("PlotExam",
          signature = "ERGExam",
          definition = function(X, labelby = "Intensity",facetby="Description") {
            dat <- as.data.frame(X)

            #crude downsampling
            dat<-dat[round(dat$Time/2.5)==dat$Time/2.5,]

            dat$Recording<-NULL
            stimtab <- StimulusTable(X)
            dat <- merge(dat, stimtab, by = "Step")
            dat$Description<-iconv(dat$Description, "UTF-8", "UTF-8",sub='')
            dat$Channel<-iconv(dat$Channel, "ASCII//TRANSLIT",sub='')
            dat$labelby <- as.ordered(dat[, labelby])
            dat$Value<-units::set_units(dat$Value,"uV")

            if(facetby=="Eye+Channel"){
              return(ggplot(dat, aes(x = Time, y = Value, linetype = as.factor(Repeat), linetype=as.ordered(Repeat), color = labelby)) +
                geom_line() +
                facet_grid(Background + Channel ~ Eye, scales = "free") +
                theme_pubr() +
                ggtitle(paste0(Subject(X), ", ", ExamDate(X), "\n", ProtocolName(X))))+
                theme_pubr(base_size = 8) +
                theme(panel.grid.major = element_line(size = .25))
            }
            if(facetby=="Description"){
              return(ggplot(dat, aes(x = Time, y = Value, color = Eye)) +
                geom_line() +
                facet_wrap(Channel+Result~Description, scales = "free") +
                theme_pubr() +
                ggtitle(paste0(Subject(X), ", ", ExamDate(X), "\n", ProtocolName(X))))+
                theme_pubr(base_size = 8) +
                theme(panel.grid.major = element_line(size = .25))
            }
          })
