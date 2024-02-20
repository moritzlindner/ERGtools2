#' Generate a ggplot2 plot for a single item/trace from an ERGExam objects
#'
#' This method generates a \link[ggplot2:ggplot]{ggplot2::ggplot} plot for a single trace from an\linkS4class{ERGExam} objects.
#'
#' @inheritParams Subset-method
#' @param Interactive Whether to return an interactive \link[plotly:ggplotly]{plotly::ggplotly}  graph
#' @return A \link[ggplot2:ggplot]{ggplot2::ggplot} plot visualizing the data from a single trace from an \linkS4class{ERGExam} object
#' @importFrom ggplot2 geom_label
#' @importFrom plotly ggplotly
#' @importFrom units deparse_unit set_units
#' @importFrom EPhysData ggEPhysData Rejected
#' @examples
#' data(ERG)
#' AverageFunction(ERG, where=pairlist(Step = 3,Channel = "ERG_auto",Result = 1)) <- mean
#' ggERGTrace(ERG, Step = 3, Eye = "RE", Channel ="ERG_auto", Result = 1, Interactive = T) # Generate a ggplot2 plot
#' # the information obtained from the interactive plot can be used e.g. to update marker positions.
#' @name ggERGTrace
#' @exportMethod ggERGTrace
setGeneric(
  name = "ggERGTrace",
  def = function(X,
                 Step,
                 Eye,
                 Channel,
                 Result = 1,
                 Interactive = F) {
    standardGeneric("ggERGTrace")
  }
)

#' @noRd
setMethod("ggERGTrace",
          "ERGExam",
          function(X,
                   Step,
                   Eye,
                   Channel,
                   Result = 1,
                   Interactive = F) {
            which <-
              IndexOf(
                X,
                Step = Step,
                Eye = Eye,
                Channel = Channel,
                Result = Result
              )

            if (length(which) == 0) {
              stop("No item matches selection.")
            }
            if (length(which) > 1) {
              stop("Multiple items match the selection criteria.")
            }
            sel <- X[[which]]

            Rejected(sel) <-
              as.vector(Rejected(sel)) #workaround in case of non-vector results of the Rejected function
            un <- Measurements(X,
                               Recording = which)$Voltage[1]
            sel@Data <-
              set_units(sel@Data, deparse_unit(un), mode = "standard")
            out <- ggEPhysData(sel)
            mes<-Measurements(X,
                              Recording = which)
            if (any(mes$Recording == which)) {
              mes <-
                mes[mes$Recording == which, ]

              for (i in 1:nrow(mes)) {
                if (!is.na(mes$Relative[i])) {
                  mes$Voltage[i] <-
                    mes$Voltage[i] + mes$Voltage[mes$Name == mes$Relative[i]]
                }
              }


              if (!Interactive) {
                out <-
                  out + geom_label(
                    data = mes,
                    aes(x = Time, y = Voltage, label = Name),
                    colour = "darkblue",
                    inherit.aes = F
                  )
              }else{
                out <-
                  out + geom_text(
                    data = mes,
                    aes(x = Time, y = Voltage, label = Name),
                    colour = "darkblue",
                    inherit.aes = F
                  )
              }

            }
            if(!Interactive){
              return(out)
            }else{
              return(ggplotly(out))
            }

          })
