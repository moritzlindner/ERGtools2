#' Get index of a recording
#'
#' This method gets the index of a recording stored in an \linkS4class{ERGExam} object.
#' @inheritParams Subset-method
#' @details This method gets the index of a recording stored in an \linkS4class{ERGExam} object.
#' @return A numeric vector of length 1 containing the index of a recording defined by the parameters, or NULL if entry was not found.
#' @name IndexOf
#' @exportMethod IndexOf
setGeneric(
  name = "IndexOf",
  def = function(X,
                 Step = Steps(X),
                 Eye = Eyes(X),
                 Channel = Channels(X),
                 Result = 1)
  {
    standardGeneric("IndexOf")
  }
)

#' @noMd
setMethod("IndexOf",
          "ERGExam",
          function(X,
                   Step = Steps(X),
                   Eye = Eyes(X),
                   Channel = Channels(X),
                   Result = 1) {
            # Check if Step, Eye, and Channel are characters
            if (!is.character(Eye) || !is.character(Channel)) {
              stop("Eye and Channel must be characters.")
            }
            # Check if Result is numeric
            if (!is.numeric(Step) || !is.numeric(Result)) {
              stop("Step and Result must be numeric.")
            }

            Md <- Metadata(X)
            which <-
              which(Md$Step  %in% Step &
                      Md$Eye %in% Eye &
                      Md$Channel %in% Channel &
                      Md$Result %in% Result)

            # if(length(which)>1){
            #   stop("Multiple recordings match the given parameters. Is the metadata slot of X corrupted?")
            # }
            return(which)
          })
