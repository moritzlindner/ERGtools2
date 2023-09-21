#' @noMd
Marker <- setClass(
  "Marker",
  slots = c(
    Name = "character",
    RelativeTo = "character"
  ),
  prototype = list(
    Name = NA_character_,
    RelativeTo = NA_character_
  )
)

#' @noMd
Channel<-setClass(
  "Channel",
  slots = c(
    Name = "character",
    Eye = "character",
    LowFreqCutoff = "character",
    HighFreqCutoff = "character",
    Inverted = "logical",
    Enabled = "logical",
    Markers = "list"
  ),
  validity = function(object) {
    if (!all(sapply(object@Markers, function(m) inherits(m, "Marker")))) {
      "Markers slot should contain only objects of class 'Marker'"
    } else {
      TRUE
    }
  },
  prototype = list(
    Name = NA_character_,
    Eye = NA_character_,
    LowFreqCutoff = NA_character_,
    HighFreqCutoff = NA_character_,
    Inverted = NA,
    Enabled = NA,
    Markers = list()
  )
)

#' @noMd
Step<-setClass(
  "Step",
  slots = c(
    Description = "character",
    Adaptation = "character",
    SampleFrequency = "character",
    PreTriggerTime = "character",
    PostTriggerTime = "character",
    InterSweepDelay = "character",
    Channels = "list"  # Slot for a list of 'Channel' objects
  ),
  validity = function(object) {
    if (!all(sapply(object@Channels, function(c) inherits(c, "Channel")))) {
      "Channels slot should contain only objects of class 'Channel'"
    } else {
      TRUE
    }
  },
  prototype = list(
    Description = NA_character_,
    Adaptation = NA_character_,
    SampleFrequency = NA_character_,
    PreTriggerTime = NA_character_,
    PostTriggerTime = NA_character_,
    InterSweepDelay = NA_character_,
    Channels = list()  # Empty list as a prototype for 'Channels'
  )
)

#' Protocol class definition
#'
#' This class represents an ERG protocol, as it can be impored from Epsion with Export_Date, Name, nSteps, nChannels, and Step slots.
#'
#' @slot Export_Date POSIXct slot for the export date and time.
#' @slot Name Character slot for the protocol name.
#' @slot nSteps Numeric slot for the number of steps.
#' @slot nChannels Numeric slot for the number of channels.
#' @slot Step List slot for a list of 'Step' objects.
#' @name Protocol
#' @rdname Protocol
Protocol<-setClass(
  "Protocol",
  slots = c(
    Export_Date = "POSIXct",
    Name = "character",
    nSteps = "numeric",
    nChannels = "numeric",
    Step = "list"  # Slot for a list of 'Channel' objects
  ),
  validity = function(object) {
    if (!all(sapply(object@Channels, function(c) inherits(c, "Step")))) {
      "Channels slot should contain only objects of class 'Channel'"
    } else {
      TRUE
    }
  },
  prototype = list(
    Export_Date = as.POSIXct(NA),
    Name = NA_character_,
    nSteps = NA_real_,
    nChannels = NA_real_,
    Step = list()  # Empty list as a prototype for 'Step'
  )
)

#' Show method for Protocol class
#'
#' @param object An instance of the Protocol class.
#' @param ... Additional arguments (not used).
#' @name show.Protocol
#' @rdname show.Protocol
setMethod("show", signature = "Protocol", function(object) {
  cat("Protocol Object\n")
  cat("Export Date: ", object@Export_Date, "\n")
  cat("Name: ", object@Name, "\n")
  cat("Number of Steps: ", object@nSteps, "\n")
  cat("Number of Channels: ", object@nChannels, "\n")
  for (i in seq_along(object@Step)) {
    cat("Step ", i, ":\n")
    cat("\tDescription: ", object@Step[[i]]@Description, "\n")
    cat("\tAdaptation: ", object@Step[[i]]@Adaptation, "\n")
    cat("\tSample Frequency: ", object@Step[[i]]@SampleFrequency, "\n")
    cat("\tPre-Trigger Time: ", object@Step[[i]]@PreTriggerTime, "\n")
    cat("\tPost-Trigger Time: ", object@Step[[i]]@PostTriggerTime, "\n")
    cat("\tInter-Sweep Delay: ", object@Step[[i]]@InterSweepDelay, "\n")
    for (j in seq_along(object@Step[[i]]@Channels)) {
      cat("\tChannel ", j, ":\n")
      cat("\t\tName: ", object@Step[[i]]@Channels[[j]]@Name, "\n")
      cat("\t\tEye: ", object@Step[[i]]@Channels[[j]]@Eye, "\n")
      cat("\t\tLow Freq Cutoff: ", object@Step[[i]]@Channels[[j]]@LowFreqCutoff, "\n")
      cat("\t\tHigh Freq Cutoff: ", object@Step[[i]]@Channels[[j]]@HighFreqCutoff, "\n")
      cat("\t\tInverted: ", object@Step[[i]]@Channels[[j]]@Inverted, "\n")
      cat("\t\tEnabled: ", object@Step[[i]]@Channels[[j]]@Enabled, "\n")
      for (k in seq_along(object@Step[[i]]@Channels[[j]]@Markers)) {
        cat("\tMarker ", k, ":\n")
        cat("\t\t\tName: ", object@Step[[i]]@Channels[[j]]@Markers[[k]]@Name, "\n")
        cat("\t\t\tRelativeTo: ", object@Step[[i]]@Channels[[j]]@Markers[[k]]@RelativeTo, "\n")

      }
    }
  }
})

setMethod("show", signature = "Protocol", function(object) {

  print_hierarchical_list <- function(hierarchical_list, level = 0) {
    for (i in 1:length(hierarchical_list)) {
      item<-hierarchical_list[[i]]
      name<-names(hierarchical_list)[[i]]
      if (is.list(item)) {
        # If the item is a list, recursively print its contents with increased indentation.
        cat(rep("\t", level), name, ": ",i, "\n", sep = "")
        print_hierarchical_list(item, level + 1)
      } else {
        # If the item is not a list, print it with the current indentation level.
        cat(rep("\t", level), name, ": ", item, "\n", sep = "")
      }
    }
  }

  moveRedundant <- function(list) {
    if (length(list) == 1) {
      # If there's only one item in the list, return that item
      return(list[[1]])
    } else {
      # Check if all items in the list are identical
      is_redundant <- all(unlist(lapply(list, identical, list[[1]])))
      if (is_redundant) {
        # If all items are identical, return the first item
        return(list[[1]])
      } else {
        # If not all items are identical, return the list as is
        return(list)
      }
    }
  }

  protocol_list <- list(
    Export_Date = object@Export_Date,
    Name = object@Name,
    nSteps = object@nSteps,
    nChannels = object@nChannels,
    Steps = list()
  )

  for (i in seq_along(object@Step)) {
    step_list <- list(
      Description = object@Step[[i]]@Description,
      Adaptation = object@Step[[i]]@Adaptation,
      SampleFrequency = object@Step[[i]]@SampleFrequency,
      PreTriggerTime = object@Step[[i]]@PreTriggerTime,
      PostTriggerTime = object@Step[[i]]@PostTriggerTime,
      InterSweepDelay = object@Step[[i]]@InterSweepDelay,
      Channels = list()
    )

    for (j in seq_along(object@Step[[i]]@Channels)) {
      channel_list <- list(
        Name = object@Step[[i]]@Channels[[j]]@Name,
        Eye = object@Step[[i]]@Channels[[j]]@Eye,
        LowFreqCutoff = object@Step[[i]]@Channels[[j]]@LowFreqCutoff,
        HighFreqCutoff = object@Step[[i]]@Channels[[j]]@HighFreqCutoff,
        Inverted = object@Step[[i]]@Channels[[j]]@Inverted,
        Enabled = object@Step[[i]]@Channels[[j]]@Enabled,
        Markers = list()
      )

      for (k in seq_along(object@Step[[i]]@Channels[[j]]@Markers)) {
        marker_list <- list(
          Name = object@Step[[i]]@Channels[[j]]@Markers[[k]]@Name,
          RelativeTo = object@Step[[i]]@Channels[[j]]@Markers[[k]]@RelativeTo
        )
        channel_list$Markers[[k]] <- marker_list
      }

      step_list$Channels[[j]] <- channel_list
    }
    #step_list <- lapply(step_list, moveRedundant)
    protocol_list$Steps[[i]] <- step_list
  }

  #protocol_list <- lapply(protocol_list, moveRedundant)
  #protocol_list
  print_hierarchical_list(protocol_list)
})

