#' Convert eye identifier strings to standard notation
#'
#' @param eye_str Character vector of eye identifier strings.
#' @param exact If \code{TRUE}: Require exact match. If \code{FALSE}: Whole word match is sufficient.
#' @param warn.only Invalid eye identifier strings will only cause a warning, not an error.
#' @return A character vector with standardized eye identifiers ('RE' for right eye, 'LE' for left eye).
#' @examples
#' as.std.eyename(c("RE", "OD", "OS", "Right"))
#' @export
as.std.eyename <- function(eye_str, exact = T, warn.only=F) {
  stopifnot(is.logical(exact))
  if(!warn.only){
    stopifnot(is.character(eye_str))
  }
  if (exact){
    if (!any(is.character(eye_str)) ||
        !all(eye_str %in% c(od_str(), os_str()))) {
      eye_str_nf<-eye_str[!(eye_str %in% c(od_str(), os_str()))]
      if(warn.only){
        warning(paste0(eye_str_nf, sep = ", "),
                " is not a / are no valid eye identifier(s).")
      } else{
        stop(paste0(eye_str_nf, sep = ", "),
             " is not a / are no valid eye identifier(s).")
      }
      
    }
    eye_str <- unlist(lapply(eye_str, function(x) {
      if (x %in% od_str()) {
        return("RE")
      }
      if (x %in% os_str()) {
        return("LE")
      }
      if (!(x %in% od_str() || x %in% os_str())) {
        return(paste0("Unknown_",as.character(x)))
      }
    }))
  } else {
    pattern <-
      paste0("(^|[^A-Za-z])(",
             paste(od_str(), collapse = "|"),
             ")([^A-Za-z]|$)")
    eye_str[grepl(pattern,eye_str)]<-"RE"
    
    pattern <-
      paste0("(^|[^A-Za-z])(",
             paste(os_str(), collapse = "|"),
             ")([^A-Za-z]|$)")
    eye_str[grepl(pattern,eye_str)]<-"LE"
  }
  return(eye_str)
}

#' Convert non-standard channel names to standard channel names
#'
#' @param channel_str Character vector of channel name strings to be converted.
#' @param clear.unmatched Logical indicating whether to clear unmatched strings.
#'
#' @return A character vector with standardized channel names.
#' @examples
#' as.std.channelname(c("ERG_auto", "C-wave", "OPs", "Nonstandard"), clear.unmatched = TRUE)
#' @keywords internal
as.std.channelname<-function(channel_str, clear.unmatched=F){
  found<-is.std.channelname(channel_str)
  
  if(clear.unmatched){
    channel_str[!found]<-as.character(NA)
  }
  
  #ERG
  pattern <-
    paste0("(^|[^A-Za-z])(",
           paste(erg_str(), collapse = "|"),
           ")([^A-Za-z]|$)")
  channel_str[found][grepl(pattern,channel_str[found])]<-"ERG"
  
  #OP
  pattern <-
    paste0("(^|[^A-Za-z])(",
           paste(op_str(), collapse = "|"),
           ")([^A-Za-z]|$)")
  channel_str[found][grepl(pattern,channel_str[found])]<-"OP"
  
  #VEP
  pattern <-
    paste0("(^|[^A-Za-z])(",
           paste(vep_str(), collapse = "|"),
           ")([^A-Za-z]|$)")
  channel_str[found][grepl(pattern,channel_str[found])]<-"VEP"
  
  return(channel_str)
}

#' @return For is.std.channelname: Logical vector indicating whether each element in \code{channel_str} contains a word that describes a standard channel name.
#' @examples
#' is.std.channelname(c("ERG_auto", "C-wave", "OPs", "Nonstandard"))
#' @describeIn as.std.channelname Check if channel name strings are standard channel names
#' @keywords internal
is.std.channelname <- function(channel_str) {
  if (!is.character(channel_str)) {
    stop(paste0(channel_str, sep = ", "),
         " is /are no character vectors.")
  }
  
  all_keywords <- c(erg_str(), op_str(), vep_str())
  
  pattern <-
    paste0("(^|[^A-Za-z])(",
           paste(all_keywords, collapse = "|"),
           ")([^A-Za-z]|$)")
  
  # Check if any word in channel_str matches the pattern
  return(grepl(pattern, channel_str))
}


#' @examples
#' eye.haystack()
#'
#' @describeIn as.std.eyename Get standard eye identifier strings
#' @noMd
#' @keywords internal
eye.haystack <- function() {
  return(c(od_str(),
           os_str()))
}

#' @examples
#' od_str()
#'
#' @describeIn as.std.eyename Get standard right eye identifier strings
#' @noMd
#' @keywords internal
od_str <- function() {
  c("OD",
    "RE",
    "Rechts",
    "Right",
    "Dexter",
    "od",
    "re",
    "rechts",
    "right",
    "dexter")
}

#' @examples
#' os_str()
#'
#' @describeIn as.std.eyename Get standard left eye identifier strings
#' @noMd
#' @keywords internal
os_str <- function() {
  c("OS",
    "LE",
    "Links",
    "Left",
    "Sinister",
    "os",
    "le",
    "links",
    "left",
    "sinister")
}

#' @examples
#' erg_str()
#'
#' @describeIn as.std.channelname Get standard ERG channel name strings
#' @noMd
#' @keywords internal
erg_str <- function() {
  c("ERG",
    "ERGs",
    "ERG_auto",
    "C-wave",
    "PhNp")
}

#' @examples
#' op_str()
#'
#' @describeIn as.std.channelname Get standard oscillatory potentials channel name strings
#' @noMd
#' @keywords internal
op_str <- function() {
  c("OP",
    "OPs")
}

#' @examples
#' vep_str()
#'
#' @describeIn as.std.channelname Get standard VEP channel name strings
#' @noMd
#' @keywords internal
vep_str <- function() {
  c("VEP",
    "VEPs")
}
