#' @keywords internal
#' @noMd
as.std.eyename <- function(eye_str) {
  od_str <-
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
  os_str <-
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

  if (!any(is.character(eye_str)) ||
      !all(eye_str %in% c(od_str, os_str))) {
    eye_str<-eye_str[!(eye_str %in% c(od_str, os_str))]
    stop(paste0(eye_str, sep = ", "),
         " is not a / are no valid eye identifier(s).")
  }

  eye_str <- unlist(lapply(eye_str, function(x) {
    if (x %in% od_str) {
      return("RE")
    }
    if (x %in% os_str) {
      return("LE")
    }
  }))
  return(eye_str)
}
