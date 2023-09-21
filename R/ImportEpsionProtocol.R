
#' Import an Epsion stimulus protocol from a CSV file.
#'
#' This function imports an Epsion stimulus protocol from a CSV file and returns it as a Protocol S4 object.
#'
#' @param filename The path to the CSV file containing the protocol data.
#' @return An instance of the Protocol S4 class.
#'
#' @importFrom  data.table fread
#' @importFrom stringr str_detect
#' @export
ImportEpsionProtocol<-function(filename){
  Header<-fread(filename,sep = ",",header = F,fill=T,strip.white = T, nrows=85, data.table=F)
  Name=getSingleColumnChar(Header,"Protocol Name")
  Exported<-as.POSIXct(getSingleColumnChar(Header,"Export date"))
  nSteps=getSingleColumnNumeric(Header,"Total steps")
  nChannels=getSingleColumnNumeric(Header,"Total channels")

  # import Step Parameters
  rn <-
    fread(
      filename,
      sep = ",",
      header = F,
      fill = T,
      skip=85,
      strip.white = T,
      select = c(1),
      data.table = F
    )$V1

  StepParam <-
    getTable(
      filename,
      start = which(rn == "Step Parameters") + 85,
      end = which(rn == "")[1] + 85,
      nCols = nSteps
    )
  keep <-
    c(
      "Description",
      "Adaptation required",
      "Sample frequency",
      "Sweep pre-trigger time",
      "Sweep post trigger time",
      "Inter sweep delay"
    )
  StepParam<-StepParam[keep,]
  StepParam<-unlist(apply(StepParam, 2, list), recursive = F, use.name = F)
  StepParam<-lapply(StepParam, function(x) {
    x <- as.data.frame(x, col.names = FALSE)
  })

  # import Channel Parameters

  ChannelList<-list()
  MarkerList<-list()
  for (i in 1:nChannels) {
    start <- which(rn == paste0("Chan ", i, " Parameters")) + 85
    end <- if (any(rn == paste0("Chan ", i + 1, " Parameters"))) {
      which(rn == paste0("Chan ", i + 1, " Parameters")) + 85
    } else {
      length(rn) + 85
    }

    ChParam <-
      getTable(filename,
               start = start[1],
               end = end[1],
               nCols = nSteps)
    markers_start <-
      which(str_detect(rownames(ChParam), "Marker"))
    markers_names <-
      which(str_detect(rownames(ChParam), "Name"))
    if (length(markers_names)>1){
      markers_names <- markers_names[markers_names > markers_start]
      markers_relto <-
        which(str_detect(rownames(ChParam), "Amplitude is relative to"))


      markers_names <- ChParam[markers_names,]
      markers_relto <- ChParam[markers_relto,]
    }else{
      markers_names<-NULL
      markers_relto<-NULL
    }

    keep <-
      c(
        "Name",
        "Enabled",
        "Eye being tested",
        "Filter low frequency cutoff",
        "Filter high frequency cutoff",
        "Invert input polarity"
      )
    ChParam <- ChParam[keep, ]

    MarkerList[[i]]<-list(Names=markers_names,RelTo=markers_relto)
    ChannelList[[i]]<-as.data.frame(ChParam)
    #ChannelList[[i]]$Marker<-
  }

  # convert into S4 Structure
  Protocol<-new("Protocol")
  Protocol@Name <- enc2utf8(Name)
  Protocol@Export_Date<-Exported
  Protocol@nSteps<-nSteps
  Protocol@nChannels<-nChannels

  Steps<-list()
  for (s in 1:Protocol@nSteps){
    currStep<-new("Step")
    currStep@Description <-
      stri_enc_toutf8(StepParam[[s]]$x[rownames(StepParam[[s]]) == "Description"])
    currStep@Adaptation <-
      StepParam[[s]]$x[rownames(StepParam[[s]]) == "Adaptation required"]
    currStep@SampleFrequency <-
      StepParam[[s]]$x[rownames(StepParam[[s]]) == "Sample frequency"]
    currStep@PreTriggerTime <-
      StepParam[[s]]$x[rownames(StepParam[[s]]) == "Sweep pre-trigger time"]
    currStep@PostTriggerTime <-
      StepParam[[s]]$x[rownames(StepParam[[s]]) == "Sweep post trigger time"]
    currStep@InterSweepDelay <-
      StepParam[[s]]$x[rownames(StepParam[[s]]) == "Inter sweep delay"]
    for (c in 1:Protocol@nChannels) {
      currChan<-new("Channel")
      curr<-ChannelList[[c]][,s, drop=F]
      currChan@Name<-curr["Name",1]
      currChan@Eye<-curr["Eye being tested",1]
      currChan@Enabled<-curr["Enabled",1]=="On"
      currChan@LowFreqCutoff<-curr["Filter low frequency cutoff",1]
      currChan@HighFreqCutoff<-curr["Filter high frequency cutoff",1]
      currChan@Inverted<-curr["Enabled",1]=="On"
      for (m in 1:length(MarkerList[[c]]$Names[, s])) {
        currMarker<-new("Marker")
        if(!is.null(MarkerList[[c]]$Names[m, s])){
          currMarker@Name<-MarkerList[[c]]$Names[m, s]
          currMarker@RelativeTo<-MarkerList[[c]]$RelTo[m, s]
          currChan@Markers[[m]]<-currMarker
        }
      }
      currStep@Channels[[c]]<-currChan
    }
    Steps[[s]]<-currStep
  }
  Protocol@Step<-Steps

  return(Protocol)
}

#' @noMd
getSingleColumnChar<-function(table,key){
  value<-table[table$V1==key,]
  value<-value[1,!is.na(value[1,])]
  paste(unlist(value[,-1]), collapse = ', ',sep = ", ")
}
#' @noMd
getSingleColumnNumeric<-function(table,key){
  value<-table[table$V1==key,]
  as.numeric(value[2])
}
#' @noMd
getTable<-function(filename,start,end,nCols){
  tab<-fread(
    filename,
    sep = ",",
    header = T,
    fill = T,
    skip=start-1,
    nrows=end-start,
    strip.white = T,
    blank.lines.skip=T,
    select = 1:(1+nCols),
    data.table = F
  )
  rownames(tab)<-make.unique(tab[,1])
  tab<-tab[,-1]
  return(tab)
}

