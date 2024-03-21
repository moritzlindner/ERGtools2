#' AutoPlaceMarkers for ERG/VEP Recordings
#'
#' These methods automatically place markers for ERG/VEP recordings in an \linkS4class{ERGExam} object.
#'
#' @inheritParams UpdateProcessingMethods
#' @param X An \linkS4class{ERGExam} object for \code{AutoPlaceMarkers()} or an \link[EPhysData:EPhysData-class]{EPhysData::EPhysData-class} object for the lower level methods \link[=AutoPlaceAB]{AutoPlaceAB}, \link[=AutoPlaceFlicker]{AutoPlaceFlicker} or \link[=AutoPlaceVEP]{AutoPlaceVEP}.
#' @param Channel.names A \code{pairlist} specifying channel names.
#'
#' @details These methods are used to automatically place markers for ERGs/VEPs.\cr\cr
#' \code{AutoPlaceMarkers()} sets markers depending on the channel (E.g. ERG, VEP, OP,...) and stimulus type (Flash, Flicker), defined via the \code{Channel.names} and \code{Stimulus.type.names} arguments. Markers are placed using the lower level methods \link[=AutoPlaceAB]{AutoPlaceAB}, \link[=AutoPlaceFlicker]{AutoPlaceFlicker} or \link[=AutoPlaceVEP]{AutoPlaceVEP} function depending on the stimulus type.\cr\cr
#'  \link[=AutoPlaceAB]{AutoPlaceAB}, \link[=AutoPlaceFlicker]{AutoPlaceFlicker} and \link[=AutoPlaceVEP]{AutoPlaceVEP} are the lower level functions which perform the actual marker placement on the \link[EPhysData:EPhysData-class]{EPhysData::EPhysData-class} objects contained in the \linkS4class{ERGExam} object. These methods are usually not called directly by a user, unless she/he wants to perform or re-run marker placement only on certain recordings while leaving previously set markers unchanged for the others. \cr\cr
#'  Currently, supported are:
#' * a and B waves for Flash ERG
#' * N1, P1 (and Frequency) for Flicker ERGs
#' * P1, N1, and P2 for Flash ERGs \cr\cr
#'
#' @return An updated \code{ERGExam} object with markers placed.
#'
#' @examples
#' data(ERG)
#' ERG<-SetStandardFunctions(ERG)
#' imported_Markers<-Measurements(ERG)
#' head(imported_Markers)
#' ERG<-ClearMeasurements(ERG)
#' imported_Markers_cleared<-Measurements(ERG)
#' head(imported_Markers_cleared)
#' ERG<-AutoPlaceMarkers(ERG, Channel.names = pairlist(ERG = "ERG"))
#' autoplaced_Markers<-Measurements(ERG)
#' head(autoplaced_Markers)
#'
#' # Calling AutoPlaceAB() directly
#' X<-ERG@Data[[1]] # get first recording
#' AutoPlaceAB(X)
#' @name AutoPlaceMarkers
NULL
#' @describeIn AutoPlaceMarkers Automatically sets markers depending on the channel (E.g. ERG, VEP, OP,...) and stimulus type (Flash, FLicker).
#' @exportMethod AutoPlaceMarkers
setGeneric(
  name = "AutoPlaceMarkers",
  def = function(X,
                 Channel.names = pairlist(ERG = "ERG",
                                          VEP = "VEP"),
                 Stimulus.type.names = pairlist(Flash = "Flash",
                                                Flicker = "Flicker")) {
    standardGeneric("AutoPlaceMarkers")
  }
)

#' @noMd
setMethod(
  "AutoPlaceMarkers",
  signature = "ERGExam",
  definition = function(X,
                        Channel.names = pairlist(ERG = "ERG",
                                                  VEP = "VEP"),
                        Stimulus.type.names = pairlist(Flash = "Flash",
                                                  Flicker = "Flicker")) {
    markerlist<-list()
    Md<-merge(Metadata(X),StimulusTable(X))
    pb = txtProgressBar(min = 0, max = length(X@Data), initial = 0)
    for(i in 1:length(X@Data)){
      x <- X@Data[[i]]
      update<-F
      # FLASH ERGs
      tryCatch({
        if (Md$Channel[i] %in% Channel.names$ERG &
            Md$Type[i]  %in%  Stimulus.type.names$Flash) {
          Markers <- AutoPlaceAB(x)
          update<-T
        }
        # Flicker ERGs
        if (Md$Channel[i] %in% Channel.names$ERG &
            Md$Type[i]  %in%  Stimulus.type.names$Flicker) {
          Markers <- AutoPlaceFlicker(x)
          update<-T
        }
        # Flash VEP
        if (Md$Channel[i] %in% Channel.names$VEP &
            Md$Type[i]  %in%  Stimulus.type.names$Flicker) {
          Markers <- AutoPlaceVEP(x)
          update<-T
        }

        if(update){
          Markers$Name <- rownames(Markers)
          for (m in 1:nrow(Markers[is.na(Markers$Relative)])) {
            Measurements(
              X,
              Marker = Markers$Name[m],
              where = i,
              create.marker.if.missing = T,
              Relative = Markers$Relative[m],
              ChannelBinding = Md$Channel[i]
            ) <- Markers$Time[m]
          }
        }
      },
      error = function(e) {
        currMd <- merge(Metadata(X), StimulusTable(X))[i, ]
        close(pb)
        stop(
          "Auto placement of markers failed for recording ",
          i,
          " (Step: ",
          currMd$Step,
          " - ",
          currMd$Description,
          ", Channel: ",
          currMd$Channel,
          ", Eye: ",
          currMd$Eye,
          ") with message: ",
          e
        )
      })
      setTxtProgressBar(pb,i)
    }
    close(pb)
    return(X)
  }
)

#' @describeIn AutoPlaceMarkers places the a and B waves on Flash ERG data stored in an an \link[EPhysData:EPhysData-class]{EPhysData::EPhysData-class} object.
#' @exportMethod AutoPlaceAB
setGeneric(
  name = "AutoPlaceAB",
  def = function(X) {
    standardGeneric("AutoPlaceAB")
  }
)
#' @importFrom EPhysMethods filter.bandpass freq.to.w
#' @noMd
setMethod(
  "AutoPlaceAB",
  signature = "EPhysData",
  definition = function(X) {
    dat<-getdf(X)
    sample.rate<-attr(dat,"sample.rate")

    cutoff<-freq.to.w(x=c(5,75),time.trace<-TimeTrace(X))
    dat$Filtered <- filter.bandpass(dat$Voltage, cutoff[1], cutoff[2])
    B_estimate <- which.max(dat$Filtered)
    a_estimate <- which.min(dat$Filtered[1:B_estimate])

    search_left<-0.005/(1/sample.rate) # 5 ms
    search_right<-0.01/(1/sample.rate) # 10 ms

    B_pos<-which.max(dat$Voltage[(B_estimate-search_left):(B_estimate+search_right)])+B_estimate-search_left
    a_pos<-which.min(dat$Voltage[(a_estimate-search_left):(a_estimate+search_right)])+a_estimate-search_left

    B_time<-dat$Time[B_pos]
    B_amp<-dat$Voltage[B_pos]-dat$Voltage[a_pos]
    a_time<-dat$Time[a_pos]
    a_amp<-dat$Voltage[a_pos]

    out<-data.frame(Time = c(a_time, B_time),
                    Voltage = c(a_amp, B_amp),
                    Relative = c(NA,"a"))
    rownames(out)<-c("a","B")
    return(out)
  }
)

#' @describeIn AutoPlaceMarkers places the N1 and P1 markers and determines 1/frequency (period) for Flicker ERG data stored in an an \link[EPhysData:EPhysData-class]{EPhysData::EPhysData-class} object
#' @exportMethod AutoPlaceFlicker
setGeneric(
  name = "AutoPlaceFlicker",
  def = function(X) {
    standardGeneric("AutoPlaceFlicker")
  }
)
#' @importFrom EPhysMethods filter.bandpass fastfourier freq.to.w
#' @noMd
setMethod(
  "AutoPlaceFlicker",
  signature = "EPhysData",
  definition = function(X) {
    dat<-getdf(X)
    sample.rate<-attr(dat,"sample.rate")
    cutoff<-freq.to.w(x=c(3,75),time.trace<-TimeTrace(X))
    dat$Filtered <- filter.bandpass(dat$Voltage, cutoff[1], cutoff[2])
    fft<-fastfourier(dat$Filtered,samp.freq = sample.rate)
    fft<-fft[fft$freq<100,]
    domfreq<-fft$freq[which.max(Re(fft$fur))]
    # 50Hz reject
    if(domfreq > 47 & domfreq < 53){
      fft$fur[which.max(Re(fft$fur))]<-0
      domfreq<-fft$freq[which.max(Re(fft$fur))]
    }
    peak_interval<-sample.rate/domfreq

    dat$Filtered[peak_interval+i]

    # a matrix for averaging, based on the peak interval calculated from the fourier transform
    start<-which(dat$Time==as_units(0,"ms"))
    end<-start+peak_interval
    posmtx <- matrix(nrow = (end - start), ncol = (nrow(dat) / peak_interval))
    for (i in 1:nrow(posmtx)) {
      tmp<-seq(i+start, nrow(dat), peak_interval)
      if (length(tmp)<ncol(posmtx)){
        tmp<-c(tmp,rep(NA,ncol(posmtx)-length(tmp)))
      }
      posmtx[i,] <-tmp
    }

    peak.avg<-apply(posmtx,1,function(x){
      mean(dat$Filtered[x])
    })
    P1_estimate<-dat$Time[which.max(peak.avg)+start]
    P1_estimate_idx<-which(dat$Time==P1_estimate)
    N1_estimate<-dat$Time[which.min(peak.avg)+start]
    N1_estimate_idx<-which(dat$Time==N1_estimate)

    search_left<-0.005/(1/sample.rate) # 5 ms
    search_right<-0.01/(1/sample.rate) # 10 ms

    P1_pos<-which.max(dat$Voltage[(P1_estimate_idx-search_left):(P1_estimate_idx+search_right)])+P1_estimate_idx-search_left
    N1_pos<-which.min(dat$Voltage[(N1_estimate_idx-search_left):(P1_pos)])+N1_estimate_idx-search_left

    P1_time<-dat$Time[P1_pos]
    P1_amp<-dat$Voltage[P1_pos]-dat$Voltage[N1_pos]
    N1_time<-dat$Time[N1_pos]
    N1_amp<-dat$Voltage[N1_pos]

    out<-data.frame(Time = c(N1_time, P1_time,set_units(set_units(1/domfreq,"s"),units(P1_time), mode = "standard")),
                    Voltage = c(N1_amp, P1_amp,as_units(NA,"V")),
                    Relative = c(NA,"N1",NA))
    rownames(out)<-c("N1","P1","Period")
    return(out)
  }
)

#' @describeIn AutoPlaceMarkers places the P1, N1 and P2 markers for Flash VEP data stored in an an \link[EPhysData:EPhysData-class]{EPhysData::EPhysData-class} object
#' @exportMethod AutoPlaceVEP
setGeneric(
  name = "AutoPlaceVEP",
  def = function(X) {
    standardGeneric("AutoPlaceVEP")
  }
)
#' @importFrom EPhysMethods filter.bandpass freq.to.w
#' @noMd
setMethod(
  "AutoPlaceVEP",
  signature = "EPhysData",
  definition = function(X) {
    dat<-getdf(X)
    sample.rate<-attr(dat,"sample.rate")
    cutoff<-freq.to.w(x=c(10,150),time.trace<-TimeTrace(X))
    dat$Filtered <- filter.bandpass(dat$Voltage, cutoff[1], cutoff[2])
    N1_estimate <- which.min(dat$Filtered)
    P1_estimate <- which.max(dat$Filtered[(N1_estimate-30):N1_estimate])+N1_estimate-30 # use first local max left of N1

    cutoff<-freq.to.w(x=c(2,150),time.trace<-TimeTrace(X))
    dat$Filtered <- filter.bandpass(dat$Voltage, cutoff[1], cutoff[2])
    P2_estimate <- which.max(dat$Filtered[N1_estimate:(N1_estimate+50)])+N1_estimate

    search_left<-0.005/(1/sample.rate) # 5 ms
    search_right<-0.01/(1/sample.rate) # 10 ms

    N1_pos<-which.min(dat$Voltage[(N1_estimate-search_left):(N1_estimate+search_right)])+N1_estimate-search_left
    P1_pos<-which.max(dat$Voltage[(P1_estimate-search_left):(P1_estimate+search_right)])+P1_estimate-search_left
    P2_pos<-which.max(dat$Voltage[(P2_estimate-search_left):(P2_estimate+search_right)])+P2_estimate-search_left

    N1_time<-dat$Time[N1_pos]
    N1_amp<-dat$Voltage[N1_pos]-dat$Voltage[P1_pos]
    P1_time<-dat$Time[P1_pos]
    P1_amp<-dat$Voltage[P1_pos]
    P2_time<-dat$Time[P2_pos]
    P2_amp<-dat$Voltage[P2_pos]-dat$Voltage[P1_pos]

    # ggplot(dat,aes(x=Time,y=Voltage))+
    #   geom_line()+
    #   geom_vline(xintercept = N1_time)+
    #   geom_vline(xintercept = P1_time)+
    #   geom_vline(xintercept = P2_time)

    out <- data.frame(
      Time = c(P1_time, N1_time, P2_time),
      Voltage = c(P1_amp, N1_amp, P2_amp),
      Relative = c(NA,"P1","P1")
    )
    rownames(out) <- c("P1", "N1", "P2")
    return(out)
  }
)

#' @importFrom units drop_units
#' @importFrom EPhysData TimeTrace GetData
#' @importFrom stats median
#' @keywords internal
#' @noMd
getdf<-function(X){
  sample.rate<-1/set_units(median(diff(TimeTrace(X))),"s")
  sample.rate<-drop_units(sample.rate)

  dat<-GetData(X,Raw = F)
  if(!ncol(dat)==1){
    stop("'X' has no valid Averaging Function set. Result of averaging Data must be a vector. See documentation for 'AverageFunction<-' or 'SetStandardFunctions'.")
  }
  dat<-as.data.frame(dat)
  colnames(dat)<-"Voltage"
  dat$Time<-TimeTrace(X)
  attr(dat,"sample.rate")<-sample.rate
  return(dat)
}
