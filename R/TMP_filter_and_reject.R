# this has to go to EPhysData or a new Package named EPhysFunctions
#' Apply Detrending Filter
#'
#' This function is a wrapper for the \code{\link[pracma]{detrend}} function in the pracma package.
#' It applies a detrending filter to a given time series.
#'
#' @param x A numeric vector representing the time series data.
#' @param ... Additional arguments to be passed to \code{\link[pracma]{detrend}} function.
#'
#' @return A numeric vector representing the detrended time series.
#'
#' @seealso \code{\link[pracma]{detrend}}
#'
#' @examples
#' x <- c(1, 2, 3, 4, 3, 2, 1)
#' detrended <- filter.detrend(x)
#'
#' @importFrom pracma detrend
#'
#' @export
#'
#' @family filter functions
#' @rdname filter.detrend
filter.detrend <- function(x, ...) {
  pracma::detrend(x, ...)
}

#' Filter a signal with a bandpass filter.
#'
#' This function filters a signal using a fourth order Butterworth bandpass filter with
#' specified low and high frequency cutoffs. This function uses the \link[signal:butter]{signal::butter} function from the \link[signal:signal]{signal::signal} package.
#'
#' @param x Numeric vector or matrix of signal data.
#' @param samp.freq Numeric scalar, the sampling frequency in Hertz.
#' @param low Numeric scalar, the lower cutoff frequency in Hertz.
#' @param high Numeric scalar, the upper cutoff frequency in Hertz.
#' @importFrom units as_units units
#'
#' @return Filtered signal with the same dimensions as input 'x'.
#'
#' @examples
#' filtered_signal <- filter.bandpass(signal, samp.freq = 1000, low = 5, high = 100)
#' @seealso \link[signal:butter]{signal::butter}
#'
#' @export
filter.bandpass <- function(x, samp.freq, low, high) {
  # Check if input parameters are numeric
  if (!all(sapply(list(x, samp.freq, low, high), is.numeric))) {
    stop("All input parameters must be numeric.")
  }

  # Check if the sampling frequency is positive
  if (samp.freq <= 0) {
    stop("Sampling frequency must be greater than zero.")
  }

  # Check if the low and high frequencies are in the correct range
  if (low >= high || low < 0 || high >= 0.5 * samp.freq) {
    stop("Invalid low and high frequency values.")
  }

  un <- NULL
  if ("units" %in% class(x)) {
    un <- units(x)
    units(x) <- NULL
  }
  units(samp.freq) <- NULL
  units(low) <- NULL
  units(high) <- NULL
  if (is.null(dim(x))) {
    out <- filter.bandpass.core(x, samp.freq, low, high)
  } else{
    out <- apply(drop_units(dat), 2, function(x) {
      filter.bandpass(x
                      , samp.freq, low, high)
    })
  }
  if (!is.null(un)) {
    return(as_units(out, un))
  } else{
    return(out)
  }
}


#' Core bandpass filter function.
#'
#' This function performs the core bandpass filtering operation using a
#' Butterworth filter.
#'
#' @param x Numeric vector of signal data.
#' @param samp.freq Numeric scalar, the sampling frequency in Hertz.
#' @param low Numeric scalar, the lower cutoff frequency in Hertz.
#' @param high Numeric scalar, the upper cutoff frequency in Hertz.
#'
#' @return Filtered signal.
#'
#' @importFrom signal butter filtfilt
#' @noRd
filter.bandpass.core <- function(x, samp.freq, low, high) {
  bf <- butter(4, c(low, high)  * (2 / samp.freq), type = "pass")
  x.filtered <- filtfilt(bf, x)
  return(x.filtered)
}

#' Detect and Outlier Traces in Time Series Data with repeated measurements
#'
#' This function detects and rejects outlier traces in time series data based on signal-free areas in the trace (e.g. before s signal-causing even occurred or when the response to it has ended).
#'
#' @param x A matrix or data frame containing the time series data. Each column represents
#' a different repeat, and each row represents a time point.
#' @param rejection.cutoff A numeric value representing the threshold for outlier rejection.
#'
#' @return A logical vector indicating whether each time series is rejected (TRUE) or not (FALSE).
#'
#' @details The function applies a series of data processing steps, including data scaling,
#' calculation of areas of maximum normalized spread, smoothening, and scaling, to identify
#' areas of high variance. Time series with high variance areas exceeding the rejection
#' cutoff are considered as outliers and are marked for rejection.
#' @examples
#' # Example usage
#' data_matrix <- matrix(rnorm(30000), ncol = 30)
#' rejected <- autoreject.by.signalfree(data_matrix, rejection.cutoff = 1)
#'
#' @import stats
#'
#' @export
#'
#' @rdname autoreject.by.signalfree
autoreject.by.signalfree <- function(x, rejection.cutoff = 1) {
  # scale the data
  scale <- apply(x, 2, function(x) {
    scale(filter.detrend(x), center = F)
  })
  # get areas of maximum normalized spread
  CoV <- apply(scale, 1, function(x) {
    log1p(sd(x) / abs(mean(x)))
  })
  #smoothen and scale, to identify areas of high variance
  smoother<-(2/floor((length(CoV) / 10)/2))+1
  CoV <- scale(runmed(CoV, smoother), center = T)
  RoV <- CoV > 0
  # take the regions of high variance
  dat_ROV <- x[RoV,]

  dat_ROV <- apply(dat_ROV, 1, function(x) {
    abs(x - mean(x)) / sd(x)
  })
  #calculate spread from normal based on this
  spread <- scale(abs((apply(dat_ROV, 1, mean))))
  rejected <- spread > rejection.cutoff
  return(rejected)
}

# fft
# docu
# additional fx to get top 3 peaks w (rel) amplitude and frequency
fastfourier <- function(x, samp.freq, ...) {
  if(!is.null(dim(x))){
    f<-apply(x,2,funciton(y){
      fastfourier.core(y, samp.freq, ...)
    })
  }else{
    f<-fastfourier.core(x, samp.freq, ...)
  }
}


fastfourier.core<-function(x, samp.freq, ...) {
  if("units" %in% class(x)){
    units(x)<-NULL
  }
  N <- length(x)
  fk <- fft(x)
  fk <- fk[2:length(fk) / 2 + 1]
  fk <- 2 * fk[seq(1, length(fk), by = 2)] / N
  freq <- (1:(length(fk))) * samp.freq / (2 * length(fk))
  return(data.frame(fur = fk, freq = freq))
}




Measurements(X)
Y<-Subset(X, Channel = "ERG_Flicker",Step=9,Eye="RE")
dat<-Y@Data[[1]]@Data
t<-TimeTrace(Y@Data[[1]])
freq<-1/set_units(unique(diff(t)),s)
dat<-as.data.frame(dat)
single<-dat[,1]
plot(single)

plot(apply(filter.bandpass(dat,samp.freq,1,200),1,mean))
plot(apply(dat,1,mean))

tmp<-fft_wrapper(dat,samp.freq)
tmp<-tmp[tmp$freq<as_units(80,"1/s"),]
plot(tmp$freq,tmp$fur)
tmp$freq[which.max(tmp$fur)]


data <- data.frame(keep = apply(dat[autoreject.function],1,mean),drop = apply(dat[,!autoreject.function],1,mean))
data$count <- 1:nrow(data)

require(ggplot2)
ggplot(data, aes(x=count)) +
  geom_line(aes(y=keep),colour="green") +
  geom_line(aes(y=drop),colour="red")
