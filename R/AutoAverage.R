
#' @exportMethod AutoAverage
setGeneric(
  name = "AutoAverage",
  def = function(X,
                 SD_thresh=2,
                 Verbose = T)
  {
    standardGeneric("AutoAverage")
  }
)

setMethod("AutoAverage",
          "ERGExperiment",
          function(X,
                   SD_thresh=2,
                   Verbose = T) {
            for (i in 1:length(X@Steps_RAW)){
              if(Verbose){
                message("Processing Step ", i)
              }
              Step<-X@Steps_RAW[[i]]
              # find most variable areas of curve
              SCALED<-apply(apply(Step,
                                  "Sweep",
                                  mean,
                                  Verbose = F),
                            2,
                            function(x){scale(abs(x),
                                              center=F)})

              # only keep most variable region (across all channels)
              TIMEVECTOR<-GetTimeTrace(Step)
              SCALED<-SCALED[,!grepl("Time",colnames(SCALED))]
              SCALED<-apply(SCALED,1,mean)
              SCALED<-SCALED>1
              TIMEVECTOR<-TIMEVECTOR[SCALED]


              VAR_AVG<-apply(GetData(Step,
                                     Time=TIMEVECTOR,
                                     TimeExclusive=T,
                                     nowarnings = T),
                             "Sweep",
                             mean,
                             ReturnPObject=F,
                             Verbose = F)


              # compute z scores for each
              score<-matrix(nrow=length(GetTraceNames(Step)),ncol=length(GetSweepNames(Step)))
              colnames(score)<-GetSweepNames(Step)
              rownames(score)<-GetTraceNames(Step)

              for (channel in GetTraceNames(Step)){
                score[channel,]<-apply(
                  apply(abs(GetData(Step,
                                    Time=TIMEVECTOR,
                                    TimeExclusive=T,
                                    nowarnings = T)@Data[[channel]]-VAR_AVG[,channel]),
                        1,
                        scale),
                  1,
                  mean
                )
              }

              # take max score from all channels
              score<-apply(score,2,max)<SD_thresh
              X@Steps_AVG[[i]]<-apply(GetData(Step,
                                              Sweeps = GetSweepNames(Step)[score],
                                              nowarnings = T),
                                      "Sweep",
                                      mean,
                                      ReturnPObject=F,
                                      Verbose = F)
              X@Steps_Excl[[i]]<-!score
              if(Verbose){
                if (length(score)!=sum(score)){
                  message("Dropping ", length(score)-sum(score),"/",length(score)," Sweeps")
                }
              }
            }
            return(X)
          }
)
