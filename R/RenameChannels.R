#' @exportMethod RenameChannels
setGeneric(
  name = "RenameChannels",
  def = function(X,
                 names)
  {
    standardGeneric("RenameChannels")
  }
)

setMethod("RenameChannels",
          "ERGExperiment",
          function(X,
                   names) {

            if(TypeOf(X)=="RAW"){
              tmp<-lapply(X@Steps_RAW,GetTraceNames)
            }
            if(TypeOf(X)=="AVG"){
              tmp<-lapply(X@Steps_AVG,function(x){
                colnames(x)[-1] #first one is time
              })
            }
            if(TypeOf(X)=="Measurements"){
              stop("Not yet implemented.")
            }
            maxlength<-max(unlist(lapply(tmp,
                                         length)))

            tmp<-lapply(tmp,function(x){
              if(length(x)<maxlength){
                for (i in (length(x)+1):maxlength){
                  x[i]<-NA
                }
                x
              }else{
                x
              }
            })

            current<-t(as.data.frame(array( unlist( tmp ) ,
                   dim = c( max(unlist(lapply(tmp,
                                             length))) ,
                           length(tmp)))))
            rownames(current)<-X@Descriptions
            message("Current names are:\n" )
            print(current)
            orig<-current
            if(!(length(dim(names)) %in% c(0,2))){
              stop("'names' has incorrect number of dimensions")
            }

            if(length(dim(names))==0){
              if (length(names)==dim(current)[2]){
                current<-t(apply(current,
                        1,
                        function(x){
                          x<-c(names[!is.na(x)],
                               as.numeric(rep(NA,
                                              length(names)
                                              )[is.na(x)]
                                          )
                          )
                          }
                        )
                  )
              }else{
                stop("Size names vector does not match number of channels.")
              }
            }
            if(length(dim(names))==2){
              current[!is.na(current)]<-names[!is.na(current)]

              current<-as.data.frame(array( unlist( names) ,
                                     dim = dim(current)
                                       ))
              rownames(current)<-X@Descriptions

            }

            message("Names after updating:\n" )
            print(current)
            user_input <- readline("Are you sure you want to update the names? (y/n)  ")
            if(user_input!="y"){
              warning("Nothing updated. stopped by user.")
            }else{
              if(TypeOf(X)=="RAW"){
                for (i in 1:length(X@Steps_RAW)){
                  names(X@Steps_RAW[[i]]@Data)<-current[i,!is.na(current[i,])]
                  X@Steps_RAW[[i]]@Traces<-current[i,!is.na(current[i,])]
                }
              }
              if((TypeOf(X)=="RAW" && length(X@Steps_AVG)!=0) || TypeOf(X)=="AVG" ){
                for (i in 1:length(X@Steps_AVG)){
                  colnames(X@Steps_AVG[[i]])[colnames(X@Steps_AVG[[i]]) %in% orig[i,]]<-current[i,!is.na(current[i,])]
                }
              }
              if((TypeOf(X)=="RAW" && length(X@Steps_AVG)!=0 && length(X@Steps_Measurements)!=0) ||
                 TypeOf(X)=="AVG" && length(X@Steps_Measurements)!=0 ||
                 TypeOf(X)=="Measurements"){
                stop("Updating Measurements not yet implemented")
              }
            }
            return(X)
          }
)
