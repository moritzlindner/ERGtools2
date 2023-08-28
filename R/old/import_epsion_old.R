#' #' Import electrophysiology recordings
#'
#' #' This function imports electrophysiology recordings from PatchMaster *.dat files ("Series") or Axon's ABF files and creates a PRecording object. Support for WinWCP files is thought in future.
#' #'
#' #' @param filename Path to file. Currently only [Patch Master .dat](https://www.heka.com/downloads/downloads_main.html#down_patchmaster) and [Axon Binary (.abf)](https://swharden.com/pyabf/abf2-file-format/) files are supported.
#' #' @seealso \linkS4class{PRecording}
#' #' @examples
#' #' \dontrun{
#' #' # import a PatchMaster file
#' #' tmp<-ImportPRecording("test.dat",series = 1,traces = c(1,2))
#' #' }
#' #' @return A \linkS4class{PRecording} object
#' #' @importFrom data.table fread
#' #' @importFrom ggBiosci si_to_exponent
#' #' @import PatchR
#' #'
#' ImportEpsion_old <- function(filename,
#'                          delim="TAB",
#'                          import="Raw") {
#'   message(paste("Importing", filename))
#'   if (!file.exists(filename)) {
#'     stop("File ", filename, " does not exist")
#'   }
#'   if (delim == "TAB") {
#'     if(read.csv(filename,header = F,sep ="\t",nrow=1)[1]=="Contents Table"){
#'       # get Table of content
#'       toc<-fread(filename,
#'                  sep ="\t",
#'                  data.table=F,
#'                  skip=1,
#'                  header=T,
#'                  blank.lines.skip=T,
#'                  select=c("Table","Left","Right","Top","Bottom"))
#'
#'       HeaderTabPos<-which(toc$Table=="Header Table") # if vertical table, chop off after end of Header
#'       if (length(HeaderTabPos)>1){ # "Header Table" found twice, so its a vertical table
#'         EndOfTOC<-min(which(toc$Table=="")[which(toc$Table=="")>HeaderTabPos[1]]) # this is the first blank line after TOC
#'         toc<-toc[1:EndOfTOC,]
#'       }
#'
#'       toc=toc[!toc$Table=="",]
#'       tmp<-toc$Table
#'       toc$Table<-NULL
#'       toc <- data.frame(apply(toc, 2, function(x) as.numeric(as.character(x))))
#'       rownames(toc)<-tmp
#'
#'
#'       # check if Header and Stimulus Info present
#'       if (!sum(rownames(toc) %in% c("Header Table","Stimulus Table"))==2){
#'         stop("Table of content incomplete.")
#'       }
#'       # get protocol info
#'       recording_info <- fread(filename,
#'                               select = toc["Header Table","Left"]:toc["Header Table","Right"],
#'                               nrows = toc["Header Table","Bottom"]-toc["Header Table","Top"],
#'                               skip = toc["Header Table","Top"]-1,
#'                               data.table = F,
#'                               header = F)
#'       rownames(recording_info)<-recording_info[,1]
#'       recording_info[,1]<-NULL
#'
#'       # Get stimulus information
#'       stim_info <- fread(filename,
#'                          select = toc["Stimulus Table","Left"]:toc["Stimulus Table","Right"],
#'                          nrows = toc["Stimulus Table","Bottom"]-toc["Stimulus Table","Top"]+1,
#'                          skip = toc["Stimulus Table","Top"]-1,
#'                          data.table = F,
#'                          header = F)
#'       colnames(stim_info)<-fread(filename,
#'                                  select = toc["Stimulus Table","Left"]:toc["Stimulus Table","Right"],
#'                                  nrows = 1,
#'                                  skip = 1,
#'                                  data.table = F,
#'                                  header = F)
#'
#'
#'       if (("Raw" %in% import) && ("Data Table" %in% rownames(toc)) ){
#' #        todo: this could be easily utilized for results column replace word "Trial" below by "Result"
#'
#'         if(!("Data Table" %in% rownames(toc))){
#'           stop("No raw data stored in file.")
#'         }
#'         # get position of individual recordings
#'         keepvector<-c(rep("NULL",toc["Data Table","Left"]-1),
#'                       rep("numeric",toc["Data Table","Right"]-(toc["Data Table","Left"]-1))
#'         )
#'
#' #hier runter fehler fuer Vertikale tabellen!!!
#'         stepidx=which(fread(filename,
#'                             select = toc["Data Table","Left"]:toc["Data Table","Right"],
#'                             nrows = 1,
#'                             skip = 1,
#'                             data.table = F,
#'                             header = F)=="Step")
#'
#'         colidx=which(fread(filename,
#'                            select = toc["Data Table","Left"]:toc["Data Table","Right"],
#'                            nrows = 1,
#'                            skip = 1,
#'                            data.table = F,
#'                            header = F)=="Column")[1]
#'
#'         chidx=which(fread(filename,
#'                           select = toc["Data Table","Left"]:toc["Data Table","Right"],
#'                           nrows = 1,
#'                           skip = 1,
#'                           data.table = F,
#'                           header = F)=="Chan")
#'
#'         ntrialsidx=which(fread(filename,
#'                                select = toc["Data Table","Left"]:toc["Data Table","Right"],
#'                                nrows = 1,
#'                                skip = 1,
#'                                data.table = F,
#'                                header = F)=="Trials")
#'
#'         data_info <- na.exclude(fread(filename,
#'                                       select = toc["Data Table","Left"]:toc["Data Table","Right"],
#'                                       nrows = toc["Data Table","Bottom"]-toc["Data Table","Top"],
#'                                       skip = toc["Data Table","Top"]-1,
#'                                       data.table = F,
#'                                       header = F)[c(stepidx,colidx,chidx,ntrialsidx)])
#'
#'         colnames(data_info)<-fread(filename,
#'                                    select = toc["Data Table","Left"]:toc["Data Table","Right"],
#'                                    nrows = 1,
#'                                    skip = 1,
#'                                    data.table = F,
#'                                    header = F)[c(stepidx,colidx,chidx,ntrialsidx)]
#'
#'         Steps<-list()
#'
#'         for (step in 1:as.numeric(recording_info["Steps",])){
#'           message(paste("Processing Step ", step, " - ", stim_info[stim_info$Step==step,"Description"]))
#'           Data <- list()
#'           unit <- character(length=length(data_info[data_info$Step==step,"Chan"]))
#'           for (channel in data_info[data_info$Step==step,"Chan"]){
#'             i=which(data_info$Step==step & data_info$Chan==channel)
#'
#'             if (channel == 1){ # in case of the first channel, first selected column is time trace
#'               timetrace<-as.matrix(fread(filename,
#'                                          select = data_info[i,"Column"],
#'                                          nrows = toc["Data Table","Bottom"]-toc["Data Table","Top"],
#'                                          skip = toc["Data Table","Top"]-1,
#'                                          data.table = F,
#'                                          header = T))[,1]
#'               timeunit<-fread(filename,
#'                               select = data_info[i,"Column"],
#'                               nrows = 1,
#'                               skip = toc["Data Table","Top"]-2,
#'                               data.table = F,
#'                               header = F)[1,1]
#'               timeunit<-gsub("[\\(\\)]", "", regmatches(timeunit, gregexpr("\\(.*?\\)", timeunit))[[1]])
#'               timetrace<-timetrace*si_to_exponent(timeunit)
#'               timeunit<-substr(timeunit,2,2)
#'               timetrace<-timetrace[!is.na(timetrace)]
#'             }
#'
#'             if (!is.na(i)){
#'               message(paste("Channel ", channel))
#'
#'               tmp<-as.matrix(fread(filename,
#'                                    select = (data_info[i,"Column"]+1):(data_info[i,"Column"]+data_info[i,"Trials"]),
#'                                    nrows = length(timetrace), #toc["Data Table","Bottom"]-toc["Data Table","Top"],
#'                                    skip = toc["Data Table","Top"]-2,
#'                                    data.table = F,
#'                                    header = T))
#'
#'               Data[[make.names(as.character(channel))]]<-tmp[,grepl("Trial",colnames(tmp))]
#'
#'               datunit<-colnames(Data[[make.names(as.character(channel))]])
#'               datunit<-gsub("[\\(\\)]", "", regmatches(datunit, gregexpr("\\(.*?\\)", datunit))[[1]])
#'               Data[[make.names(as.character(channel))]]<-Data[[make.names(as.character(channel))]]*si_to_exponent(datunit)
#'               unit[channel]<-substr(datunit,2,2)
#'
#'             }
#'           }
#'
#'           # Get type of Step/Series - this is a workaround as no fixed column for Flash and LA/DA passed on from Espion
#'           Type<-character()
#'           if(grepl( "LA", stim_info[stim_info$Step == step, "Description"], fixed = TRUE,useBytes = TRUE)){
#'             Type<-"LA"
#'           }else if(grepl( "DA", stim_info[stim_info$Step == step, "Description"], fixed = TRUE,useBytes = TRUE)){
#'             Type<-"DA"
#'           }
#'           if(grepl( "Flicker", stim_info[stim_info$Step == step, "Description"], fixed = TRUE,useBytes = TRUE)){
#'             Type<-paste0(Type,"; Flicker")
#'           }else{
#'             Type<-paste0(Type,"; Flash")
#'           }
#'           Type<-paste0(Type,"; ",stim_info[stim_info$Step == step, "cd.s/m\xb2"])
#'
#'           # Write Step/Series parameters
#'           params <- PatchR:::PRecordingParams(
#'             Traces = make.names(data_info[data_info$Step == step, "Chan"]),  # !!!! this should be replaced by a meaningful naming of the channels
#'             ProtocolName = recording_info["Protocol", 1],
#'             Version = recording_info["Version", 1],
#'             Created = as.POSIXct(recording_info["Date performed", 1], format =
#'                                    "%d/%m/%Y %H:%M:%S"),
#'             Filename = filename,
#'             RecMode = recording_info["Test method", 1],
#'             Experiment = paste0(
#'               recording_info["Animal #", 1],
#'               "; DOB: ",
#'               as.POSIXct(recording_info["DOB", 1], format =    "%d/%m/%Y")
#'             ),
#'             Series = stim_info[stim_info$Step == step, "Description"]
#'           )
#'
#'           #Compile into PRecording Object
#'           Steps[[step]] <- PatchR:::PRecording(
#'             Traces = make.names(data_info[data_info$Step==step,"Chan"]),
#'             Units = unit,
#'             TimeTrace = timetrace,
#'             TimeUnit = timeunit,
#'             Sweeps = ordered(1:dim(Data[[1]])[2],levels=1:dim(Data[[1]])[2]),
#'             SweepTimes = rep(0,dim(Data[[1]])[2]), # not given, so MAKE NULL
#'             Data = Data,
#'             Plots = list(),
#'             RecordingParams = params
#'           )
#'         }
#'
#'         return(
#'           ERGExperiment(
#'             Steps_RAW=Steps,
#'             Descriptions=stim_info[, "Description"],
#'             ProtocolName = recording_info["Protocol", 1],
#'             Version = recording_info["Version", 1],
#'             Created = as.POSIXct(recording_info["Date performed", 1], format =
#'                                    "%d/%m/%Y %H:%M:%S"),
#'             Filename = filename,
#'             RecMode = recording_info["Test method", 1],
#'             Patient = recording_info["Animal #", 1],
#'             DOB = as.POSIXct(recording_info["DOB", 1], format =    "%d/%m/%Y")
#'           )
#'         )
#'
#'
#'       }
#'     }else{
#'       stop(paste(filename," does not contain a Contents Table in first cell."))
#'     }
#'   }
#' }
