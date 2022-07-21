#' Import ERG datas and measurements from CSV file
#'
#' `r lifecycle::badge("stable")` \cr
#' This function imports ERG recordings from *.csv files exported from Diagnosys' Epsion and creates an \link[=ERGExam]{ERGExam} object.
#' @param filename Path to file. Currently supported *.csv files exported from Diagnosys' Epsion that contain at least: An area with 1) a contents table, 2) a header table, 3) a stimulus table and 4) a marker table. This function in only tested on files with the default horizontal table arrangement but should function for files with a vertical arrangement as well. Behaves otherways similar to the argument \code{file} from \link[utils:read.table]{utils::read.table()}.
#' @param sep The field separator character. Values on each line of the file are separated by this character. If sep = "" (the default for read.table) the separator is ‘white space’, that is one or more spaces, tabs, newlines or carriage returns.
#' @seealso \linkS4class{ERGExam}, \link[utils:read.table]{utils::read.table()}
#' @examples
#' \dontrun{
#' # import a *.csv files exported from Diagnosys' Epsion
#' ERG_Experiment<-ImportEpsion("test.csv")
#' }
#' @return A \linkS4class{ERGExam} object
#' @importFrom data.table fread
#' @importFrom utils read.csv
#' @importFrom ggBiosci si_to_exponent
#' @name ImportEpsion
#' @export
#'
ImportEpsion <- function(filename,
                                 sep = "\t") {

  OUT<-ImportEpsionMeasures(filename,sep = sep)

      # get Table of content
      toc <- get_toc(filename, sep = sep)

      # Get Data
      Steps_Timetrace=vector("list", length(GetStimulusNames(OUT)))
      Steps_RAW=vector("list", length(GetStimulusNames(OUT)))
      Steps_AVG=vector("list", length(GetStimulusNames(OUT)))

      if("Data Table" %in% rownames(toc)){
        tmp<-toc # modify to only get header of data table
        tmp$Right<-tmp$Left+5 # maximum width, if results are included
        Data_Header<-na.exclude(get_content(filename, tmp, "Data Table", sep = sep))

        for (i in 1:dim(Data_Header)[1]){
          if(as.numeric(Data_Header$Chan[i])==1){
            timetrace<-(na.exclude(fread(filename,
                                         select = Data_Header[i,"Column"],
                                         nrows = toc["Data Table","Bottom"]-toc["Data Table","Top"],
                                         skip = toc["Data Table","Top"]-1,
                                         data.table = F,
                                         header = F)))[,1]
            timeunit<-fread(filename,
                            select = Data_Header[i,"Column"],
                            nrows = 1,
                            skip = toc["Data Table","Top"]-2,
                            data.table = F,
                            header = F)[1,1]

            timeunit<-gsub("[\\(\\)]", "", regmatches(timeunit, gregexpr("\\(.*?\\)", timeunit))[[1]])
            timetrace<-timetrace*si_to_exponent(timeunit)
            timeunit<-substr(timeunit,2,2)
            timetrace<-timetrace[!is.na(timetrace)]

            Steps_Timetrace[[Data_Header$Step[i]]]<-timetrace
          }

          if("Result" %in% colnames(Data_Header)){
            resulttrace<-(na.exclude(fread(filename,
                                         select = Data_Header[i,"Column.1"],
                                         nrows = toc["Data Table","Bottom"]-toc["Data Table","Top"],
                                         skip = toc["Data Table","Top"]-1,
                                         data.table = F,
                                         header = F)))[,1]
            resultunit<-fread(filename,
                            select = Data_Header[i,"Column.1"],
                            nrows = 1,
                            skip = toc["Data Table","Top"]-2,
                            data.table = F,
                            header = F)[1,1]

            resultunit<-gsub("[\\(\\)]", "", regmatches(resultunit, gregexpr("\\(.*?\\)", resultunit))[[1]])
            resulttrace<-resulttrace*si_to_exponent(resultunit)
            resultunit<-substr(resultunit,2,2)
            resulttrace<-resulttrace[!is.na(resulttrace)]

            if (!is.array(Steps_AVG[[Data_Header$Step[i]]])){
              Steps_AVG[[Data_Header$Step[i]]]<-array(dim=c(length(timetrace),
                                                            length(unique(Data_Header$Chan[Data_Header$Step==Data_Header$Step[i]])),
                                                            length(unique(Data_Header$Result[Data_Header$Step==Data_Header$Step[i]]))
                                                            )) # change this to only include channels availabe f individual step
            }
            Steps_AVG[[Data_Header$Step[i]]][, as.numeric(Data_Header$Chan[i]), as.numeric(Data_Header$Result[i])] <-
              resulttrace
          }

          if("Trials" %in% colnames(Data_Header)){
            if(i==1){
              message("This file contains raw traces. Currently import of raw traces is not supported. Skipping raw traces.")
            }
            # trialtraces<-(na.exclude(fread(filename,
            #                                select = c((Data_Header[i,"Column.1"]+1):(Data_Header[i,"Column.1"]+Data_Header[i,"Trials"])),
            #                                nrows = toc["Data Table","Bottom"]-toc["Data Table","Top"],
            #                                skip = toc["Data Table","Top"]-1,
            #                                data.table = F,
            #                                header = F)))
            # trialunits<-fread(filename,
            #                   select = c((Data_Header[i,"Column.1"]+1):(Data_Header[i,"Column.1"]+Data_Header[i,"Trials"])),
            #                   nrows = 1,
            #                   skip = toc["Data Table","Top"]-2,
            #                   data.table = F,
            #                   header = F)
            #
            # trialunits<-unique(gsub("[\\(\\)]", "", regmatches(trialunits, gregexpr("\\(.*?\\)", trialunits))[[1]]))
            # if(length(trialunits)>1){stop("Error importing individual trials")}
            # trialtraces<-trialtraces*si_to_exponent(trialunits)
            # trialunits<-substr(trialunits,2,2)
            # trialtraces<-as.matrix(trialtraces[apply(trialtraces,1,function(x){all(!is.na(x))}),])
          }

        }
      }

      OUT@Steps_Timetrace<-Steps_Timetrace
      OUT@Steps_AVG<-Steps_AVG

      return(OUT)
}
