#' Structured Index and Introduction for the ERGtools2 R Package
#'
#' This package contains an environment for working with electroretinogram data. It contains an import method for Diagnosys Espion data, but allows reading in of data also from other manufacturers with limited coding effort. Standard procedures like averaging, subsetting an visualization of individual exams are supported.
#' This package provides the  \link[=ERGExam]{ERGExam-class} class that stores data from a single ERG examination. These may include different recording channels (ERG, OP, VEP, ...), or sequential recordings in repsonse to different stimulus paradigms.
#' It is usually generated from imported raw data using \link[=newERGExam]{newERGExam}. Data acuired from Diagnosys Espion can be imported directly using \link[=ImportEpsion]{ImportEpsion}.\cr\cr
#'
#' The class \link[=ERGExam]{ERGExam-class} expands \link[EPhysData:EPhysSet]{EPhysData::EPhysSet}, so \strong{Methods that work on \link[EPhysData:EPhysSet]{EPhysData::EPhysSet} can be applied to \link[=ERGExam]{ERGExam-class} as well. See: \link[EPhysData:EPhysData-package]{EPhysData::EPhysData-package}}.
#'
## usethis namespace: start
#' @section Standard workflow:
#'
#' After setting the functions, the accession methods inherited from \link[EPhysData:EPhysSet-class]{EPhysData::EPhysSet-class} like \link[EPhysData:as.data.frame]{EPhysData::as.data.frame} and \link[EPhysData:GetData]{EPhysData::GetData} with the argument \code{Raw=F} can be used to return the processed data.\cr\cr
#'
#' @section Object creation:
#' * \link[=newERGExam]{newERGExam} and \link[=ImportEpsion]{ImportEpsion} for \link[=ERGExam]{ERGExam} objects \cr
#' * \link[methods:new]{methods:new} and \link[=ImportEpsionProtocol]{ImportEpsionProtocol} for \link[=ERGProtocol]{ERGProtocol} objects \cr
#' * \link[=ERGExam-data]{data(ERG)} Load example ERG recording \cr\cr
#'
#' @section Accession methods:
#' * \link[=Subset]{Subset} This method subsets an (for \link[=ERGExam]{ERGExam} object into a new object of the same class.
#' * \link[=as.data.frame]{as.data.frame} Returns data frame representing the \link[=ERGExam]{ERGExam} or \link{ERGProtocol} object in long format. When used with the argument \code{Raw = F} on an\link[=ERGExam]{ERGExam} process (i.e. filtered, averaged) data is returned. See also: \link[EPhysData:as.data.frame]{EPhysData::as.data.frame}. \cr\cr
#'
#' * \link[=DOB]{DOB}
#' * \link[=ExamDate]{ExamDate}
#' * \link[=GroupName]{GroupName}
#' * \link[=ProtocolName]{ProtocolName}
#'
#' * \link[=Eyes]{Eyes}
#' * \link[=Steps]{Steps}
#' * \link[=Channels]{Channels}
#' * \link[=StimulusNames]{StimulusNames}
#' * \link[=MarkerNames]{MarkerNames} \cr\cr
#'
#' * \link[=Measurements]{Measurements} Returns the Measurements table. \cr
#' * \link[=StimulusTable]{StimulusTable} \cr\cr
#'
#' @section Processing:
#' * \link[=FilterFunction<-]{FilterFunction<-} Update the FilterFunction for all Recordings in an \linkS4class{ERGExam}, or only those slected using \code{where}.
#' * \link[=Rejected<-]{Rejected<-} Update the Rejected for all Recordings in an \linkS4class{ERGExam}, or only those slected using \code{where}.
#' * \link[=AverageFunction<-]{AverageFunction<-} Update the AverageFunction for all Recordings in an \linkS4class{ERGExam}, or only those slected using \code{where}.
#' * \link[=SetStandardFunctions]{SetStandardFunctions} This method is used to set standard functions for processing \linkS4class{ERGExam} data. It defines default functions for averaging, filtering, and signal rejection based on the stimulus type.
#' * \link[=AutoPlaceMarkers]{AutoPlaceVEP} Automatically sets markers depending on the channel (E.g. ERG, VEP, OP,...) and stimulus type (Flash, FLicker).
#' * \link[=AutoPlaceAB]{AutoPlaceAB} Place the a and B waves on Flash ERG data stored in an an \link[EPhysData:EPhysData-class]{EPhysData::EPhysData-class} object.
#' * \link[=AutoPlaceFlicker]{AutoPlaceFlicker} Place the N1 and P1 markers and determines 1/frequency (period) for Flicker ERG data stored in an an \link[EPhysData:EPhysData-class]{EPhysData::EPhysData-class} object.
#' * \link[=AutoPlaceVEP]{AutoPlaceVEP} Place the P1, N1 and P2 markers for Flash VEP data stored in an an \link[EPhysData:EPhysData-class]{EPhysData::EPhysData-class} object.
#'
#' @section Merging and other object manipulation:
#' * \link[=MergeERGExams]{MergeERGExams} (for \link[=ERGExam]{ERGExam} objects) \cr\cr
#' * \link[=UpdateMeasurements]{UpdateMeasurements} Update the measurements slot in an 'ERGExam' object with new data.
#' * \link[=UpdateChannelNames]{UpdateChannelNames} Update or replace channel names.
#' * \link[=ClearMeasurements]{ClearMeasurements}  Clear the Measurements slots in an  \link[=ERGExam]{ERGExam} object.
#'
#'
#' @section Plot methods:
#' FIXME Tbc.
#' PlotExam	Plot an ERGExam object.
#' PlotIntensitySequence	Plot intensity sequence for ERG exams
#'' PlotRecordings	Plot ERG recordings
# PlotStepSequence	Plot step sequence for ERG exams
#'
#'
#' @examples
#' FIXME Tbc.
#'
#' @author \href{https://www.lindnerlab.de}{Moritz Lindner}
#'
#' @docType package
#' @name .Overview
"_PACKAGE"
