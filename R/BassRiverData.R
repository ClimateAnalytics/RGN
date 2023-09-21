#' @title Hydrological data for Bass River catchment in Victoria, Australia
#'
#' @name BassRiverData
#' @description Streamflow, rainfall and PET data for Bass River catchment
#' (227219) in Victoria, Australia. Originally obtained from Francis Chiew.
#' @docType data
#' @usage data(BassRiver)
#' @references \url{https://github.com/eachonly/Robust-Gauss-Newton-Algorithm},
#' \url{http://www.bom.gov.au/water/hrs/}
#' @format List containing numerical vectors for
#'         precipitation (\code{Rain.mm}),
#'         potential evapotranspiration (\code{ET.mm}),
#'         and runoff (\code{Runoff.mm.day}), and
#'         date vector (\code{Date})
#' @examples
#' # select first 3 years of data
#' sel = 1:365*3
#' par(mfrow=c(3,1))
#' # plot streamflow
#' plot(BassRiverData$Date[sel],
#'      BassRiverData$Runoff.mm.day[sel],
#'      type='l',xlab='Date',ylab='Flow (mm/day)',main='Streamflow')
#' # plot rainfall
#' plot(BassRiverData$Date[sel],
#'      BassRiverData$Rain.mm[sel],
#'      type='l',xlab='Date',ylab='Precipitation (mm/day)',main='Precipitation')
#' # plot PET
#' plot(BassRiverData$Date[sel],
#'      BassRiverData$ET.mm[sel],
#'      type='l',xlab='Date',ylab='PET (mm/day)',main='Potential evapotranspiration')
"BassRiverData"

