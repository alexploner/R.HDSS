#' Check pre-processed HDSS data
#'
#' These functions test and report which records of a pre-processed HDSS data
#' have valid and consistent values
#'
#' @param x The data frame to be validated
#' @param maxAge A maximum age for checking the valid range of birth dates
#' @param studyPeriod A vector of length two, either of dates indicating the
#' beginning and end of the study period, or of characters expressing dates
#' that can be passed to `as.Date`. If not specified, the stuyd period is
#' inferred from the data, see Details.
#' @param observationLag A grace period (in years) after the end of the study
#' period: observation dates are still accepted as prima facie valid if the fall
#' not more than \code{observationLag} years after the end of study.
#'
#' @details These functions report on the content of a pre-processed HDSS data
#'   set (as opposed to \code{coreVariableTests} and \code{vaVariableTests},
#'   which check the formal requirements for a raw HDSS data set). \cr\cr
#'   \code{coreRecordTests} checks for valid codes, missing values introduced by
#'   pre-processing, proper sequence of events, reasonable ranges of dates and
#'   ages, proper counting of events and female mothers. This is done for the
#'   same set of core variables as in \code{coreVariableTests}. \cr
#'   If no study period is specified by the user, it is inferred from the data
#'   as follows: start of study is the earliest non-missing event date for all
#'   starting events, i.e. `ENU`, `BTH` and `IMG` (enumeration, birth and
#'   immigration); end of study is the latest non-missing event data for
#'   closing events (coded as `OBE`). \cr\cr
#'   \code{vaRecordTests} checks again for valid codes, agreement between event
#'   coding and verbal autopsy coding, as well as agreement within verbal 
#'   autopsy variables (the same set as in \code{vaVariableTests}).
#'
#' @return Each test returns a list with two entries:
#'   \itemize{
#'     \item{\code{flag}}{ a logical flag hat indicates whether all tests were passsed or not}
#'     \item{\code{text}}{ a data frame with one column that contains the error messages from the failed tests; empty if all tests were passed}
#'   }
#'
#' @references Osman Sankoh and Peter Byass. The INDEPTH Network: filling vital gaps in global epidemiology. Int J Epi 2012; 41:579-588, Tables 1 and 2.
#' @seealso \code{\link{var-check-tools}} \code{\link{readRawHDSS}}
#'          \code{\link{preprocHDSS}} \code{\link{coreVariableTests}}
#' @name recTests-HDSS
#' @export
#'
coreRecordTests = function(x, maxAge=105, studyPeriod, observationLag=0.5)
{
	## Infer the study period, if required
	if (missing(studyPeriod)) {
		## Note: dplyr::summarise does not work with na.rm=TRUE
		BoS <- min(filter(x, EventCode %in% c("ENU", "IMG", "BTH") )$EventDate, na.rm=TRUE)
	    EoS <- max(filter(x, EventCode == "OBE" )$EventDate, na.rm=TRUE)
	} else {
		studyPeriod = as.Date(studyPeriod)
		BoS = min(studyPeriod)
		EoS = max(studyPeriod)
	}
	
	## Valid country identifiers
	ret = x %>% recTest_CorrectCodes("CountryId", INDEPTH_Centres$ISOcode)
	ret = x %>% recTest_CorrectCodes("CentreId",  INDEPTH_Centres$Code) %>% addRecTest(ret, .)

	## Correct sex and event codes
	ret = x %>% recTest_CorrectCodes("Sex", c("m", "f")) %>% addRecTest(ret, .)
	ret = x %>% recTest_CorrectCodes("EventCode", INDEPTH_eventCodes) %>% addRecTest(ret, .)

	## Check location ID for missingness
	ret = x %>% recTest_Missing("LocationId") %>% addRecTest(ret, .)	

	## Check the dates for missingness, valid ranges
	ret = x %>% recTest_Missing("DoB") %>% addRecTest(ret, .)
	## Valid range: not too old at event, not born after reasonable end of study estimate
	## Note: one constant, one varying limit, handled with grace
	lower <- x$EventDate - round(maxAge * 365.25)
	ret = x %>% recTest_InRange("DoB", lower, EoS) %>% addRecTest(ret, .)				

	ret = x %>% recTest_Missing("EventDate") %>% addRecTest(ret, .)
	ret = x %>% recTest_InRange("EventDate", BoS, EoS) %>% addRecTest(ret, .)				

	ret = x %>% recTest_Missing("ObservationDate") %>% addRecTest(ret, .)
	ret = x %>% recTest_InRange("ObservationDate", BoS, EoS+round(observationLag*365.25)) %>% addRecTest(ret, .)				

	## Check the order in which things happen
	ret = x %>% recTest_LessOrEqual("DoB", "EventDate") %>% addRecTest(ret, .)

	## For event OBE, observation data is generally before event date (last visit before censoring)
	## While doubtful, we'll play along for now
	tt = x %>% recTest_LessOrEqual("EventDate", "ObservationDate")
	## We set all results for OBE to NA
	tt$Index[x$EventCode=="OBE"] = NA
	tt$Desc = paste("No OBE only: ", tt$Desc, sep="")
	ret = addRecTest(ret, tt)

	## All deliveries linked to females
	isDelivery = x$EventCode == "DLV"
	isFemale   = x$Sex == "f"
	femDLV = isFemale
	femDLV[!isDelivery] = NA
	ret = addRecTest(ret, recTest(femDLV, "Delivery for female subjects only"))

	## All births linked to bona-fide mother
	isBirth = x$EventCode == "BTH"
	dlvID_off = dlvID_mom = x$DeliveryId
	dlvID_off[!isBirth] = NA
	dlvID_mom[!isDelivery] = NA
	ndx = !is.na(match(dlvID_off, dlvID_mom))
	ndx[!isBirth] = NA
	ret = addRecTest(ret, recTest(ndx, "Birth event linked to valid delivery"))	

	## Event number not greater than event count
	ret
}

#' @rdname recTests-HDSS
#' @export
vaRecordTests = function(x)
{

}
