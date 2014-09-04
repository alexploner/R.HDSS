#' Check pre-processed HDSS data
#'
#' These functions test and report which records of a pre-processed HDSS data
#' have valid and consistent values
#'
#' @param x The data frame to be validated
#'
#' @details These functions reprot on the content of a pre-processed HDSS data
#'   set (as opposed to \code{coreVariableTests} and \code{vaVariableTests},
#'   which check the formal requirements for a raw HDSS data set). \cr\cr
#'   \code{coreRecordTests} checks for valid codes, missing values introduced by
#'   pre-processing, proper sequence of events, reasonable ranges of dates and
#'   ages, proper counting of events and female mothers. This is done for the
#'   same set of core variables as in \code{coreVariableTests}. \cr\cr
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
coreRecordTests = function(x)
{
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
	ret = x %>% recTest_InRange("DoB", as.Date("1890-01-01"), as.Date("2014-01-01")) %>% addRecTest(ret, .)				

	ret = x %>% recTest_Missing("EventDate") %>% addRecTest(ret, .)
	ret = x %>% recTest_InRange("EventDate", as.Date("1990-01-01"), as.Date("2014-01-01")) %>% addRecTest(ret, .)				

	ret = x %>% recTest_Missing("ObservationDate") %>% addRecTest(ret, .)
	ret = x %>% recTest_InRange("ObservationDate", as.Date("1990-01-01"), as.Date("2014-01-01")) %>% addRecTest(ret, .)				

	## Check the order in which things happen
	ret = x %>% recTest_LessOrEqual("DoB", "EventDate") %>% addRecTest(ret, .)
	ret = x %>% recTest_LessOrEqual("EventDate", "ObservationDate") %>% addRecTest(ret, .)

	## Reasonable range of ages at event

	## All mothers are female

	## Event number not greater than event count

	ret
}

#' @rdname recTests-HDSS
#' @export
vaRecordTests = function(x)
{

}
