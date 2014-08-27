#' Check HDSS data
#'
#' These functions test whether a given data frame is (similar to)
#' a valid HDSS data set. 
#'
#' @param x The data frame to be validated
#'
#' @details The functions provide a pragmatic way of testing core requirements
#'   for valid HDSS data. This is not a validation of a formal HDSS
#'   specification, but rather a set of reasonable tests that allow the user to
#'   identifity data quality issues and to fix them on a case-to-case base. \cr\cr
#'   \code{coreTests} checks for the presence of the basic variables specified
#'   in Sankoh & Byass (2012), as well as some of their properties (presence
#'   of missing values, sequential numbers, identifier formats etc.). This
#'   function can highlight problems with raw data read in directly via
#'   \code{readRawHDSS}. \cr\cr
#'   \code{vaTests} checks for the presence and validity of verbal autopsy
#'   variables (given as \code{Cause1} to \code{Cause3} and \code{Likelihood1}
#'   to \code{Likelihood3}. This is not part of the HDSS specification, and may
#'   or may not be present in a data set. The function makes some assumptions
#'   about the presence of core variables (especially \code{EventCode}), so it
#'   is good practive to run the data through \code{coreTests} first. \cr\cr
#'   \code{contentTests} tests some more assumptions about the core HDSS variables,
#'   assuming that the raw data has been suitably preprocessed using
#'   \code{preprocHDSS}, resulting e.g. in valid date variables throughout.
#'
#' @return Each test returns a list with two entries:
#'   \itemize{
#'     \item{\code{flag}}{ a logical flag hat indicates whether all tests were passsed or not}
#'     \item{\code{text}}{ a data frame with one column that contains the error messages from the failed tests; empty if all tests were passed}
#'   }
#'
#' @references Osman Sankoh and Peter Byass. The INDEPTH Network: filling vital gaps in global epidemiology. Int J Epi 2012; 41:579-588, Tables 1 and 2.
#' @seealso \code{\link{var-check-tools}} \code{\link{readRawHDSS}}
#' @name validate-HDSS
#' @export
#'
coreTests = function(x)
{
	## Initialization
	allTests = list()
	
	## Check for unique non-missing sequential record identifier
	test = varExistsNoMissing("RecNr", x)
	allTests$RecNr01 = test
	if (test$flag) {
		allTests$RecNr02 = varNoDuplicate("RecNr", x)
		allTests$RecNr03 = varIsSequential("RecNr", x)
	}

	## Check for valid non-missing centre identifiers of length 5
	test = varExistsNoMissing("CentreId", x)
	allTests$CentreID01 = test
	if (test$flag) {
		if ( any(nchar(as.character(x$CentreID)) != 5) ) {
			test = list(flag = FALSE, text ="Invalid code length in variable 'CentreId'")
		} else {
			test = list(flag = TRUE, text = "ok")
		}
		allTests$CentreID02 = test
	}

	## Check for non-missing subject Ids
	allTests$IndividualID01 = varExistsNoMissing("IndividualId", x)

	## Check the sex of subjects
	test = varExistsNoMissing("Sex", x)
	allTests$Sex01 = test
	if (test$flag) {
		if (is.factor(x$Sex)) {
			val = c("m", "f")
		} else{
			val = 1:2
		}
		allTests$Sex02 = varValidValues("Sex", x, val)
	}

	## Check for country identifier: present and valid
	test = varExistsNoMissing("CountryId", x)
	allTests$CountryID01 = test
	if (test$flag) {
		require(ISOcodes)
		data(ISO_3166_1)
		if ( !all(as.character(x$CountryID) %in% ISO_3166_1$Numeric) ) {
			test = list(flag = FALSE, text = "Invalid country code in variable 'CountryId'")
		} else {
			test = list(flag = TRUE, text = "ok")
		}
		allTests$CountryID02 = test
	}

	## Check for non-missing location Ids
	allTests$LocationID01 = varExistsNoMissing("LocationId", x)

	## Check for non-missing date of birth
	allTests$DoB = varExistsNoMissing("DoB", x)

	## Check for non-missing event code from given list
	test = varExistsNoMissing("EventCode", x)
	allTests$EventCode01 = test
	if (test$flag) {
		allTests$EventCode02 = varValidValues("EventCode", x, val=INDEPTH_eventCodes)
	}

	## Check for non-missing event date
	allTests$EventData01 = varExistsNoMissing("EventDate", x)

	## Check for non-missing observation date
	allTests$ObservationDate01 = varExistsNoMissing("ObservationDate", x)

	## Check for non-missing count of events
	allTests$EventCount01 = varExistsNoMissing("EventCount", x)

	## Check for non-missing event number (sequential within subject?)
	allTests$EventNr01 = varExistsNoMissing("EventNr", x)

	flag = all(sapply(allTests, function(x) x$flag))
	if (flag) {
		text = NULL
	} else {
		text = unlist(sapply(allTests, function(x) if (!x$flag) x$text else NULL))
		text = text[text != "ok"]
	}
	list(flag = flag, text = data.frame(Message=text))

}


#' @rdname validate-HDSS
#' @export
vaTests = function(x)
{
	## Initialization
	allTests = list()
	
	## Test for primary causes
	test1 = varExists("Cause1", x)
	test2 = varExists("Likelihood1", x)
	allTests$Cause1_01 = test1
	allTests$Cause1_02 = test2
	if (test1$flag & test2$flag) {
		allTests$Cause1_03 = varMatchingMissing("Cause1", "Likelihood1", x)
	}
	
	## Test for secondary causes
	test1 = varExists("Cause2", x) 
	test2 = varExists("Likelihood2", x)
	allTests$Cause2_01 = test1
	allTests$Cause2_02 = test2
	if (test1$flag & test2$flag) {
		allTests$Cause2_03 = varMatchingMissing("Cause2", "Likelihood2", x)
	}
	
	## Test for tertiary causes
	test1 = varExists("Cause3", x)
	test2 = varExists("Likelihood3", x)
	allTests$Cause3_01 = test1
	allTests$Cause3_02 = test2
	if (test1$flag & test2$flag) {
		allTests$Cause3_03 = varMatchingMissing("Cause3", "Likelihood3", x)
	}
	
	flag = all(sapply(allTests, function(x) x$flag))
	if (flag) {
		text = NULL
	} else {
		text = unlist(sapply(allTests, function(x) if (!x$flag) x$text else NULL))
		text = text[text != "ok"]
	}
	
	list(flag = flag, text = data.frame(Message=text))
}

#' @rdname validate-HDSS
#' @export
contentTests = function(x)
{
	## Obervation after event

	## Reasonable range of event dates

	## Reasonable range of ages at event

	## Birth before event date	

	## All mothers are female

	## Event number not greater than event count

}

	## 
