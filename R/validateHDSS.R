#' Test raw HDSS data
#'
#' This function tests whether a given data frame fulfills core requirements of
#' a valid HDSS data set. 
#'
#' @param x The data frame to be validated
#' @param inclVA A logical flag indicating whether to check for verbal autopsy
#'  results in the data (default \code{TRUE}).
#'
#' @return A list with two entries: a logical flag \code{flag} that is \code{TRUE}
#'   if all tests were passed and \code{FALSE} otherwise, and data frame \code{text}
#'   of diagnostic messages from the failed tests. 
#'
#' @references Osman Sankoh and Peter Byass. The INDEPTH Network: filling vital gaps in global epidemiology. Int J Epi 2012; 41:579-588, Tables 1 and 2.
#' @seealso \code{\link{var-check-tools}}
#' @export
#'
validateHDSS = function(x, inclVA=TRUE)
{
	## Initialization
	allTests = list()
	add = function(List, new) { List[[length(List)+1]] = new; List }
	
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
		allTests$Sex02 = varValidValues("Sex", x, 1:2)
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

	## Only if we are reuqired to test for verbal autospy results
	if (inclVA) {
		
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
