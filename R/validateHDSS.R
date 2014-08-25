#' Test and clean raw HDSS data
#'
#' This function tests whether a given data frame fulfills core requirements of
#' a valid HDSS data set. Optionally, it alos cleans up minor inconsistencies
#' concerning coding of dates etc.
#'
#' @param x The data frame to be validated
#' @param clean A logical flag indicating whether to clean up variables 
#'  (\code{TRUE}, the default), or whether just to test the validity
#' @param inclVA A logical flag indicating whether to check for verbal autopsy
#'  results in the data (default \code{TRUE}).
#'
#' @return If \code{clean} is \code{TRUE}, the cleaned data frame; if \code{clean}
#' is \code{FALSE}, a logical value idnicating whether or not \code{x} follows
#' the specification
#'
#' @references Osman Sankoh and Peter Byass. The INDEPTH Network: filling vital gaps in global epidemiology. Int J Epi 2012; 41:579-588, Tables 1 and 2.
#' @seealso \code{\link{var-check-tools}}
#' @export
#'
validateHDSS = function(x, clean=TRUE, inclVA=TRUE)
{
	## nitialization
	allTests = list()
	add = function(List, new) { List[[length(List)+1]] = new; List }
	
	## Check for unique non-missing sequential record identifier
	test = varExists("RecNr", x)
	allTests = add(allTests, test)
	if (test$flag) {
		allTests = add(allTests, varNoMissing("RecNr", x) )
		allTests = add(allTests, varNoDuplicate("RecNr", x) )
		allTests = add(allTests, varIsSequential("RecNr", x) )
	}

	## Check for valid non-missing centre identifiers of length 5
	test = varExistsNoMissing("CentreId", x)
	allTests = add(allTests, test)	
	if (test$flag) {
		if ( any(nchar(as.character(x$CentreID)) != 5) ) {
			test = list(flag = FALSE, text ="Invalid code length in variable 'CentreId'")
		} else {
			test = list(flag = TRUE, text = "ok")
		}
		allTests = add(allTests, test)						
	}

	## Check for non-missing subject Ids
	allTests = add(allTests, varExistsNoMissing("IndividualId", x) )

	## Check for country identifier: present and valid
	test = varExistsNoMissing("CountryId", x)
	allTests = add(allTests, test)		
	if (test$flag) {
		require(ISOcodes)
		data(ISO_3166_1)
		if ( !all(as.character(x$CountryID) %in% ISO_3166_1$Numeric) ) {
			test = list(flag = FALSE, text = "Invalid country code in variable 'CountryId'")
		} else {
			test = list(flag = TRUE, text = "ok")
		}
		allTests = add(allTests, test)						
	}

	## Check for non-missing location Ids
	allTests = add(allTests, varExistsNoMissing("LocationId", x) )

	## Check for non-missing date of birth
	allTests = add(allTests,  varExistsNoMissing("DoB", x) )

	## Check for non-missing event code from given list
	test = varExistsNoMissing("EventCode", x)
	allTests = add(allTests, test)
	if (test$flag) {
		allTests = add(allTests, varValidValues("EventCode", x, val=INDEPTH_eventCodes) )
	}

	## Check for non-missing event date
	allTests = add(allTests, varExistsNoMissing("EventDate", x) )

	## Check for non-missing observation date
	allTests = add(allTests, varExistsNoMissing("ObservationDate", x)	)

	## Check for non-missing count of events
	allTests = add(allTests, varExistsNoMissing("EventCount", x) )

	## Check for non-missing event number (sequential within subject?)
	allTests = add(allTests, varExistsNoMissing("EventNr", x) )

	## Only if we are reuqired to test for verbal autospy results
	if (inclVA) {

		##isGood = isGood & varExists("




	}
	
	if (clean) {
		return(x)
	} else {
		flag = all(sapply(allTests, function(x) x$flag))
		if (flag) {
			text = "ok"
		} else {
			text = unlist(sapply(allTests, function(x) if (!x$flag) x$text else NULL))
			text = text[text != "ok"]
		}
		return(list(flag = flag, text = text))
	}

}
