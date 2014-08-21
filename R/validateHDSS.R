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
#' @param verb A logical flag indicating whether to issue explicit warnings when
#'  a deviation from the specification is found (default \code{TRUE})
#'
#' @return If \code{clean} is \code{TRUE}, the cleaned data frame; if \code{clean}
#' is \code{FALSE}, a logical value idnicating whether or not \code{x} follows
#' the specification
#'
#' @references Osman Sankoh and Peter Byass. The INDEPTH Network: filling vital gaps in global epidemiology. Int J Epi 2012; 41:579-588, Tables 1 and 2.
#' @seealso \code{\link{var-check-tools}}
#' @export
#'
validateHDSS = function(x, clean=TRUE, inclVA=TRUE, verb=TRUE)
{
	## Hopeful initialization
	isGood = TRUE
	
	## Check for unique non-missing sequential record identifier
	test = varExists("RecNr", x, verb=verb)
	if (test) {
		test = test & varNoMissing("RecNr", x, verb=verb)
		test = test & varNoDuplicate("RecNr", x, verb=verb)
		test = test & varIsSequential("RecNr", x, verb=verb)
	}
	isGood = isGood & test

	## Check for valid non-missing centre identifiers of length 5
	test = varExistsNoMissing("CentreId", x, verb=verb)
	if (test) {
		if ( any(nchar(as.character(x$CentreID)) != 5) ) {
			test = FALSE
			if (verb) warning("Invalid code length in variable 'CentreId'")
		}
	}
	isGood = isGood & test

	## Check for non-missing subject Ids
	isGood = isGood & varExistsNoMissing("IndividualId", x, verb=verb)	

	## Check for country identifier: present and valid
	test = varExistsNoMissing("CountryId", x, verb=verb)	
	if (test) {
		require(ISOcodes)
		data(ISO_3166_1)
		if ( !all(as.character(x$CountryID) %in% ISO_3166_1$Numeric) ) {
			test = FALSE 
			if (verb) warning("Invalid country code in variable 'CountryId'")
		}
	}
	isGood = isGood & test

	## Check for non-missing location Ids
	isGood = isGood & varExistsNoMissing("LocationId", x, verb=verb)	

	## Check for non-missing date of birth
	isGood = isGood & varExistsNoMissing("DoB", x, verb=verb)	

	## Check for non-missing event code from given list
	test = varExistsNoMissing("EventCode", x, verb=verb)
	if (test) {
		test = test & varValidValues("EventCode", x, val=INDEPTH_eventCodes, verb=verb)
	}
	isGood = isGood & test

	## Check for non-missing event date
	isGood = isGood & varExistsNoMissing("EventDate", x, verb=verb)

	## Check for non-missing observation date
	isGood = isGood & varExistsNoMissing("ObservationDate", x, verb=verb)	

	## Check for non-missing count of events
	isGood = isGood & varExistsNoMissing("EventCount", x, verb=verb)	

	## Check for non-missing event number (sequential within subject?)
	isGood = isGood & varExistsNoMissing("EventNr", x, verb=verb)

	## Only if we are reuired to test for verbal autospy results
	if (inclVA) {

		##isGood = isGood & varExists("




	}
	

	if (clean) return(x) else return(isGood)

}
