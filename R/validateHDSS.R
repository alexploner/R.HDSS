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
	test = varExists("CentreId", x, verb=verb)
	if (test) {
		test = test & varNoMissing("CentreId", x, verb=verb)
		if ( any(nchar(as.character(x$CentreID)) != 5) ) {
			test = FALSE
			if (verb) warning("Invalid code length in variable 'CentreId'")
		}
	}
	isGood = isGood & test

	## Check for non-missing subject Ids
	test = varExists("IndividualId", x, verb=verb)
	if (test) {
		test = test & varNoMissing("IndividualId", x, verb=verb)
	}
	isGood = isGood & test

	## Check for country identifier: present and valid
	test = varExists("CountryId", x, verb=verb)	
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
	test = varExists("LocationId", x, verb=verb)
	if (test) {
		test = test & varNoMissing("LocationId", x, verb=verb)
	}
	isGood = isGood & test

	## Check for non-missing date of birth
	test = varExists("DoB", x, verb=verb)
	if (test) {
		test = test & varNoMissing("DoB", x, verb=verb)
	}
	isGood = isGood & test

	## Check for non-missing event code from given list
	test = varExists("EventCode", x, verb=verb)
	if (test) {
		test = test & varNoMissing("EventCode", x, verb=verb)
		code = c("BTH", "ENU", "IMG", "OMG", "EXT", "ENT", "DTH", "DLV",
		         "OBE", "OBL", "OBS")
		if ( !all(as.character(x$EventCode) %in% code) ) {
			test = FALSE
			if (verb) warning("Unrecognized code in variable 'EventCode'")
		}
	}
	isGood = isGood & test

	## Check for non-missing event date
	test = varExists("EventDate", x, verb=verb)
	if (test) {
		test = test & varNoMissing("EventDate", x, verb=verb)
	}
	isGood = isGood & test

	## Check for non-missing observation date
	test = varExists("ObservationDate", x, verb=verb)
	if (test) {
		test = test & varNoMissing("ObservationDate", x, verb=verb)
	}
	isGood = isGood & test

	## Check for non-missing count of events
	test = varExists("EventCount", x, verb=verb)
	if (test) {
		test = test & varNoMissing("EventCount", x, verb=verb)
	}
	isGood = isGood & test

	## Check for non-missing event number (sequential within subject?)
	test = varExists("EventNr", x, verb=verb)
	if (test) {
		test = test & varNoMissing("EventNr", x, verb=verb)
	}
	isGood = isGood & test

	if (clean) return(x) else return(isGood)

}
