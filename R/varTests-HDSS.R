#' Check raw HDSS data
#'
#' These functions test whether a given dataframe read by \code{readRawHDSS} is
#' a valid HDSS data set in terms of variables present and a valid unique
#' identifier.
#'
#' @param x The data frame to be validated
#'
#' @details These functions provide a pragmatic way of testing core requirements
#'   for valid HDSS data. While is not a full validation of a formal HDSS
#'   specification, it is a set of reasonable tests: a data set that fails them
#'   will likely need manual intervention before further processing; a data set
#'   that passes them can be pre-processed before checking the actual content
#'   of the variables (see e.g. \code{coreRecTests}). \cr\cr
#'   \code{coreVariableTests} checks for the presence of the basic variables
#'   specified in Sankoh & Byass (2012), the presence of a valid unique record
#'   identifier, and for some crucial variables also the presence of missing
#'   values. \cr\cr
#'   \code{vaVariableTests} checks for the presence of verbal autopsy
#'   variables (given as \code{Cause1} to \code{Cause3} and \code{Likelihood1}
#'   to \code{Likelihood3}. This is not part of the core HDSS specification, and
#'   may or may not be present in a data set. \cr\cr
#'
#' @return Each test returns a list with two entries:
#'   \itemize{
#'     \item{\code{flag}}{ a logical flag hat indicates whether all tests were passsed or not}
#'     \item{\code{text}}{ a data frame with one column that contains the error messages from the failed tests; empty if all tests were passed}
#'   }
#'
#' @references Osman Sankoh and Peter Byass. The INDEPTH Network: filling vital gaps in global epidemiology. Int J Epi 2012; 41:579-588, Tables 1 and 2.
#' @seealso \code{\link{var-check-tools}} \code{\link{readRawHDSS}}
#'          \code{\link{preprocHDSS}} \code{\link{coreRecTests}}
#' @name varTests-HDSS
#' @export
#'
coreVariableTests = function(x)
{
	## We need a unique, sequential key
	ret = x %>% varTest_Exists("RecNr")
	if ( last(ret$Pass) ) {
		ret = x %>% varTest_NoMissing("RecNr") %>% rbind(ret, .)
		ret = x %>% varTest_NoDuplicate("RecNr") %>% rbind(ret, .)
		ret = x %>% varTest_IsSequential("RecNr") %>% rbind(ret, .)
	}

	## Check for valid non-missing constant country identifier
	ret = x %>% varTest_Exists("CountryId") %>% rbind(ret, .)
	if ( last(ret$Pass) ) {
		ret = x %>% varTest_NoMissing("CountryId") %>% rbind(ret, .)
		ret = x %>% varTest_IsConstant("CountryId") %>% rbind(ret, .)
	}
	
	## Check for valid non-missing constant centre identifier
	ret = x %>% varTest_Exists("CentreId") %>% rbind(ret, .)
	if ( last(ret$Pass) ) {
		ret = x %>% varTest_NoMissing("CentreId") %>% rbind(ret, .)
		ret = x %>% varTest_IsConstant("CentreId") %>% rbind(ret, .)
	}

	ret = x %>% varTest_ExistsNoMissing("IndividualId") %>% rbind(ret, .)
	ret = x %>% varTest_ExistsNoMissing("Sex") %>% rbind(ret, .)
	ret = x %>% varTest_ExistsNoMissing("LocationId") %>% rbind(ret, .)
	ret = x %>% varTest_ExistsNoMissing("DoB") %>% rbind(ret, .)
	ret = x %>% varTest_ExistsNoMissing("EventCode") %>% rbind(ret, .)
	ret = x %>% varTest_ExistsNoMissing("EventDate") %>% rbind(ret, .)
	ret = x %>% varTest_ExistsNoMissing("ObservationDate") %>% rbind(ret, .)
	ret = x %>% varTest_ExistsNoMissing("EventCount") %>% rbind(ret, .)
	ret = x %>% varTest_ExistsNoMissing("EventNr") %>% rbind(ret, .)

	ret
}


#' @rdname validate-HDSS
#' @export
vaVariableTests = function(x)
{
	ret = x %>% varTest_Exists("Cause1")
	ret = x %>% varTest_Exists("Cause2") %>% rbind(ret, .)
	ret = x %>% varTest_Exists("Cause3") %>% rbind(ret, .)
	ret = x %>% varTest_Exists("Likelihood1") %>% rbind(ret, .)
	ret = x %>% varTest_Exists("Likelihood2") %>% rbind(ret, .)
	ret = x %>% varTest_Exists("Likelihood3") %>% rbind(ret, .)

	ret
}

