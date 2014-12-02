#' Pre-process raw HDSS data
#'
#' This function cleans up some common data variants that appear in raw
#' HDSS data files.
#'
#' @details The function undertakes the following modifications:
#' \itemize{
#'     \item empty dates are set to \code{NA};
#'     \item dates get stripped of trailing time stamps and converted to
#'           \code{Date}-variables, regardless of the separator used;
#'     \item \code{Sex} is coded as an f/m factor;
#'     \item for verbal autopsy variables, blanks are replaced with NAs;
#'     \item cause of death codes are matched against the canonical list of
#'           approved causes (based on WHO standard).
#' }
#'
#' @param x a data frame with raw HDSS data
#' @param checkVA A logical flag indicating whether to test for the presence and
#' validity of verbal autopsy data. 
#'
#' @return A data frame with modified columns.
#' @seealso \code{\link{readRawHDSS}} \code{\link{coreTests}}
#' @export
preprocHDSS = function(x, checkVA=FALSE)
{
	## Dates
	datevars = c("DoB", "EventDate", "ObservationDate")
	if ( !all(datevars %in% colnames(x)) ) {
		stop("Not all date variables in data frame 'x'")
	}
	for (dd in datevars) {
		x[, dd] = extractDate(x[, dd])
	}

	## Sex
	if ( !("Sex" %in% colnames(x)) ) {
		stop("No variable 'Sex' in data frame 'x'")
	}
	if ( !is.factor(x$Sex) ) {
		x$Sex = factor(x$Sex, levels=1:2, labels=c("m", "f"))
	} else {
		x$Sex = factor(x$Sex, levels=c("m", "f"))
	}

	## Unblank COD
	if (checkVA) {
		cvars = paste("Cause", 1:3, sep="")
		lvars = paste("Likelihood", 1:3, sep="")
		if ( !all(c(cvars, lvars) %in% colnames(x)) ) {
			warning("Not all verbal autopsy variables in data frame 'x'")
		} else {
			for (v in c(cvars, lvars)) {
				ndx = as.character(x[, v]) == "" & !is.na(x[, v])
				x[ndx, v] = NA
			}
		}
		## Match the causes of death to the approved ones
		if ( !all(cvars %in% colnames(x)) ) {
			warning("Not all cause-of-death variables in data frame 'x'")
		} else {
			for (v in cvars) {
				x[ , v] = extractVAcode(x[, v])
			}
		}
	}

	class(x) = c("HDSSdata", "data.frame")
	x
}

#' Standardize raw HDSS variable
#'
#' These functions attempt to extract standardized data from specific variables
#' that may appear with variants in different HDSS sets
#'
#' @details The function undertakes the following modifications:
#' \itemize{
#'     \item dates get stripped of trailing time stamps and converted to
#'           \code{Date}-variables, regardless of the separator used;
#'     \item \code{Sex} is coded as an f/m factor;
#'     \item for verbal autopsy variables, blanks are replaced with NAs;
#'     \item cause of death codes are matched against the canonical list of
#'           approved causes (based on WHO standard).
#' }
#'
#' @note These functions will terminate with an error if their input is found to
#'   be too irregular for standardization
#'
#' @param x the variable to be processed (date, cause of death)
#'
#' @return For \code{extractDate}, a vector of \code{Date}s;
#'   for \code{extractVAcode}, a factor variable with standardized VA codes
#' @seealso \code{\link{preprocHDSS}} \code{\link{INDEPTH_verbalAutopsy}}
#' @name standardize-HDSS-var
#' @export
extractDate = function(x)
{
	## Work with chars
	xc = as.character(x)
	
	## Empty date strings are a nuisance; they are set to NA, and someone
	## else can deal with them downstream; we only process the non-missing
	ret = rep(as.Date(NA), length(x))	
	ndx = nchar(xc) > 0 & !is.na(xc)
	xc  = xc[ndx]
	## We bail if *all* dates are missing
	if ( length(xc) == 0) return(ret)
	## Still here? Continue
	nc = unique(nchar(xc))  
	if (length(nc) > 1) {
		stop("Date variable with different char length of dates")
	}

	## Truncate in case of trailing times
	xc = substr(xc, 1, 10)

	## Check the separators
	sep1 = unique(substr(xc, 5, 5))
	sep2 = unique(substr(xc, 8, 8))
	if ( (length(sep1) > 1) | (length(sep2) > 1) ) {
		stop("Non-constant separators in time variables")
	}
	if (sep1 != sep2) {
		stop("Different separators in the same date specification")
	}

	## Convert
	format = paste("%Y", sep1, "%m", sep2, "%d", sep="")
	dd = as.Date(xc, format=format)

	## Check for invalid dates
	if ( any(is.na(dd)) ) {
		stop("Unsuccessful date conversion")
	}

	## Ok then
	ret[ndx] = dd
	ret
}
	
#' @name standardize-HDSS-var
#' @export
extractVAcode = function(x)
{
	xc = as.character(x)
	## Official dd.dd codes (may be fusion)
	co = substr(xc, 1, 5)
	## For three-digits codes: remove two non-digits at the end
	co = sub("[^0-9][^0-9]$", "", co)
	## For two-digits codes: remove another non-digits at the end
	## This will also cover two-letter codes (XX)
	co = sub("[^0-9]$", "", co)
	ndxmatch = match(co, as.character(INDEPTH_verbalAutopsy$Code))
	comatch  = INDEPTH_verbalAutopsy$mappedCode[ndxmatch]
	comatch
}

#' Filter out valid observations in an HDSS data set
#'
#' Runs a number of filters on a pre-processed data set to identify questionable
#' events (before birth, in the future etc.)
#'
#' @param x the pre-processed HDSS data frame to filter
#'
#' @return A list with three entries:
#' \itemize{
#'   \item{report}{a data frame with one row per filter, describing the filtering
#'                 step and listing the number of events removed by it}
#'   \item{keep}{the record numbers (\code{RecNr}) of the events to be kept
#'               after filtering}
#'   \item{remove}{a list of vectors of record numbers, one vector per filter
#'                 step, identifying events that are dropped in this step}
#' }
#'
#' @seealso \code{\link{contenTests}}
#' @export
filterHDSS = function(x)
{

	## Obervation after event

	## Reasonable range of event dates

	## Reasonable range of ages at event

	## Birth before event date	

	## All mothers are female

	## Event number not greater than event count

	## Incorrect sex




}



	
