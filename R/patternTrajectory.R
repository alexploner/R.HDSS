#' Extract patterns of events over time
#'
#' Given a pre-processed HDSS data set, return the sequence of events for
#' each individual subject, or the first/last events respectively
#'
#' @param x A pre-processed HDSS data set
#' @param sep A character expression that is used to separate events in the pattern
#'
#' @seealso \code{\link{preprocHDSS}}
#' 
#' @return Either a character vector of event sequences, one for each individual,
#' or a character of first/last events (by event date), one for each individual
#' @export
getPatterns = function(x, sep="->")
{
	require(dplyr, quietly=TRUE)
	pat <- group_by(x, IndividualId) %>%
	       summarise(Pattern = paste(EventCode, collapse=sep)) %>%
	       extract2("Pattern")
	pat
}

#' @rdname getPatterns
#' @export
firstEvents = function(x)
{
	pat = strsplit(getPatterns(x), "->")
	sapply(pat, function(x) x[1])
}	
	
#' @rdname getPatterns
#' @export
lastEvents = function(x)
{
	pat = strsplit(getPatterns(x), "->")
	sapply(pat, function(x) x[length(x)])
}	

#' Extract transitions between events
#'
#' Given a pre-processed HDSS data set, return matrices describing transitions
#' between events, both counts and aggregated times between events
#'
#' @param x A pre-processed HDSS data set
#'
#' @seealso \code{\link{preprocHDSS}}
#' 
#' @return A list with two numeric matrices: rows and columns are indexed by the
#' event codes, with rows indicating the from-state and columns the to-states.
#' The first list element is called \code{counts}, and contains the number of
#' transitions from one state to the next; the second element is called
#' \code{deltaT} and contains the total time spent between the from- and the
#' to-states.
#'
#' @export
getTransitions = function(x)
{
	## Crucial to convert the event codes
	ID = x$IndividualId
	EC = as.character(x$EventCode)
	ED = x$EventDate

	## Initialize the transition matrix
	nn = length(INDEPTH_eventCodes)
	trans = matrix(0, nrow=nn, ncol=nn)
	rownames(trans) = colnames(trans) = INDEPTH_eventCodes
	deltaT =trans

	## Loop over transitions
	nn = nrow(x)
	curid  = ID[1]
	curdat = ED[1]
	from   = EC[1]
	for (i in 2:nn) {
		newid  = ID[i]
		newdat = ED[i]
		to     = EC[i]
		if (curid == newid) {
			trans[from, to] = trans[from,to] + 1
			deltaT[from, to] = deltaT[from, to] + (newdat-curdat)
		}
		curid  = newid
		curdat = newdat
		from  = to
	}
	list(counts=trans, deltaT=deltaT)
}


