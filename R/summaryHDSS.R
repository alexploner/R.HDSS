#' Summary of pre-processed HDSS data
#'
#' @param x A pre-processed HDSS data file, as returned by
#'          \code{\link{preprocHDSS}}
#'
#' @return A data frame with key information as columns
#'
#' @export
summary.HDSSdata = function(x)
{
	require(dplyr)
	country <- x %>% slice(1) %>% semi_join(INDEPTH_Centres, ., by=c("ISOcode"="CountryId")) %>% extract2("Country") %>% unique()
	## To avoid stupid warning message
	## Still warning if x$CentreId has unknown code
	levs = unique(c(levels(INDEPTH_Centres$Code), levels(x$CentreId)))
	levels(x$CentreId) = levs
	site    <- x %>% slice(1) %>% semi_join(INDEPTH_Centres, ., by=c("Code"="CentreId")) %>% extract2("Site")	
	numrec  = nrow(x)
	numsub  = length(unique(x$IndividualId))
	i2d = function(i) as.Date(i, origin="1970-01-01")	
	mindate = i2d(min(x$EventDate))
	maxdate = i2d(max(x$EventDate))
	ret = list(Country = country, Site=site, "Individuals"=numsub, "Events"=numrec, "Begin"=mindate, "End"=maxdate)
	as.data.frame(ret, optional=TRUE)
}

#' Summarize a pre-processed HDSS data set
#'
#' Summarize key parameters of a pre-processed HDSS data set.
#'
#' @param x The pre-processed HDSS data frame to be summarized
#'
#' @details \code{summarizeHDSS} works for a single HDSS data set,
#'    \code{overviewHDSS} for a list of pre-processed data sets. Assuming that
#'    the list of HDSS files is based on raw data files following the INDEPTH
#'    naming conventions and read in by \code{readRawHDSS} before pre-processing,
#'    \code{overviewHDSS} tires to guess at the version of the data.
#'
#' @return For \code{summarizeHDSS} a named list of parameters, for
#'    \code{overviewHDSS} a data frame with centres/data files as rows and
#'    the parameters as columns.
#'
#' @name summarize-HDSS
#' @seealso \code{\link{preprocHDSS}}
#' @export
summarizeHDSS = function(x)
{
	centreId  = levels(x$CentreId)[1]
	country   = with(INDEPTH_Centres, Country[match(centreId, Code)])
	site      = with(INDEPTH_Centres,Site[match(centreId, Code)])
	numEvents = nrow(x)
	numSubj   = length(unique(x$IndividualId))
	dateRange = range(x$EventDate, na.rm=TRUE)
	ageRange  = range(round(as.vector(x$EventDate - x$DoB)/365.25,1), na.rm=TRUE)
	numDeaths = length(which(x$EventCode=="DTH"))
	numBirths = length(which(x$EventCode=="BTH"))
	vaVars = as.vector(outer(c("Cause", "Likelihood"), 1:3, paste, sep=""))
	hasVA     = all(vaVars %in% colnames(x))
	list("Code" = centreId, "Country" = country, "Site" = site,
	      "numSubjects" = numSubj, "numEvents" = numEvents,
	     "minDate" = dateRange[1], "maxDate" = dateRange[2],
	     "minAge"  = ageRange[1],  "maxAge"  = ageRange[2],
	     "numBirths" = numBirths, "numDeaths" = numDeaths,
	     "hasVerbalAutopsy" = hasVA)
}	     
	     
#' @rdname summarize-HDSS
#' @param l a list of pre-processed HDSS data files
#' @export
overviewHDSS = function(l)
{
	ll = lapply(l, function(x) as.data.frame(summarizeHDSS(x)))
	nn = length(ll)
	ret = ll[[1]]
	if (nn > 1) {
		for (i in 2:nn) {
			ret = rbind(ret, ll[[i]])
		}
	}
	rownames(ret) = names(ll)
	ret
}
