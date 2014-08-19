#' Tools for checking variables in a data frame
#'
#' These functions check for existence and properties of a specified variable
#' name in a given data frame.
#'
#' @param v a character expression giving the full variable name
#' @param x the data frame where to check
#' @param verb a logical flag indicating whether to issue a warning if the test
#'        fails (default \code{TRUE})
#'
#' @return a logical value indicating whether the condition holds
#'
#' @name var-check-tools
NULL

#' @rdname var-check-tools
varExists = function(v, x, verb=TRUE)
{
	ret = TRUE
	if ( !( v %in% colnames(x) ) ) {
		if (verb) warning("Required variable '", v, "' not found")
		ret = FALSE
	}
	ret
}

#' @rdname var-check-tools
varNoMissing = function(v, x, verb=TRUE)
{
	ret = TRUE
	if ( any(is.na(x[,v])) ) {
		if (verb) warning("Variable '", v, "' has missing values")
		ret = FALSE
	}
	ret
}

#' @rdname var-check-tools
varNoMissing = function(v, x, verb=TRUE)
{
	ret = TRUE
	if ( any(is.na(x[,v])) ) {
		if (verb) warning("Variable '", v, "' has missing values")
		ret = FALSE
	}
	ret
}

#' @rdname var-check-tools
varNoDuplicate = function(v, x, verb=TRUE)
{
	ret = TRUE
	if ( any(duplicated(x[,v])) ) {
		if (verb) warning("Variable '", v, "' has non-unique values")
		ret = FALSE
	}
	ret
}

#' @rdname var-check-tools
varIsSequential = function(v, x, verb=TRUE)
{
	ret = TRUE
	if ( !all(diff(x[,v]) == 1) ) {
		if (verb) warning("Variable '", v, "' has not sequential")
		ret = FALSE
	}
	ret
}


