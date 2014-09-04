#' Output from a record test
#' 
#' A simple class that wraps the outpur from a record test like e.g.
#' \code{recTest_Missing} and comes with a print/summary method.
#'
#' @param Index logical vector of test results
#' @param Desc character expression describing the test
#' @param x,y an object of class \code{recTest}
#'
#' @details Internally, this is a list with two entries: a logical matrix named
#'   \code{Index} with as many rows as records and as many columsn as tests, and
#'   a character vector \code{Desc} with one entry per test. The constructor
#'   \code{recTest} takes just one test result, but multiple tests can be
#'   combined using \code{addRecTest}.
#' 
#' @return An object of class \code{recTest} (invisibly for the print-method)
#'
#' @name recTest-class
#' @seealso \code{\link{recTest_Missing}}
#' @export
recTest = function(Index, Desc)
{
	if (!is.logical(Index)) stop("Index must be logical vector")
	if (!is.character(Desc)) stop("Desc must be a character expression")
	if (length(Desc) > 1) {
		warning("Only first element of argument Desc is used")
		Desc = Desc[1]
	}
	ret = list(Index = matrix(Index, ncol=1), Desc = Desc)
	class(ret) = "recTest"
	ret
}

#' @rdname recTest-class
#' @export
print.recTest = function(x, ...)
{
	ret = summary(x)
	print(ret, ...)
	invisible(x)
}

#' @rdname recTest-class
#' @export
summary.recTest = function(x, ...)
{
	tabfun = function(x) table(factor(x, level=c(TRUE, FALSE)), useNA="always")
	ret = t(apply(x$Index, 2, tabfun))
	ret = data.frame(Description=x$Desc, ret, check.names=FALSE)
	ret
}

#' @rdname recTest-class
#' @export
addRecTest = function(x, y)
{
	if (nrow(x$Index) != nrow(y$Index)) stop("Number of records tested does not match")
	ret = list(Index = cbind(x$Index, y$Index), Desc = c(x$Desc, y$Desc))
	class(ret) = "recTest"
	ret
}

#' @rdname recTest-class
#' @export
compressRecTest = function(x)
{
	keep = apply(x$Index, 2, function(x) any(!x | is.na(x)) )
	if (!any(keep) ) {
		warning("Cannot compress recTest: all tests passed")
		return(x)
	}
	x$Index = x$Index[, keep, drop=FALSE]
	x$Desc  = x$Desc[keep]
	x
}




#' Tools for checking records in a data frame
#'
#' These functions check for records in a data frame that some required
#' property.
#'
#' @param x the data frame where to check
#' @param v a character vector giving full variable names
#'
#' @details These are tools for checking with records in a data frame have a
#'    required property (not missing, specified values etc.) Record test of this
#'    type are usually run after running basic tests on variables as well as
#'    generic pre-processing to adjuts data formats. The output allows both
#'    a top-level summary of how many records pass a series of record tests, as
#'    well as identification of the records that fail one or more of the
#'    required tests.\cr\cr
#'    The data frame is the first argument, to permit simple use of the
#'    pipe-operator.
#'
#' @return An object of class \code{recTest}.
#'
#' @name recTests-generic
#' @seealso \code{\link{dplyr::'\%>\%'}} \code{\link{varTests-generic}}
NULL

#' @rdname recTests-generic
#' @export
recTest_Missing = function(x, v)
{
	val  = !is.na(x[, v])
	desc = paste("Value not missing in '", v, "'", sep="")
	recTest(Index = val, Desc = desc)
}

#' @rdname recTests-generic
#' @param cc a vector of correct (permissible) codes for variable \code{v}
#' @export
recTest_CorrectCodes = function(x, v, cc)
{
	val  = x[, v] %in% cc
	desc = paste("Correct codes in '", v, "'", sep="")
	recTest(Index = val, Desc = desc)
}

#' @rdname recTests-generic
#' @param lower,upper smallest/largest permissible value for variable \code{v}
#' @export
recTest_InRange = function(x, v, lower, upper)
{
	val  = (lower <= x[, v] ) & ( x[,v] <= upper)
	desc = paste("Valid range for '", v, "'", sep="")
	recTest(Index = val, Desc = desc)
}

#' @rdname recTests-generic
#' @param v1,v2 variable names
#' @export
recTest_LessOrEqual = function(x, v1, v2)
{
	val  = x[, v1] <= x[,v2]
	desc = paste("'",v1, "' less or equal to '", v2, "'", sep="")
	recTest(Index = val, Desc = desc)
}




## ??????
varMatchingMissing = function(v1, v2, x)
{
	flag = TRUE ; text = "ok"
	vv1  = x[, v1]
	vv2  = x[, v2]
	if ( any(is.na(vv1) != is.na(vv2) ) ) {
		flag = FALSE		
		text = paste("Variables '", v1, "' and '", v2, "' have non-matching pattern of missingness", sep="")
	}
	list(flag=flag, text=text)
}
