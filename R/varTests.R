#' Tools for checking variables in a data frame
#'
#' These functions check for existence and properties of a specified variable
#' name in a given data frame.
#'
#' @param x the data frame where to check
#' @param v a character vector giving full variable names
#'
#' @details These are simple tools suitable for checking whether a required
#'    variable is present in the data, and also whether there is a working
#'    primary key. The idea is that a data frame that passes these tests should
#'    be suitable for preprocessing. \cr\cr
#'    These tests are not suitable for checking the content of the variables,
#'    with the exception of checking for valid keys or crude missingness.\cr\cr
#'    Note that the data frame to be tested is the first argument, to make
#'    processing via the pipe operator easier.
#'
#' @return A data frame with three columns and as many rows as there are
#'   variable names specified. The columns are \code{Test}, which gives a short
#'   description of the test, \code{Variable}, which names the variable for
#'   which the test was applied, and \code{Pass}, which is a logical flag
#'   indicating whether or not the variable passed the test
#'
#' @name varTests-generic
#' @seealso \code{\link{dplyr::'\%>\%'}} \code{\link{recTests-generic}}
NULL

#' @rdname varTests-generic
#' @export
varTest_Exists = function(x, v)
{
	desc = "Variable exists"
	flag = v %in% colnames(x) 
	data.frame(Test=desc, Variable=v, Pass=flag)
}

#' @rdname varTests-generic
#' @export
varTest_NoMissing = function(x, v)
{
	desc = "No missing values"
	flag = sapply(x[, v, drop=FALSE], function(x) all(!is.na(x)))
	names(flag) = NULL
	data.frame(Test=desc, Variable=v, Pass=flag)
}

#' @rdname varTests-generic
#' @export
varTest_ExistsNoMissing = function(x, v)
{
	ret = varTest_Exists(x, v)
	if ( ret$Pass[1] ) {
		ret = rbind(ret, varTest_NoMissing(x, v))
	}
	ret
}

#' @rdname varTests-generic
#' @export
varTest_IsConstant = function(x, v)
{
	desc = "Constant label"
	flag = sapply(x[, v, drop=FALSE], function(x) length(unique(x))==1)
	names(flag) = NULL
	data.frame(Test=desc, Variable=v, Pass=flag)
}

#' @rdname varTests-generic
#' @export
varTest_NoDuplicate = function(x, v)
{
	desc = "No duplicate values"
	flag = sapply(x[, v, drop=FALSE], function(x) all(!duplicated(x)))
	names(flag) = NULL
	data.frame(Test=desc, Variable=v, Pass=flag)
}

#' @rdname varTests-generic
#' @export
varTest_IsSequential = function(x, v)
{
	desc  = "Numeric and sequential"
	flag = sapply(x[, v, drop=FALSE], function(x) is.numeric(x) & all(diff(as.numeric(x))==1))
	names(flag) = NULL
	data.frame(Test=desc, Variable=v, Pass=flag)
}













varExistsNoMissing = function(v, x)
{
	test = varExists(v, x)
	if (test$flag) {
		test2 = varNoMissing(v, x)
		test$flag = test$flag & test2$flag
		test$text = c(test$text, test2$text)
	}
	test
}


varValidValues = function(v, x, val)
{
	flag = TRUE
	text = "ok"
	vv  = x[, v]
	vv  = vv[!is.na(vv)]
	if ( !all(vv %in% val) ) {
		flag = FALSE		
		text = paste("Variable '", v, "' has non-valid values", sep="")
	}
	list(flag=flag, text=text)
}


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
