#' Event codes for HDSS data file
#'
#' The three-letter codes for different events that are recorded during surveillance
#'
#' @format A character vector
#' @source Osman Sankoh and Peter Byass. The INDEPTH Network: filling vital gaps in global epidemiology. Int J Epi 2012; 41:579-588, Table 2
#' @name INDEPTH_eventCodes
NULL

#' Verbal autopsy codes
#'
#' The verbal autopsy codes for causes of death used in the INDEPTH project, and how they map to WHO codes and categories, as well as of our own categories.
#'
#' \itemize{
#'   \item Code. The code as seen in the raw HDSS data files. Note that we allow for variation of separators and white space for fusion codes.
#'   \item Label. A representative sample of a label associated with this code. This is purely informative and not used for mapping.
#'   \item isWHOcode. Logical flag, indicating whether this is a code seen in WHORC1.
#'   \item isWHOlabel. Logical flag, indicating whether the label in the data corresponds to a label seen in WHORC1 (up to spelling, abbreviation).
#'   \item WHOcategory. One of 14 categories for verbal autospy results defined in WHORC1.
#'   \item mappedCode. The internal code that we use to present the code seen in the data files.
#'   \item mappedLabelLong. A long label for this code; this is taken directly from WHORC1.
#'   \item mappedLabelShort. An abbreviated version of the label.
#'   \item mappedCategory. A condensed categorisation of causes of death used for data exploration.
#'   \item Comment. Anything even faintly interesting about this row of data.
#' }
#'
#' @format A data frame with ten variables and 68 observations
#' @name INDEPTH_verbalAutopsy
#' @source Manual extration based on existing Stata code
#' @references Verbal autopsy standards: ascertaining and attributing causes of death. The 2012 WHO verbal autopsy instrument, Release Candidate 1.
#'             http://www.who.int/healthinfo/statistics/verbalautopsystandards/en/
NULL

#' INDEPTH HDSS Sites
#'
#' An incomplete list of centres that are part of INDEPTH: the project code,
#' country and site name.
#'
#' \itemize{
#'    \item Code. The five-character code for the side used by INDEPTH.
#'    \item Country. The name of the country where the site is located.
#'    \item ISOcode. The three-digit ISO code for the country.
#'    \item Site. The name of the site.
#' }
#'
#' @format A data frame with three variables.
#' @name INDEPTH_Centres
#' @references http://www.indepth-ishare.org/index.php/catalog/central
NULL
