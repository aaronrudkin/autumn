#' Nationscape Target Proportions
#'
#' A dataset containing target proportions of the demographic variables used
#' for weighting the Democracy Fund - UCLA Nationscape survey. Targets are
#' based on the proportions found in the 2017 ACS survey of adults over
#' age 18, with the exception of 2016 vote choice, which comes from the FEC's
#' 2016 election results.
#'
#' Notes go here
#'
#' @format A list of named vectors containing 17 items, each of which is a
#'   named numeric vector.
#' \describe{
#'   \item{gender}{Binary gender: "male" or "female"},
#'   \item{region}{Macro region of U.S: "Midwest", "Northeast", "South",
#'     "West"}
#'   \item{hispanic}{Hispanic ethnic self-identification: "Not Hispanic",
#'     "Mexican", "Other Hispanic"}
#'   \item{race}{Racial self-identification: "White", "Black", "AAPI"
#'     (Asian-American Pacific Islander), "Other race"}
#'   \item{household_income}{Reported household income, bracketed. Brackets:
#'     "$19,999 or less", "$20,000-$34,999", "$35,000-$49,999",
#'     "$50,000-$64,999", "$65,000-$79,999", "$80,000-$99,999",
#'     "$100,000-$124,999", "$125,000-$199,999", "$200,000 and above"}
#'   \item{education}{Level of education:  "No high school diploma",
#'     "High school diploma", "Some college", "Associate's degree",
#'     "Bachelor's degree", "Graduate degree"}
#'   \item{age}{Bracketed age: "18–23", "24–29", "30–39", "40–49", "50–59",
#'     "60–69", "70+"}
#'   \item{language}{Primary language spoken at home: "English only",
#'     "Spanish", "Other language"}
#'   \item{birthplace}{Respondent birthplace: "Another country",
#'     "The United States"}
#'   \item{urban}{PUMA-level metropolitan status: "Rural",
#'     "Rural-metropolitan", "Suburban", "Urban-suburban", "Urban"}
#'   \item{hispanic_x_language}{Cross of \code{hispanic} and \code{language}}
#'   \item{education_x_gender}{Cross of \code{education} and \code{gender}}
#'   \item{race_x_hispanic}{Cross of \code{race} and \code{hispanic}}
#'   \item{hispanic_x_education}{Cross of \code{hispanic} and
#'     \code{education}}
#'   \item{vote_2016}{2016 presidential vote: "Clinton", "Trump", "Other vote",
#'     "No vote"}
#' }
#' @author Alex Rossell-Hayes
"ns_target"

#' Survey respondents
#'
#' A sample of 6,703 survey respondents from a single wave of the Nationscape
#' survey. Column names match \code{\link{ns_target}} in addition to a
#' single identifier \code{ResponseID} to track respondents. All variables are
#' character.
#'
#' Respondents to the survey were permitted to leave
#' \code{household_income} blank, resulting in 652 missing values in this
#' sample.
#'
#' @seealso \code{\link{ns_target}} data in this package.
#' @author Nationscape Team, University of California Los Angeles. Alex
#' Rossell-Hayes, Aaron Rudkin, Tyler Reny, Chris Tausanovitch, and Lynn
#' Vavreck.
"respondent_data"
