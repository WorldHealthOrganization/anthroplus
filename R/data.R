#' Sample Survey Data for the WHO 2007 References
#'
#' The dataset contains information of 933 children aged 61-228 months and it
#' was created using several surveys; therefore, it does not represent any
#' particular population.
#'
#' @format A data frame with 933 rows and 12 variables:
#' \describe{
#'   \item{strata}{stratified sampling (integer)}
#'   \item{cluster}{primary sampling unit (integer)}
#'   \item{id}{child identification number (integer)}
#'   \item{sex}{sex of the child (integer; 1=male/2=female)}
#'   \item{dob}{date of birth (date)}
#'   \item{dov}{date of visit (date)}
#'   \item{agemons}{age in months (numeric)}
#'   \item{weight}{child weight in kilograms (numeric)}
#'   \item{height}{child height in centimeters (numeric)}
#'   \item{sw}{child sample weight (numeric)}
#'   \item{oedema}{presence of oedema (character; y=yes/n=no)}
#'   \item{region}{geographical region (character; north/east/west/south)}
#' }
"Survey_WHO2007"
