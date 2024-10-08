% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zscores.R
\name{anthroplus_zscores}
\alias{anthroplus_zscores}
\title{Compute z-scores for age 5 to 19}
\usage{
anthroplus_zscores(
  sex,
  age_in_months = NA_real_,
  oedema = NA_character_,
  height_in_cm = NA_real_,
  weight_in_kg = NA_real_
)
}
\arguments{
\item{sex}{A numeric or text variable containing gender information.
If it is numeric, its values must be: 1 for males and 2 for
females. If it is character, it must be "m" or "M" for males
and "f" or "F" for females. No z-scores will be calculated
if sex is missing.}

\item{age_in_months}{A numeric variable containing age information;
Age-related z-scores will NOT be calculated if age is missing.}

\item{oedema}{The values of this character variable must be "n", "N" or "2"
for non-oedema, and "y", "Y", "1" for oedema. Although it
is highly recommended that this variable is provided by the
survey, it is possible to run
the analysis without specifying this variable. If unspecified,
the default vector of all "n" with values considered as
non-oedema is used. Missing values will be
treated as non-oedema. For oedema, weight related z-scores
are NOT calculated (set to missing),
BUT they are treated as being < -3 SD in the weight-related
indicator prevalence (\code{\link{anthroplus_prevalence}})
estimation.}

\item{height_in_cm}{A numeric variable containing standing height
information, which must be in
centimeters. Height-related z-scores will not be
calculated if missing.}

\item{weight_in_kg}{A numeric variable containing body weight information,
which must be in kilograms. Weight-related z-scores are not
calculated if missing.}
}
\value{
A data.frame with three types of columns. Columns starting with a
"c" are cleaned versions of the input arguments. Columns beginning with
a "z" are the respective z-scores and columns prefixed by a "f" indicate
if these z-scores are flagged (integers).
The number of rows is given by the length
of the input arguments.

The following columns are returned:
\itemize{
\item{\code{age_in_months}} the input age in months
\item{\code{csex}} standardized sex information
\item{\code{coedema}} standardized oedema value
\item{\code{cbmi}} BMI value based on weight/height

\item{\code{zhfa}} Height-for-age z-score
\item{\code{fhfa}} 1, if \code{abs(zhfa) > 6}

\item{\code{zwfa}} Weight-for-age z-score
\item{\code{fwfa}} 1, if \code{zwfa > 5} or \code{zwfa < -6}

\item{\code{zbfa}} BMI-for-age z-score
\item{\code{fbfa}} 1, if \code{abs(zbfa) > 5}

}
}
\description{
Compute z-scores for age 5 to 19
}
\details{
The following age cutoffs are used:
\itemize{
\item{Height-for-age} age between 60 and 228 months inclusive
\item{Weight-for-age} age between 60 and 120 months inclusive
\item{BMI-for-age} age between 60 and 228 months inclusive
}
}
\examples{
anthroplus_zscores(
  sex = c("f", "m"),
  age_in_months = c(100, 110),
  height_in_cm = c(100, 90),
  weight_in_kg = c(30, 40)
)
}
