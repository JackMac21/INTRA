# Radiocarbon Calculations

# Function to Calculate Years BP from Calendar Year BCE/CE
#' @param calendar Numeric vector. The dated BCE/CE calendar year of the sample, with a negative value representing that year in BCE.
#' @return A data.frame containing the dated calendar years BP (before present, "present" = 1950 CE) of the sample. Negative values indicate dates after 1950 CE.

calendar_to_bp <- function(calendar) {
  # Convert BCE/CE to BP while accounting for year zero problem
  bp <- ifelse(calendar < 0, 1949 - calendar, 1950 - calendar)
  return(data.frame(YearsBP = bp))
}


# Function to calculate Calendar Year BCE/CE from Years BP
#' @param bp Numeric vector. The dated calendar years BP (before present, "present" = 1950 CE) of the sample. Negative values represent years after 1950 CE.
#' @return A data.frame containing the dated BCE/CE calendar years of the sample, with a negative value representing that year in BCE.

bp_to_calendar <- function(bp) {
  # Convert BP to BCE/CE while accounting for the year zero problem
  calendar <- ifelse(bp < 1950, 1950 - bp, 1949 - bp)
  return(data.frame(DatedCalendarYear = calendar))
}


# Function to Calculate Age-Corrected Delta 14C from Fraction Modern (Fm)
#' @param Fm Numeric vector. Fraction Modern value.
#' @param Fm_error Numeric vector. Uncertainty/error associated with the Fm value.
#' @param bp Numeric vector. The dated calendar years BP (before present, "present" = 1950 CE) of the sample. Negative values represent years after 1950 CE.
#' @return A data.frame containing the calculated Delta14C and its associated error.

fm_to_delta14c <- function(fm, fm_error, bp) {
  # True physical mean life of radiocarbon (based on 5730-year half-life)
  lambda_true <- 1 / 8267
  # Calculate decay correction factor for time elapsed since 1950
  decay_factor <- exp(lambda_true * (-(bp)))
  # Calculate Age-Corrected Delta 14C (per mil, ‰)
  delta14C <- (fm * decay_factor - 1) * 1000
  # Propagate the error linearly
  delta14C_error <- fm_error * decay_factor * 1000
  return(data.frame(Delta14C = delta14C, Delta14C_error = delta14C_error))
}


# Function to Calculate Fraction Modern (Fm) from Age-Corrected Delta 14C
#' @param delta14C Numeric vector. Delta 14C value in per mil (‰).
#' @param delta14C_error Numeric vector. Uncertainty associated with the Delta 14C value.
#' @param bp Numeric vector. The dated calendar years BP (before present, "present" = 1950 CE) of the sample. Negative values represent years after 1950 CE.
#' @return A data.frame containing the calculated Fm and its associated error.

delta14c_to_fm <- function(delta14C, delta14C_error, bp) {
  # True physical mean life of radiocarbon (based on 5730-year half-life)
  lambda_true <- 1 / 8267
  # Calculate decay correction factor for time elapsed since 1950
  decay_factor <- exp(lambda_true * (-(bp)))
  # Calculate Fraction Modern (Fm)
  Fm <- ((delta14C / 1000) + 1) * decay_factor
  # Propagate the error linearly
  Fm_error <- (delta14C_error / 1000) * decay_factor
  return(data.frame(Fm = Fm, Fm_error = Fm_error))
}


# Function to Calculate Age-Corrected Delta 14C from 14C Age
#' @param RadAge Numeric vector. 14C Age value.
#' @param RadAge_error Numeric vector. Uncertainty/error associated with the RadAge value.
#' @param bp Numeric vector. The dated calendar years BP (before present, "present" = 1950 CE) of the sample. Negative values represent years after 1950 CE.
#' @return A data.frame containing the calculated Delta14C and its associated error.

radAge_to_delta14C <- function(radAge, radAge_error, bp) {
  # True physical mean life of radiocarbon (based on 5730-year half-life)
  lambda_true <- 1 / 8267
  # Libby mean life of radiocarbon (based on 5568-year half-life)
  lambda_libby <- 1 / 8033
  # Calculate decay correction factor for time elapsed since 1950
  decay_factor <- lambda_true * bp
  # Calculate Age-Corrected Delta 14C (per mil, ‰)
  delta14C <- (exp(decay_factor - radAge * lambda_libby) - 1) * 1000
  # Propagate the error linearly
  delta14C_error <- 1000 * lambda_libby * exp((bp * lambda_true) - (radAge * lambda_libby)) * radAge_error
  return(data.frame(Delta14C = delta14C, Delta14C_error = delta14C_error))
}


# Function to Calculate 14C Age from Age-Corrected Delta 14C
#' @param delta14C Numeric vector. Delta 14C value in per mil (‰).
#' @param delta14C_error Numeric vector. Uncertainty associated with the Delta 14C value.
#' @param bp Numeric vector. The dated calendar years BP (before present, "present" = 1950 CE) of the sample. Negative values represent years after 1950 CE.
#' @return A data.frame containing the calculated 14C Age and its associated error. Note that 14C ages and errors are rounded due to convention.

delta14C_to_radAge <- function(delta14C, delta14C_error, bp) {
  # True physical mean life of radiocarbon (based on 5730-year half-life)
  lambda_true <- 1 / 8267
  # Libby mean life of radiocarbon (based on 5568-year half-life)
  lambda_libby <- 8033
  # Calculate 14C Age (years)
  radAge <- lambda_libby * ((lambda_true * bp) - log(delta14C / 1000 + 1))
  # Propagate the error linearly
  radAge_error <- (lambda_libby * delta14C_error) / (1000 + delta14C)
  return(data.frame(RadAge = round(radAge, digits = 0), RadAge_error = round(radAge_error, digits = 0)))
}


# Function to Calculate 14C Age from Fraction Modern (Fm)
#' @param Fm Numeric vector. Fraction Modern value.
#' @param Fm_error Numeric vector. Uncertainty/error associated with the Fm value.
#' @return A data.frame containing the calculated 14C Age and its associated error. Note that 14C ages and errors are rounded due to convention.

fm_to_radAge <- function(fm, fm_error) {
  # Libby mean life of radiocarbon (based on 5568-year half-life)
  lambda_libby <- 8033
  # Calculate 14C Age (years)
  radAge <- -(lambda_libby) * log(fm)
  # Propagate the error linearly
  radAge_Error <- lambda_libby / fm * fm_error
  return(data.frame(RadAge = round(radAge, digits = 0), RadAge_error = round(radAge_Error, digits = 0)))
}


# Function to Calculate Fraction Modern (fm) from 14C Age
#' @param RadAge Numeric vector. 14C Age value.
#' @param RadAge_error Numeric vector. Uncertainty/error associated with the RadAge value.
#' @return A data.frame containing the calculated Fm and its associated error.

radAge_to_fm <- function(radAge, radAge_error) {
  # Libby mean life of radiocarbon (based on 5568-year half-life)
  lambda_libby <- 1/8033
  # Calculate Fraction Modern (Fm)
  fm <- exp(-(radAge) * lambda_libby)
  # Propagate the error linearly
  fm_error <- fm * lambda_libby * radAge_error
  return(data.frame(Fm = fm, Fm_error = fm_error))
}



