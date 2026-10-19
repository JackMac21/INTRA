
# Function to Calculate Year BP from Calendar Year
#' Convert Calendar Year to cal BP (Before Present = 1950 AD)
#'
#' @param collection_year Numeric vector. The calendar year.
#' @param is_bc Logical vector. TRUE if the year is BC/BCE, FALSE if AD/CE (Default).
#' @return A numeric vector representing calendar years Before Present (cal BP). 
#'         Negative values indicate dates after 1950 AD.
#'         
cal_to_bp <- function(collection_year, is_bc = FALSE) {
  # Convert to astronomical years, accounting for the missing Year 0
  astron_year <- ifelse(is_bc, 1 - collection_year, collection_year)
  
  # Calculate cal BP relative to the 1950 baseline
  cal_bp <- 1950 - astron_year
  
  return(cal_bp)
}

# Function to Calculate Age-Corrected Delta 14C from Fraction Modern (Fm)

#' @param Fm Numeric vector. Fraction Modern value.
#' @param Fm_error Numeric vector. Uncertainty/error associated with the Fm value.
#' @param collection_year Numeric vector. The cal BP year (1950-calendar year, adjusted for year 0) the sample was collected or died.
#' @return A data.frame containing the calculated Delta14C and its associated error.

fm_to_delta14c <- function(Fm, Fm_error, cal_bp) {
  # True physical mean life of radiocarbon (based on 5730-year half-life)
  lambda_true <- 1 / 8267
  
  # Calculate decay correction factor for time elapsed since 1950 (what about BCE?)
  decay_factor <- exp(lambda_true * (cal_bp))
  
  # Calculate Age-Corrected Delta 14C (per mil, ‰)
  delta14C <- (Fm * decay_factor - 1) * 1000
  
  # Propagate the error linearly
  delta14C_error <- Fm_error * decay_factor * 1000
  
  return(data.frame(Delta14C = delta14C, Delta14C_error = delta14C_error))
}

#' Calculate Fraction Modern (Fm) from Age-Corrected Delta 14C
#'
#' @param delta14C Numeric vector. Delta 14C value in per mil (‰).
#' @param delta14C_error Numeric vector. Uncertainty associated with the Delta 14C value.
#' @param collection_year Numeric vector. The cal BP year  (1950-calendar year, adjusted for year 0) the sample was collected or died.
#' @return A data.frame containing the calculated Fm and its associated error.

delta14c_to_fm <- function(delta14C, delta14C_error, cal_bp) {
  # True physical mean life of radiocarbon (based on 5730-year half-life)
  lambda_true <- 1 / 8267
  
  # Calculate decay correction factor for time elapsed since 1950
  decay_factor <- exp(lambda_true * (cal_bp))
  
  # Calculate Fraction Modern (Fm)
  Fm <- ((delta14C / 1000) + 1) / decay_factor
  
  # Propagate the error linearly
  Fm_error <- (delta14C_error / 1000) / decay_factor
  
  return(data.frame(Fm = Fm, Fm_error = Fm_error))
}

# Add 14C age calculations to this


