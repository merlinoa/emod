#' emod calculation factors for workers compensation payroll classes
#' 
#' Data from the North Carolina Rating Bureau NCRB for calcuating
#' emods given the class of the payroll
#' 
#' @format A data frame of 6 variables with 1214 observations
#' \describe{
#' \item{year}{the year the factors were effective.  Effective years begin April, 1 
#' of each calendar year}
#' \item{class}{payroll class}
#' \item{loss_cots}{expected ultimate loss and ALAE per 100 payroll}
#' \item{elr}{expected reported loss per 100 payroll after emod adjustments}
#' \item{d}{weighting for losses above primary cutoff}
#' \item{min_prem}{minimum possible premium}
#' }
#' @examples 
#' class_factors
"class_factors"