#' emod
#' 
#' generic function for calculating emods under various assumtions
#' 
#' @param loss S3 object of one of the loss classes defined in the \code{emod} package
#' @param payroll a list of n data frames.  Each list element contains a data frame with
#' two columns.  The first column is the payroll class and the second column is the
#' payroll dollar value.  Each data frame is in a seperate list item and contains a
#' years worth of payroll.
#' @param ... additional arguments
#' 
#' @export
emod <- function(loss, payroll, ...) UseMethod("emod")



#' NCCI emod
#' 
#' @export
#' @examples
#' emod()
emod.loss_ncci <- function(loss, payroll, emod_year = 2015) {
  stopifnot(length(payroll) == 3)
  
  year_class_factors <- dplyr::filter(class_factors, year == emod_year) %>%
    select(-year)
  
  dplyr::left_join(payroll[1], year_class_factors)
}