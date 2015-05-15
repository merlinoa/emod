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
#' payroll <- list("2012" = data.frame(class = as.factor(c("8868", "9101")), 
#'                                     payroll = c(10000000, 2000000)),
#'                 "2013" = data.frame(class = as.factor(c("8868", "9101")),
#'                                     payroll = c(10500000, 2200000)),
#'                 "2014" = data.frame(class = as.factor(c("8868", "9101")), 
#'                                     payroll = c(11000000, 2400000))
#'             )
#' 
#' emod(test, payroll)
emod.loss_ncci <- function(loss, payroll, emod_year = 2015) {
  stopifnot(length(payroll) == 3)
  
  load("R/sysdata.rda")
  
  year_class_factors <- dplyr::filter(class_factors, year == emod_year)
  year_class_factors <- dplyr::select(year_class_factors, -year)
  
  dplyr::left_join(payroll[[1]], year_class_factors, by = "class")
}