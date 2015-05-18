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
emod.loss_ncci <- function(loss, payroll) {
  stopifnot(length(payroll) == 3)
  
  load("R/sysdata.rda")
  
  # expected by accident year
  expected <- lapply(payroll, expected_ncci)
  expected <- t(as.data.frame(expected))
  # actual ------------------------------------------------
  loss$primary <- min(loss$incurred, 15500)
  loss$excess <- loss$incurred - loss$primary
  
  loss_year <- dplyr::group_by(loss, year)
  loss_year <- dplyr::summarize(loss_year, 
                                incurred = sum(incurred),
                                primary = sum(primary),
                                excess = sum(excess)
                               )
}

# expected loss by year ------------------------
expected_ncci <- function(payroll) {
  
  expected_class <- dplyr::left_join(payroll, class_factors_2015, by = "class")
  
  expected_class <- dplyr::mutate(expected_class,
                                  expected_total = payroll / 100 * elr,
                                  expected_primary = payroll / 100 * elr * d,
                                  expected_excess = expected_total - expected_primary
  )
  apply(expected_class[, (length(expected_class) - 2):length(expected_class)], 2, sum)
}

# ballast calculation for expected losses greater than 5,562,875
ballast <- function(expected) {
  0.1 * expected + (2500 * expected * 11.65) / (expected + 700 * 11.65)
}
