#' emod
#' 
#' generic function for calculating emods under various assumtions
#' 
#' @param loss S3 object of one of the loss classes defined in the \code{emod} package
#' @param payroll a data frame with
#' two columns.  The first column is the payroll class and the second column is the payroll 
#' dollar value.  Payroll dollar value are for the entire 3 year period the NCCI emod takes
#' into account (i.e. do not segregate payroll dollars by policy year)
#' @param ... additional arguments
#' 
#' @export
emod <- function(loss, payroll, ...) UseMethod("emod")



#' NCCI emod
#' 
#' @export
#' @examples
#' payroll <- data.frame(class = as.factor(c("8868", "9101")), 
#'                       payroll = c(32000000, 6500000)
#'                       )
#'              
#' set.seed(1234)
#' test <- loss_ncci(year = sample(c(2012, 2013, 2014), 20, replace = TRUE),
#'                   type = sample(c("MO", "IND"), 20, replace = TRUE),
#'                   incurred = rlnorm(20, 10, 2))
#' emod(test, payroll)
emod.loss_ncci <- function(loss, payroll) {
  
  # expected by accident year
  expected_annual <- loss_expected_ncci(payroll)
  expected_annual <- as.data.frame(t(as.data.frame(expected_annual)))
  expected_total <- sum(expected_annual$expected_total)
  expected_primary <- sum(expected_annual$expected_primary)
  expected_excess <- expected_total - expected_primary
  # actual ------------------------------------------------
  actual_annual <- summary.loss_ncci(loss)
  actual_total <- sum(actual_annual$incurred)
  actual_primary <- sum(actual_annual$primary)
  actual_excess <- sum(actual_annual$excess)
  
  # determine appropriate w value from w table and ballast from 
  # ballast function
  w <- w_2015$w[findInterval(expected_total, w_2015$e)]
  b <- ballast(expected = expected_total)
  
  # calculate emod
  mod <- (actual_primary + w * actual_excess + (1.0 - w) * expected_excess + b) / (expected_total + b)
  mod <- list("claims" = loss,
              "payroll" = payroll,
              "expected_total" = expected_total,
              "expected_primary" = expected_primary,
              "actual_total" = actual_total,
              "actual_primary" = actual_primary,
              "w" = w,
              "b" = b,
              "emod" = emod
              )
  class(mod) <- c("emod_ncci", "list")
  mod
}

# expected loss by year ------------------------
loss_expected_ncci <- function(payroll) {
  
  expected_class <- dplyr::left_join(payroll, class_factors_2015, by = "class")
  
  expected_class <- dplyr::mutate(expected_class,
                                  expected_total = payroll / 100 * elr,
                                  expected_primary = payroll / 100 * elr * d,
                                  expected_excess = expected_total - expected_primary
  )
  apply(expected_class[, (length(expected_class) - 2):length(expected_class)], 2, sum)
}



# ballast calculation
ballast <- function(expected) {
  0.1 * expected + (2500 * expected * 11.90) / (expected + 700 * 11.90)
}