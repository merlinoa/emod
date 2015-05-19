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
#' set.seed(1234)
#' test <- loss_ncci(year = sample(c(2012, 2013, 2014), 20, replace = TRUE),
#'                   type = sample(c("MO", "IND"), 20, replace = TRUE),
#'                   incurred = rlnorm(20, 10, 2))
#' emod(test, payroll)
emod.loss_ncci <- function(loss, payroll) {
  stopifnot(length(payroll) == 3)
  
  load("R/sysdata.rda")
  
  # expected by accident year
  expected_annual <- lapply(payroll, expected_ncci)
  expected_annual <- as.data.frame(t(as.data.frame(expected_annual)))
  expected_total <- sum(expected_annual$expected_total)
  expected_primary <- sum(expected_annual$expected_primary)
  expected_excess <- expected_total - expected_primary
  # actual ------------------------------------------------
  actual_annual <- loss_annual(loss)
  actual_total <- sum(actual_annual$incurred)
  actual_primary <- sum(actual_annual$primary)
  actual_excess <- sum(actual_annual$excess)
  
  # determine appropriate w value from w table and ballast from 
  # ballast function
  w <- w_2015$w[findInterval(expected_total, w_2015$e)]
  b <- ballast(expected = expected_total)
  
  # calculate emod
  emod <- (actual_primary + w * actual_excess + (1.0 - w) * expected_excess + b) / (expected_total + b)
  list("expected_total" = expected_total,
       "expected_primary" = expected_primary,
       "actual_total" = actual_total,
       "actual_primary" = actual_primary,
       "w" = w,
       "b" = b,
       "emod" = emod
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

# actual loss per year ----------------------------------------
loss_annual <- function(loss) {
  # 10% medical only reduction and cap primary losses at 15500
  loss$incurred[loss$type == "MO"] <- loss$incurred[loss$type == "MO"] * 0.3
  loss$primary <- loss$incurred
  loss$primary[loss$primary > 15500] <- 15500
  
  # determine excess portion
  loss$excess <- loss$incurred - loss$primary
  
  # group by year and sum years
  loss_year <- dplyr::group_by(loss, year)
  dplyr::summarize(loss_year, 
                   incurred = sum(incurred),
                   primary = sum(primary),
                   excess = sum(excess)
  )
}

# ballast calculation
ballast <- function(expected) {
  0.1 * expected + (2500 * expected * 11.90) / (expected + 700 * 11.90)
}