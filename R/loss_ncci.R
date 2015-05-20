#' loss_ncci
#' 
#' S3 class for insurer loss information necessary to calculate the 
#' NCCI 3 year emod
#' 
#' @param year loss year
#' @param type loss type.  medical only "MO" or indemnity "IND"
#' @param incurred net incurred amount
#' 
#' @export
#' @examples 
#' set.seed(1234)
#' test <- loss_ncci(year = sample(c(2012, 2013, 2014), 20, replace = TRUE),
#'                   type = sample(c("MO", "IND"), 20, replace = TRUE),
#'                   incurred = rlnorm(20, 10, 2))
loss_ncci <- function(year, type, incurred) {
  loss_ncci_validate(year = year, type = type, incurred = incurred)
  loss_ncci <- list(year = year,
               type = type,
               incurred = incurred)
  loss_ncci <- as.data.frame(loss_ncci)
  class(loss_ncci) <- c("loss_ncci", "data.frame")
  loss_ncci
}

# validate loss arguments
loss_ncci_validate <- function(year, type, incurred) {
  errors <- character()
  warnings <- character()
  # check argument length
  if (length(year) != length(type) || length(year) != length(incurred)) {
    errors <- "Error: year, type, and incurred must be of same length"
  }
  # check year
  if (!is.numeric(year)) {
    errors <- c(errors, "Error: year must be of type numeric")
  }
  if (length(unique(year)) < 3) {
    warnings <- "Warniing: loss data contains less than 3 unique years. 
                 Assuming no losses for missing year(s)."
  }
  if (length(unique(year)) > 3) {
    warnings <- c(warnings, "Warnings: loss data contains more than 3 unique years.
                 Only most recent 3 years will be used.")
  }
  if (!is.numeric(incurred)) {
    error <- c(error, "Error: incurred must be of type numeric")
  }
  
  if (length(errors) > 0) {
    stop(errors)
  }
  if(length(warnings) > 0) {
    warning(warnings)
  }
}


#' summary.loss_ncci
#' 
#' Summarize loss_ncci by accident year
#' 
#' @param loss_ncci object of class loss_ncci
#' 
#' @export
#' 
#' @examples 
#' test <- loss_ncci(year = sample(c(2011, 2012, 2013), 20, replace = TRUE),
#'                   type = sample(c("MO", "IND"), 20, replace = TRUE),
#'                   incurred = rlnorm(20, 10, 2))
#'                   
#' set.seed(1234)
#' summary(test)
# actual loss per year ----------------------------------------
summary.loss_ncci <- function(loss_ncci) {
  # 10% medical only reduction and cap primary losses at 15500
  l <- loss_ncci
  l$incurred[l$type == "MO"] <- l$incurred[l$type == "MO"] * 0.3
  l$primary <- l$incurred
  l$primary[l$primary > 15500] <- 15500
  
  # determine excess portion
  l$excess <- l$incurred - l$primary
  
  # group by year and sum years
  loss_year <- dplyr::group_by(l, year)
  indemnity_claims <- dplyr::filter(loss_year, type == "IND")
  indemnity_claims <- dplyr::summarise(indemnity_claims, indemnity_claims = n())
  out <- dplyr::summarize(loss_year,
                   claims = n(),
                   incurred = sum(incurred),
                   primary = sum(primary),
                   excess = sum(excess)
                   )
  cbind(out, indemnity_claims)
}
