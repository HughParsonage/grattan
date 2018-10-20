#' Family tax benefit
#' 
#' @param .data \code{data.table} input. Each row is an individual. 
#' Columns must be have the same names 
#' @param id individual identifier
#' @param id_hh household identifier, used to group households to determine eligiblity 
#' and number of children
#' @param age numeric: age of each \code{id}
#' @param income numeric: income of each \code{id}
#' @param in_secondary_school logical column: does \code{id} attend secondary school?
#' @param single_parent logical column: is \code{id} (a parent) single?
#' @param other_allowance_benefit_or_pension logical column: does the individual receive a pension, benefit, or labour market program payment such as Youth Allowance?
#' @param maintenance_income numeric: the amount of maintenance income the individual receives for the care of a child/children from a previous relationship
#' @param maintenance_children integer: the number of children in the care of \code{id} for whom \code{id} receives maintenance
#' @param income_test_ftbA_1_bound Lower bound for which reduction in FTB A max 
#' payment occurs at rate \code{taper_ftbA_1}.
#' @param income_test_ftbA_2_bound Lower bound for which reduction in FTB A base
#'  payment occurs at rate \code{taper_ftbA_1}.
#' @param income_test_ftbB_bound Lower bound for which reduction in FTB B payment
#'  occurs at rate \code{taper_ftbB}.
#' @param taper_ftbA_1 The amount at which ftb A max payment is reduced for each
#'  dollar earned above \code{income_test_ftbA_1_bound}.
#' @param taper_ftbA_2 The amount at which ftb A base payment is reduced for each
#'  dollar earned above \code{income_test_ftbA_2_bound}.
#' @param taper_ftbB The amount at which ftb B payment is reduced for each dollar
#'  earned above \code{income_test_ftbB_bound}.
#' @param per How often the payment will be made. At present, payments can only 
#' be annually.
#' @param copy (logical, default: \code{TRUE}) Should a copy of \code{.data} be 
#' made before the calculation? 
#' If \code{FALSE}, intermediate values will be assigned by reference to 
#' \code{.data} (if not \code{NULL}).
#' @author Matthew Katzen
#' @export
#' 

family_tax_benefit <- function(.data = NULL,
                               id_hh = NULL,
                               id = NULL,
                               age = NULL,
                               income = NULL,
                               in_secondary_school = NULL,
                               single_parent = NULL,
                               other_allowance_benefit_or_pension = NULL,
                               maintenance_income = NULL,
                               maintenance_children = NULL,
                               income_test_ftbA_1_bound = 51027,
                               income_test_ftbA_2_bound = 94316,
                               income_test_ftbB_bound = 5402,
                               taper_ftbA_1 = 0.2,
                               taper_ftbA_2 = 0.3,
                               taper_ftbB = 0.2,
                               per = 'year', 
                               copy = TRUE) {
  # https://www.humanservices.gov.au/sites/default/files/co029-1603en.pdf
  # https://web.archive.org/web/20160420184949/http://guides.dss.gov.au/family-assistance-guide/3/1/1/20
  # historical rates: http://guides.dss.gov.au/family-assistance-guide/3/6/
  if (is.null(.data)) {
    check_null <- function(x, ...) {
      if (is.null(x)) {
        xd <- deparse(substitute(x))
        stop("`.data` was NULL, yet ", "`", xd, "` was also NULL. ", 
             "When `data` is not provided, you must provided all columns explicitly.")
      }
      if (!missing(..1)) {
        check_null(...)
      }
    }
    
    check_null(id_hh,
               id,
               age,
               income,
               in_secondary_school, 
               single_parent, 
               other_allowance_benefit_or_pension,
               maintenance_income,
               maintenance_children)
    
    max.length <- 
      prohibit_vector_recycling.MAXLENGTH(id_hh,
                                          id,
                                          age,
                                          income,
                                          in_secondary_school, 
                                          single_parent, 
                                          other_allowance_benefit_or_pension,
                                          maintenance_income,
                                          maintenance_children)
    
    if (!is.numeric(age)) {
      stop("`age` was type ", typeof(age), ", but must be a numeric vector.")
    }
    
    if (!is.numeric(income)) {
      stop("`income` was type ", typeof(income), ", but must be a numeric vector.")
    }
    
    if (!is.logical(in_secondary_school)) {
      stop("`in_secondary_school` was type ", typeof(in_secondary_school),
           ", but must be a logical vector.")
    }
    if (!is.logical(single_parent)) {
      stop("`single_parent` was type ", typeof(single_parent),
           ", but must be a logical vector.")
    }
    if (!is.logical(other_allowance_benefit_or_pension)) {
      stop("`other_allowance_benefit_or_pension` was type ", typeof(other_allowance_benefit_or_pension),
           ", but must be a logical vector.")
    }
    if (!is.numeric(maintenance_income)) {
      stop("`maintenance_income` was type ", typeof(maintenance_income),
           ", but must be a numeric vector.")
    }
    if (!is.integer(maintenance_children)) {
      stop("`maintenance_children` was type ", typeof(maintenance_children),
           ", but must be an integer vector.")
    }
    
    .data <- 
      data.table(id_hh,
                 id,
                 age,
                 income,
                 in_secondary_school, 
                 single_parent, 
                 other_allowance_benefit_or_pension,
                 maintenance_income,
                 maintenance_children)
    
  } else {
    if (!is.data.frame(.data)) {
      stop("`.data` is not of class `data.frame`.")
    }
    
    
    for (j in c("id_hh", "id", "age", "income", "in_secondary_school",
                "single_parent", "other_allowance_benefit_or_pension",
                "maintenance_income", "maintenance_children")) {
      if (j %chin% names(.data)) {
        if (j %chin% c("age", "income", "maintenance_income")) {
          if (!is.numeric(.data[[j]])) {
            stop("Column '", j, "' in `.data` ", 
                 "was type ", typeof(.data[[j]]),
                 ", but must be numeric.")
          }
        }
        if (j %chin% c("in_secondary_school",
                       "single_parent",
                       "other_allowance_benefit_or_pension")) {
          if (!is.logical(.data[[j]])) {
            stop("Column '", j, "' in `.data` ", 
                 "was type ", typeof(.data[[j]]),
                 ", but must be a logical vector.")
          }
        }
        if (j == "maintenance_children") {
          integerish <- function(x) {
            isTRUE(all.equal(x, as.integer(x)))
          }
          if (!is.integer(.data[[j]]) &&
              !integerish(.data[[j]])) {
            if (is.factor(.data[[j]])) {
              stop("Column 'maintenance_children' was a factor",
                   ", but must be integer.")
            } else {
              stop("Column 'maintenance_children' was type ",
                   typeof(.data[[j]]),
                   ", but must be integer.")
            }
          }
        }
      } else {
        stop("`.data` did not contain a column called ", j, ". ", 
             "If `.data` is supplied it must contain the following columns:\n\t", 
             paste0(c("id_hh", "id", "age", "income", "in_secondary_school",
                      "single_parent", "other_allowance_benefit_or_pension",
                      "maintenance_income", "maintenance_children"), 
                    sep = "\t\n"))
      }
    }
  
    # Maintenance warning
    if (.data[ ,any((maintenance_income > 0) == (maintenance_children == 0))]) {
      stop("Incompatible combination of `maintenance_income` and `maintenance_children`.")
    } 
  }
  
  if (copy) {
    .data <- copy(.data)
  }
  
  # per_m <- validate_per(per)
  
  # ftbA: paid per child
  ftbA_max_rate_July_2015 <- NULL
  .data[, ftbA_max_rate_July_2015 := if_else(other_allowance_benefit_or_pension,
                                             0,
                                             if_else(age < 13,
                                                     179.76,
                                                     if_else(age <= 15,
                                                             233.94,
                                                             if_else(age <= 19 & in_secondary_school,
                                                                     233.94,
                                                                     0))))]
  ftbA_base_rate_July_2015 <- NULL
  .data[, ftbA_base_rate_July_2015 := if_else(!other_allowance_benefit_or_pension & (age <= 15 | (age <= 19 & in_secondary_school)),
                                              57.68,
                                              0)]
  
  ftbA_supplement_July_2015 <- NULL
  .data[, ftbA_supplement_July_2015 := if_else(!other_allowance_benefit_or_pension & (age <= 15 | (age <= 19 & in_secondary_school)),
                                               726.35,
                                               0)]
  # Note 1: supplement conditional on meeting requirements e.g. immunisations, tax returns
  # Note 2: can only be paid annually
  
  # MAINTENANCE ACTION TEST: if you care for a child from a previous relationship and do not take reasonable action to attain child support, child is ineleigible for ftb A max rate (can still receive ftbA base payment).
  
  # MAINTENANCE INCOME TEST ftbA
  maintenance_income_test_ftbA <- NULL
  .data[, maintenance_income_test_ftbA := if_else(maintenance_children > 0,
                                                  if_else(maintenance_children == 1,
                                                          pmax(0.5 * (maintenance_income - 1543.95), 0),
                                                          pmax(0.5 * (maintenance_income - 1543.95 - (514.65 * (maintenance_children - 1))), 0)),
                                                  0)] 
  
  second_highest <- function(x) {
    max(x[-which.max(x)])
  }
  
  family_income = NULL
  ftbA_max_family_rate = NULL
  ftbA_base_family_rate = NULL
  ftbA_supplement_family_rate = NULL
  primary_income = NULL
  secondary_income = NULL
  youngest_age = NULL
  youngest_in_secondary = NULL
  youngest_allowance_benefit_or_pension = NULL
  family_maintenance_income_test_ftbA = NULL
  
  family_data <- 
    .data[, .(family_income = sum(income), 
              ftbA_max_family_rate = sum(ftbA_max_rate_July_2015), 
              ftbA_base_family_rate = sum(ftbA_base_rate_July_2015),
              ftbA_supplement_family_rate = sum(ftbA_supplement_July_2015),
              primary_income = max(income), 
              secondary_income = if (any(single_parent)) 0 else second_highest(income),
              youngest_age = min(age),
              youngest_in_secondary = in_secondary_school[which.min(age)],
              youngest_allowance_benefit_or_pension = other_allowance_benefit_or_pension[which.min(age)],
              single_parent = any(single_parent),
              family_maintenance_income_test_ftbA = sum(maintenance_income_test_ftbA)),
          by = "id_hh"]
  
  # ftbB: payed based on youngest child
  # cannot be paid during Paid Parental Leave period
  # note: eligibility changed for fy 2016-17:
  # https://www.humanservices.gov.au/sites/default/files/co029-1607en.pdf
  ftbB_rate_July_2015 <- NULL
  family_data %>%
    .[, ftbB_rate_July_2015 := if_else(youngest_allowance_benefit_or_pension,
                                       0,
                                       if_else(youngest_age < 5,
                                               152.88,
                                               if_else(youngest_age <= 15 | (youngest_age <= 19 & youngest_in_secondary),
                                                       106.82,
                                                       0)))]
  
  ftbB_supplement_July_2015 <- NULL
  family_data %>%
    .[, ftbB_supplement_July_2015 := 354.05 * and(!youngest_allowance_benefit_or_pension,
                                                  or(youngest_age <= 15,
                                                     and(youngest_age <= 19, 
                                                         youngest_in_secondary)))]
  
  income_test_ftbA_1 <- NULL
  # income reduction test ftbA
  family_data %>%
    .[, income_test_ftbA_1 := if_else(family_income < income_test_ftbA_1_bound,
                                      0,
                                      taper_ftbA_1 * (family_income - income_test_ftbA_1_bound))]
  
  income_test_ftbA_2 <- NULL
  family_data %>%
    .[, income_test_ftbA_2 := if_else(family_income < income_test_ftbA_2_bound,
                                      0,
                                      taper_ftbA_2 * (family_income - income_test_ftbA_2_bound))]
    
    # Note: before 2015 income_test_ftbA_2_bound
    # increased based upon number of ftb children http://guides.dss.gov.au/family-assistance-guide/3/6/1 note 2G
  
  # Income test ftbB
  ftbB_eligible <- NULL
  family_data %>%
    .[, ftbB_eligible := primary_income < 100000]
    
  income_test_ftbB <- NULL
  family_data %>%
    .[, income_test_ftbB := if_else(ftbB_eligible & !single_parent & secondary_income > income_test_ftbB_bound,
                                    taper_ftbB * (secondary_income - income_test_ftbB_bound),
                                    0)]
  family_data %>%
    .[, .(id_hh, 
          ftbA_incl_supplement = pmax(365/14 * ftbA_max_family_rate + ftbA_supplement_family_rate - income_test_ftbA_1 - family_maintenance_income_test_ftbA, 
                                      365/14 * ftbA_base_family_rate + ftbA_supplement_family_rate - income_test_ftbA_2,
                                      0),
          ftbB_incl_supplement = if_else(ftbB_eligible, 
                                         pmaxC(365/14 * ftbB_rate_July_2015 + ftbB_supplement_July_2015 - income_test_ftbB,
                                               0), 
                                         0))]
  
  
}
