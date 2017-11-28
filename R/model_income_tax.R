#' Modelled Income Tax
#' @description The income tax payable if tax settings are changed. 
#' @param sample_file A sample file having at least as many variables as the 2012-13 sample file.
#' @param baseline_fy If a parameter is not selected, the parameter's value in this tax year is used.
#' @param exclude A character vector specifying which womponents of the income tax to \emph{exclude}. 
#' Multiple values are allowed. 
#' The special value \code{"nothing"} means no components are excluded. 
#' If present at the first position of \code{exclude} (the default) it takes precedence and no components are excluded; 
#' elsewhere it has no effect.
#' @param n_dependants The number of dependants for each entry in \code{sample_file}.
#' @param ordinary_tax_thresholds A numeric vector specifying the lower bounds of the brackets for "ordinary tax" as defined by the Regulations.
#' The first element should be zero if there is a tax-free threshold.
#' @param ordinary_tax_rates The marginal rates of ordinary tax. The first element should be zero if there is a tax-free threshold. 
#' Since the temporary budget repair levy was imposed on a discrete tax bracket when it applied, it is not included in this function.
#' @param medicare_levy_lower_threshold Minimum taxable income at which the Medicare levy will be applied.
#' @param medicare_levy_upper_threshold Minimum taxable income at which the Medicare levy will be applied at the full Medicare levy rate (2\% in 2015-16). Between this threshold and the \code{medicare_levy_lower_threshold}, a tapered rate applies, starting from zero and climbing to \code{medicare_levy_rate}. 
#' @param medicare_levy_taper The taper that applies between the \code{_lower} and \code{_upper} thresholds.
#' @param medicare_levy_rate The ordinary rate of the Medicare levy for taxable incomes above \code{medicare_levy_upper_threshold}.
#' @param medicare_levy_lower_family_threshold,medicare_levy_upper_family_threshold The equivalent values for families.
#' @param medicare_levy_lower_up_for_each_child The amount to add to the \code{_family_threshold}s for each dependant child.
#' @param lito_max_offset The maximum offset available for low incomes.
#' @param lito_taper The taper to apply beyond \code{lito_min_bracket}.
#' @param lito_min_bracket The taxable income at which the value of the offset starts to reduce (from \code{lito_max_offset}).
#' @param sapto_eligible Whether or not each taxpayer in \code{sample_file} is eligible for \code{SAPTO}. 
#' If \code{NULL}, the default, then eligibility is determined by \code{age_range} in \code{sample_file};
#' \emph{i.e.}, if \code{age_range <= 1} then the taxpayer is assumed to be eligible for SAPTO.
#' @param sapto_max_offset The maximum offset available through SAPTO. 
#' @param sapto_lower_threshold The threshold at which SAPTO begins to reduce (from \code{sapto_max_offset}).
#' @param sapto_taper The taper rate beyond \code{sapto_lower_threshold}.
#' 
#' @export



model_income_tax <- function(sample_file,
                             baseline_fy,
                             exclude = c("nothing", "ordinary_tax", "medicare_levy", "lito", "sapto"),
                             n_dependants = 0L,
                             
                             ordinary_tax_thresholds = NULL,
                             ordinary_tax_rates = NULL,
                             
                             medicare_levy_lower_threshold = NULL,
                             medicare_levy_upper_threshold = NULL,
                             medicare_levy_taper = NULL, 
                             medicare_levy_rate = NULL,
                             medicare_levy_lower_family_threshold = NULL,
                             medicare_levy_upper_family_threshold = NULL,
                             medicare_levy_lower_up_for_each_child = NULL,
                             
                             lito_max_offset = NULL,
                             lito_taper = NULL,
                             lito_min_bracket = NULL,
                             
                             sapto_eligible = NULL,
                             sapto_max_offset = NULL,
                             sapto_lower_threshold = NULL,
                             sapto_taper = NULL) {
  arguments <- ls()
  if (!is.null(exclude) && exclude[1L] != "nothing") {
    exclude <- match.arg(exclude, several.ok = TRUE)
  }
  
  `%|||%` <- function(lhs, rhs) {
    if (is.null(lhs)) {
      rep_len(rhs, max.length)
    } else {
      rep_len(lhs, max.length)
    }
  }
    
  stopifnot(is.data.table(sample_file))
  .dots.ATO <- sample_file
  income <- sample_file[["Taxable_Income"]]
  if (is.null(income)) {
    stop("`sample_file` does not contain a column `Taxable_Income`, ", 
         "yet it is required for this function.")
  }
  
  max.length <- length(income)
  prohibit_vector_recycling(income, n_dependants, baseline_fy)
  
  if (is.null(sapto_eligible)) {
    if ("age_range" %chin% names(sample_file)) {
      sapto_eligible <- .subset2(sample_file, "age_range") <= 1L
    } else {
      warning("Assuming everyone is ineligible for SAPTO.")
      sapto_eligible <- logical(max.length)
    }
  }
  
  ordering <- NULL
  
  input <-
    data.table(income = income,
               fy_year = baseline_fy, 
               SaptoEligible = sapto_eligible) %>%
    .[, "ordering" := .I] %>%
    setkeyv(c("fy_year", "income"))
  
  # Check base tax
  if (!is.null(ordinary_tax_thresholds) || !is.null(ordinary_tax_rates)) {
    tax_table2_fy <- tax_table2[fy_year == baseline_fy]
    
    Thresholds <- ordinary_tax_thresholds %||% tax_table2_fy[["lower_bracket"]]
    Rates <-  ordinary_tax_rates %||% tax_table2_fy[["marginal_rate"]]
    
    if (length(Thresholds) != length(Rates)) {
      stop("`ordinary_tax_thresholds` and `ordinary_tax_rates` have different lengths. ",
           "Specify numeric vectors of equal length (or NULL).")
    }
    
    base_tax. <-
      IncomeTax(income,
                thresholds = Thresholds,
                rates = Rates)
  } else {
    tax_at <- lower_bracket <- marginal_rate <- NULL
    
    base_tax. <- 
      tax_table2[input, roll = Inf] %>%
      .[, .(ordering, tax = tax_at + (income - lower_bracket) * marginal_rate)] %>%
      setorderv("ordering") %>%
      .subset2("tax")
    
    temp_budget_repair_levy. <-
      and(input[["fy_year"]] %chin% c("2014-15", "2015-16", "2016-17"),
          income > 180e3) * 
      (0.02 * (income - 180e3))
    
    base_tax. <- base_tax. + temp_budget_repair_levy. 
  }
  
  
  
  # If .dots.ATO  is NULL, for loops over zero-length vector
  for (j in which(vapply(.dots.ATO, FUN = is.double, logical(1)))){
    set(.dots.ATO, j = j, value = as.integer(.dots.ATO[[j]]))
  }
  
  if (is.null(.dots.ATO) || "Spouse_adjusted_taxable_inc" %notin% names(.dots.ATO)) {
    the_spouse_income <- 0L
  } else {
    the_spouse_income <- .dots.ATO[["Spouse_adjusted_taxable_inc"]]
    the_spouse_income[is.na(the_spouse_income)] <- 0L
  }
  
  
  # Check medicare levy
  medicare_args <- mget(grep("^medicare_levy", arguments, perl = TRUE, value = TRUE))
  
  if (all(vapply(medicare_args, is.null, FALSE))) {
    medicare_levy. <- 
      medicare_levy(income, 
                    Spouse_income = the_spouse_income,
                    fy.year = baseline_fy, 
                    sapto.eligible = sapto_eligible, 
                    family_status = if (is.null(.dots.ATO) || "Spouse_adjusted_taxable_inc" %notin% names(.dots.ATO)) {
                      family_status
                    } else {
                      FS <- rep_len("individual", max.length)
                      FS[the_spouse_income > 0] <- "family"
                      FS
                    }, 
                    n_dependants = n_dependants, 
                    .checks = FALSE)
    
  } else {
    medicare_tbl_fy <- 
      medicare_tbl[input, on = c("fy_year", "sapto==SaptoEligible")] %>%
      setorderv("ordering")
    
    # When a parameter is selected it must satisfy the 
    # conditions of the low-income area
    
    #    |                   .
    #    |              .
    # rb |         .
    #    |       /
    #    |      /
    #    |-----/
    #    0   a  b
    # 
    # t(b - a) = rb
    #
    
    # Here, we test whether or not the conditions are so satisfied; 
    # if not, we fix one of the parameters that is not specified to
    # satisfy the relation
    
    ma <- medicare_levy_lower_threshold %|||% medicare_tbl_fy[["lower_threshold"]]
    mb <- medicare_levy_upper_threshold %|||% medicare_tbl_fy[["upper_threshold"]]
    mt <- medicare_levy_taper %|||% medicare_tbl_fy[["taper"]]
    mr <- medicare_levy_rate  %|||% medicare_tbl_fy[["rate"]]
    
    ma <- as.integer(ma)
    mb <- as.integer(mb - 1)
    
    null_medicare_args <- vapply(medicare_args, is.null, FALSE)
    
    medicare_parameter_roots <- abs(mt * (mb - ma) - mr * mb)
    
    if (any(medicare_parameter_roots[sapto_eligible] > 1)) {
      # model is misspecified in respect of offsets etc
      
      warning_if_misspecified <- function(the_arg) {
        val <- switch(the_arg, 
                      "medicare_levy_upper_threshold" = mb, 
                      "medicare_levy_lower_threshold" = ma, 
                      "medicare_levy_taper" = mt,
                      "medicare_levy_rate" = mr) %>%
          .[sapto_eligible]
        
        if (uniqueN(val) == 1L) {
          warning("`", the_arg, "` was not specified, but is inconsistent with other parameters.\n", 
                  "Set\n\t", the_arg, " = ", round(val[1]),
                  call. = FALSE)
        } else {
          warning("`", the_arg, "` was not specified, but is inconsistent with other parameters. ",
                  "\t", paste0(head(unique(round(val))), collapse = "\n\t"), "\n",
                  uniqueN(val),
                  call. = FALSE)
        }
      }
      
      if (is.null(medicare_levy_upper_threshold)) {
        mb <- mt * ma / (mt - mr)
        warning_if_misspecified("medicare_levy_upper_threshold")
        
      } else {
        
        if (is.null(medicare_levy_lower_threshold)) {
          ma <- mb - mr / mt
          warning_if_misspecified("medicare_levy_lower_threshold")
          
        } else {
          
          if (is.null(medicare_levy_taper)) {
            mt <- mr * mb / (mb - ma)
            warning_if_misspecified("medicare_levy_taper")
            
          } else {
            
            if (is.null(medicare_levy_rate)) {
              mr <- mt * (mb - ma) / mb
              warning_if_misspecified("medicare_levy_rate")
            } # alternative not reachable
          }
        }
      }
    }
    
    
    
    
    medicare_levy. <-
      MedicareLevy(income = income,
                   
                   lowerThreshold = medicare_levy_lower_threshold %|||% medicare_tbl_fy[["lower_threshold"]],
                   upperThreshold = medicare_levy_upper_threshold %|||% medicare_tbl_fy[["upper_threshold"]],
                   
                   SpouseIncome = the_spouse_income,
                   isFamily = the_spouse_income > 0,
                   NDependants = if (length(n_dependants) == 1) rep_len(n_dependants, max.length) else n_dependants,
                   
                   lowerFamilyThreshold = medicare_levy_lower_family_threshold  %|||% medicare_tbl_fy[["lower_family_threshold"]],
                   upperFamilyThreshold = medicare_levy_upper_family_threshold  %|||% medicare_tbl_fy[["upper_family_threshold"]],
                   lowerUpForEachChild  = medicare_levy_lower_up_for_each_child %|||% medicare_tbl_fy[["lower_up_for_each_child"]], 
                   
                   rate = medicare_levy_rate %|||% medicare_tbl_fy[["rate"]],
                   taper = medicare_levy_taper %|||% medicare_tbl_fy[["taper"]])
  }
  
  lito_args <- mget(grep("^lito_", arguments, perl = TRUE, value = TRUE))
  
  if (all(vapply(lito_args, is.null, FALSE))) {
    setkeyv(input, "fy_year")
    lito. <- .lito(input)
  } else {
    lito_fy <- lito_tbl[fy_year == baseline_fy]
    lito. <- lito(income,
                  max_lito = lito_max_offset %||% lito_fy[["max_offset"]],
                  lito_taper = lito_taper %||% lito_fy[["lito_taper"]],
                  min_bracket = lito_taper %||% lito_fy[["min_bracket"]])
  }
  
  
  
  sapto. <- double(max.length)
  if (any(sapto_eligible)) {
    sapto_args <- mget(grep("^sapto_(?!eligible)", arguments, perl = TRUE, value = TRUE))
    rebate_income_over_eligible <-
      rebate_income(Taxable_Income = income[sapto_eligible],
                    Rptbl_Empr_spr_cont_amt = .dots.ATO[sapto_eligible][["Rptbl_Empr_spr_cont_amt"]],
                    Net_fincl_invstmt_lss_amt = .dots.ATO[sapto_eligible][["Net_fincl_invstmt_lss_amt"]],
                    Net_rent_amt = .dots.ATO[sapto_eligible][["Net_rent_amt"]],
                    Rep_frng_ben_amt = .dots.ATO[sapto_eligible][["Rep_frng_ben_amt"]])
    if (all(vapply(sapto_args, is.null, FALSE))) {
      if (AND(!is.null(.dots.ATO),
              all(c("Rptbl_Empr_spr_cont_amt",
                    "Net_fincl_invstmt_lss_amt",
                    "Net_rent_amt", 
                    "Rep_frng_ben_amt") %chin% names(.dots.ATO)))) {
        
        sapto.[sapto_eligible] <-
          sapto(rebate_income = rebate_income_over_eligible, 
                fy.year = if (length(baseline_fy) > 1) baseline_fy[sapto_eligible] else baseline_fy,
                Spouse_income = the_spouse_income[sapto_eligible],
                family_status = {
                  FS_sapto <- rep_len("single", max.length)
                  FS_sapto[the_spouse_income > 0] <- "married"
                  FS_sapto[sapto_eligible]
                },
                sapto.eligible = TRUE)
      } else {
        sapto.[sapto_eligible] <- 
          sapto(rebate_income = rebate_income(Taxable_Income = income[sapto_eligible]), 
                fy.year = if (length(baseline_fy) > 1) baseline_fy[sapto_eligible] else baseline_fy,
                sapto.eligible = TRUE)
      }
    } else {
      sapto. <- 
        sapto_rcpp(RebateIncome = rebate_income_over_eligible,
                   MaxOffset = sapto_max_offset,
                   LowerThreshold = sapto_lower_threshold ,
                   TaperRate = sapto_taper,
                   SaptoEligible = sapto_eligible,
                   IsMarried = the_spouse_income > 0,
                   SpouseIncome = the_spouse_income)
    }
  }
  
  pmaxC(base_tax. - lito. - sapto., 0) + medicare_levy.
}

