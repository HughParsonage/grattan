#' Print LaTeX tables with headings over two lines
#' 
#' @param .data A data frame with at least some of the column names having a \code{separator}.
#' @param separator A regular expression that splits the top and bottom rows. If the separator is not found in the names of \code{.data}, the function returns an error (saying you should probably just use \code{print.xtable()})
#' @param xtable.align Passed to \code{xtable}: Character vector of length equal to the number of columns of the resulting table, indicating the alignment of the corresponding columns.
#' @param booktabs Should the tabular environment produced use booktabs? Set to TRUE for (my) convenience. This will cause an error if  \verb{\usepackage{booktabs}} has not been called in LaTeX.
#' @param heading_command A (simple) LaTeX control sequence (properly escaped) to apply to each heading names.
#' @param ... Arguments passed to \code{print.xtable}. You cannot pass \code{add.to.row}, \code{include.rownames}, or \code{include.colnames} as we make use of these options in this function.  
#' @return Output intended for LaTeX. A table produced using xtable where groups of column names are put in the top row. 
#' @author Hugh Parsonage
#' @examples
#' example_df <- 
#' data.frame(yr = 2001:2005, 
#'           Revenue__foo = 1:5, 
#'           Revenue__bar = 11:15, 
#'           Revenue__baz = 21:25, 
#'           ordinary = 1:5,
#'           Expense__foo = 1:5,
#'           Expense__bar = 11:15, 
#'           Expense__baz = 21:25, 
#'           Last__foo = 1:5, 
#'           Last__baz = 2:6,
#'           last = 101:105)
#' print_2heading_xtable(example_df, separator = "__")
#' @export

print_2heading_xtable <- function(.data, 
                                  separator = "__", 
                                  xtable.align = NULL, 
                                  booktabs = TRUE, 
                                  heading_command = "\\textbf", ...){
  orig_names <- names(.data)
  if (!any(grepl(separator, orig_names))){
    stop("No separator found in column names, so there is no point in using this function. Make sure you have specified the right separator; otherwise, just use print.xtable().")
  }
  
  if (any(c("add.to.row", "include.colnames", "include.rownames") %in% names(list(...)))){
    stop("You should not pass add.to.row, include.colnames, or include.rownames to print.xtable() via this function.")
  }
  
  
  split_names <-  grep(separator, orig_names, value = TRUE)
  split_positions <- grep(separator, orig_names, value = FALSE)
  
  # get the names before the separator
  top_headers <- gsub(paste0("^(.*)", separator, ".*$"), "\\1", split_names)
  # Where in the original table is there a new top header?
  
  orig_names_no_suffix <- 
    gsub(paste0("^(.*)", separator, ".*$"), paste0("\\1", separator), orig_names)
  
  # For cmidrule{}
  position_of_header_instance <- 
    # Need to test first column
    which(orig_names_no_suffix == dplyr::lead(orig_names_no_suffix) & 
            (orig_names_no_suffix != dplyr::lag(orig_names_no_suffix) | is.na(dplyr::lag(orig_names_no_suffix))))
  
  position_of_header_final <- 
    # Need to test final column
    which((orig_names_no_suffix != dplyr::lead(orig_names_no_suffix) | is.na(dplyr::lead(orig_names_no_suffix))) &
            orig_names_no_suffix == dplyr::lag(orig_names_no_suffix))
  
  if (length(position_of_header_instance) != length(position_of_header_final)){
    stop("This is a bug. Sorry. Please provide your data frame to the grattan package maintainer.")
  }
  
  double_row_column_names <- 
    rbind(gsub("^(.*)__(.*)$", "\\1", orig_names), gsub("^(.*)__(.*)$", "\\2", orig_names))
  
  # factor etc in table to preserve order
  top_headers_widths <- 
    as.data.frame(table(factor(double_row_column_names[1,], levels = unique(double_row_column_names[1,]))))
  
  first_row <- 
    unique(double_row_column_names[1,])
  
  first_row_formatted <- 
    paste0(heading_command, "{", first_row, "}")
  
  top_row <- character(length(first_row))
  
  # Could do paste0() directly but decided that it would 
  # avoid the point which is to add \multicolumn only to the rows that call for it.
  for (ii in seq_along(first_row)){
    if (first_row[ii] %in% top_headers){
      top_row[ii] <- paste0("\\multicolumn{", top_headers_widths$Freq[ii], "}{c}{", first_row_formatted[ii], "}")
    }
  }
  rm(ii)
  
  for_latex_top_row <- 
    paste0(paste0(top_row, collapse = " & "), "\\\\")
  
  if (booktabs){
    # (lr) to avoid cmidrule touching adjacent groups
    between_row <- paste0("\\cmidrule(lr){",  position_of_header_instance, "-", position_of_header_final, "}")
  } else {
    between_row <- paste0("\\cline{",  position_of_header_instance, "-", position_of_header_final, "}")
  }
  for_latex_between_row <- 
    paste0(paste0(between_row, collapse = ""))
  
  for_latex_second_row <- 
    paste0(heading_command, "{", double_row_column_names[2,], "}")
  
  for_latex_second_row <- 
    paste0(paste0(for_latex_second_row, collapse = " & "), "\\\\")
  
  addtorow <- list()
  addtorow$pos <- list(0, 0, 0)
  addtorow$command <- 
    paste0(paste0(c(for_latex_top_row, for_latex_between_row, for_latex_second_row)), "\n")
  
  xtable::print.xtable(xtable::xtable(.data, align = xtable.align), 
                       type = "latex",
                       add.to.row = addtorow, 
                       include.colnames = FALSE, 
                       include.rownames = FALSE,
                       booktabs = booktabs,
                       ...)
  
}