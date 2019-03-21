#' SAPTO done in Rcpp
#' @param RebateIncome,MaxOffset,LowerThreshold,TaperRate,SaptoEligible,SpouseIncome,IsMarried Arguments
#' as in \code{\link{sapto}}.
#' @export

sapto_rcpp <- function(RebateIncome,
                        MaxOffset, 
                        LowerThreshold,
                        TaperRate,
                        SaptoEligible,
                        SpouseIncome, 
                        IsMarried) {
  # Make sure they're all the same length before passing to Rcpp
  prohibit_arg_recycling.MAXLENGTH(mget(ls()))
  
  do_sapto_rcpp(RebateIncome, MaxOffset, LowerThreshold, TaperRate,
                SaptoEligible, SpouseIncome, IsMarried)
}
