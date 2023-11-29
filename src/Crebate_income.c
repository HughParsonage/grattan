#include "grattan.h"

// add integer vectors, setting NAs to zero
void add_recycle0(int * restrict ansp, R_xlen_t N, int nThread, SEXP x) {
  if (!isInteger(x)) {
    return;
  }
  
  const int * xp = INTEGER(x);
  if (xlength(x) == N) {
    FORLOOP({
      if (xp[i] != NA_INTEGER) {
        ansp[i] += xp[i];
      }
    })
    return;
  }
  if (xlength(x) == 1) {
    const int xp0 = xp[0];
    if (xp0 == NA_INTEGER) {
      return;
    }
    FORLOOP({
      ansp[i] += xp0;
    })
  }
}

const double top_marginal_rates_since_1990[43] = 
  {0.48, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47,  
   0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.45, 0.45, 0.45, 0.45, 
   0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45,  
   0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45
  };

double top_marginal_rate(int yr) {
  // approximate! need to include accurate medicare levy 
  // return top_marginal_rates_since_1990[yr - 1990] + 0.02;  
  return 0.47;
}

SEXP Crebate_income(SEXP iic_taxable_income_loss,  
                    SEXP iit_rept_empl_super_cont, 
                    SEXP ssc_empl_cont,
                    SEXP dds_pers_super_cont,
                    SEXP iit_invest_loss,
                    SEXP iis_net_rent, 
                    SEXP iit_rept_fringe_benefit,
                    SEXP Yr,
                    SEXP nthreads) {
  // # nocov start
  if (!isInteger(Yr)) {
    error("Internal error(Crebate_income): Yr was type '%s' and length %lld but must be a length-1 int", 
          type2char(TYPEOF(Yr)), (long long)xlength(Yr));
  }
  const int * yr = INTEGER(Yr);
  // # nocov end
  R_xlen_t N = xlength(iic_taxable_income_loss);
  const int * ic_taxable_income_loss = INTEGER(iic_taxable_income_loss);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  int nThread = as_nThread(nthreads);
  FORLOOP({
    ansp[i] = ic_taxable_income_loss[i];
  })
  
  
  add_recycle0(ansp, N, nThread, iit_rept_empl_super_cont);
  add_recycle0(ansp, N, nThread, ssc_empl_cont);
  add_recycle0(ansp, N, nThread, dds_pers_super_cont);
  add_recycle0(ansp, N, nThread, iit_invest_loss);
  if (isInteger(iis_net_rent)) {
    const int * is_net_rent = INTEGER(iis_net_rent);
    if (xlength(iis_net_rent) == 1) {
      const int is_net_rent0 = is_net_rent[0];
      if (is_net_rent0 > 0) {
        FORLOOP({
          ansp[i] += is_net_rent0;
        })
      }
    } else if (xlength(iis_net_rent) == N) {
      FORLOOP({
        if (is_net_rent[i] > 0) {
          ansp[i] += is_net_rent[i];
        }
      })
    }
  }
  // add_recycle0(ansp, N, nThread, iit_rept_fringe_benefit);
  if (isInteger(iit_rept_fringe_benefit)) {
    double r_fbt = 1 - top_marginal_rate(yr[0]);
    const int * fbtp = INTEGER(iit_rept_fringe_benefit);
    if (xlength(Yr) == 1) {
      
      if (xlength(iit_rept_fringe_benefit) == N) {
        FORLOOP({
          ansp[i] += r_fbt * fbtp[i];
        })
      } else {
        const int fbt0 = fbtp[0];
        if (fbt0 != 0 && fbt0 != NA_INTEGER) {
          const int fbt0r = r_fbt * fbt0;
          FORLOOP({
            ansp[i] += fbt0r;
          })
        }
        
      }
    } else {
      if (xlength(iit_rept_fringe_benefit) == N) {
        FORLOOP({
          r_fbt = 1 - 0.47;
          ansp[i] += r_fbt * fbtp[i];
        })
      } else {
        const int fbt0 = fbtp[0];
        if (fbt0 != 0 && fbt0 != NA_INTEGER) {
          const int fbt0r = r_fbt * fbt0;
          FORLOOP({
            ansp[i] += fbt0r;
          })
        }
        
      }
    }
  }
  UNPROTECT(1);
  return ans;
}
