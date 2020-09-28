

#include "Sapto.h"
#include "grattan.h"

struct Sapto SaptoSince2000[31];

int set_Sapto2000() {
  SaptoSince2000[0].year = 2000;
  SaptoSince2000[0].pension_age = 65;
  SaptoSince2000[0].mxo_single = SAPTO_MAX_SINGLE_2000;
  SaptoSince2000[0].mxo_couple = SAPTO_MAX_MARRIED_2000;
  SaptoSince2000[0].lwr_single = SAPTO_LWR_SINGLE_2000;
  SaptoSince2000[0].lwr_couple = SAPTO_LWR_MARRIED_2000;
  SaptoSince2000[0].upr_single = SAPTO_LWR_SINGLE_2000 + SAPTO_MAX_SINGLE_2000 / SAPTO_TAPER;
  SaptoSince2000[0].upr_couple = SAPTO_LWR_MARRIED_2000 + SAPTO_MAX_MARRIED_2000 / SAPTO_TAPER;
  SaptoSince2000[0].taper = 1/8;
  
  SaptoSince2000[0].first_tax_rate = ORD_TAX_RATES_2000[1];
  SaptoSince2000[0].tax_free_thresh = ORD_TAX_BRACK_2000[1];
  SaptoSince2000[0].lito_max_offset = LITO_MAX_OFFSET_2000;
  
  return 0;
}
int set_Sapto2001() {
  SaptoSince2000[1].year = 2001;
  SaptoSince2000[1].pension_age = 65;
  SaptoSince2000[1].mxo_single = SAPTO_MAX_SINGLE_2001;
  SaptoSince2000[1].mxo_couple = SAPTO_MAX_MARRIED_2001;
  SaptoSince2000[1].lwr_single = SAPTO_LWR_SINGLE_2001;
  SaptoSince2000[1].lwr_couple = SAPTO_LWR_MARRIED_2001;
  SaptoSince2000[1].upr_single = SAPTO_LWR_SINGLE_2001 + SAPTO_MAX_SINGLE_2001 / SAPTO_TAPER;
  SaptoSince2000[1].upr_couple = SAPTO_LWR_MARRIED_2001 + SAPTO_MAX_MARRIED_2001 / SAPTO_TAPER;
  SaptoSince2000[1].taper = 1/8;
  
  SaptoSince2000[1].first_tax_rate = ORD_TAX_RATES_2001[1];
  SaptoSince2000[1].tax_free_thresh = ORD_TAX_BRACK_2001[1];
  SaptoSince2000[1].lito_max_offset = LITO_MAX_OFFSET_2001;
  
  return 0;
}
int set_Sapto2002() {
  SaptoSince2000[2].year = 2002;
  SaptoSince2000[2].pension_age = 65;
  SaptoSince2000[2].mxo_single = SAPTO_MAX_SINGLE_2002;
  SaptoSince2000[2].mxo_couple = SAPTO_MAX_MARRIED_2002;
  SaptoSince2000[2].lwr_single = SAPTO_LWR_SINGLE_2002;
  SaptoSince2000[2].lwr_couple = SAPTO_LWR_MARRIED_2002;
  SaptoSince2000[2].upr_single = SAPTO_LWR_SINGLE_2002 + SAPTO_MAX_SINGLE_2002 / SAPTO_TAPER;
  SaptoSince2000[2].upr_couple = SAPTO_LWR_MARRIED_2002 + SAPTO_MAX_MARRIED_2002 / SAPTO_TAPER;
  SaptoSince2000[2].taper = 1/8;
  
  SaptoSince2000[2].first_tax_rate = ORD_TAX_RATES_2002[1];
  SaptoSince2000[2].tax_free_thresh = ORD_TAX_BRACK_2002[1];
  SaptoSince2000[2].lito_max_offset = LITO_MAX_OFFSET_2002;
  
  return 0;
}
int set_Sapto2003() {
  SaptoSince2000[3].year = 2003;
  SaptoSince2000[3].pension_age = 65;
  SaptoSince2000[3].mxo_single = SAPTO_MAX_SINGLE_2003;
  SaptoSince2000[3].mxo_couple = SAPTO_MAX_MARRIED_2003;
  SaptoSince2000[3].lwr_single = SAPTO_LWR_SINGLE_2003;
  SaptoSince2000[3].lwr_couple = SAPTO_LWR_MARRIED_2003;
  SaptoSince2000[3].upr_single = SAPTO_LWR_SINGLE_2003 + SAPTO_MAX_SINGLE_2003 / SAPTO_TAPER;
  SaptoSince2000[3].upr_couple = SAPTO_LWR_MARRIED_2003 + SAPTO_MAX_MARRIED_2003 / SAPTO_TAPER;
  SaptoSince2000[3].taper = 1/8;
  
  SaptoSince2000[3].first_tax_rate = ORD_TAX_RATES_2003[1];
  SaptoSince2000[3].tax_free_thresh = ORD_TAX_BRACK_2003[1];
  SaptoSince2000[3].lito_max_offset = LITO_MAX_OFFSET_2003;
  
  return 0;
}
int set_Sapto2004() {
  SaptoSince2000[4].year = 2004;
  SaptoSince2000[4].pension_age = 65;
  SaptoSince2000[4].mxo_single = SAPTO_MAX_SINGLE_2004;
  SaptoSince2000[4].mxo_couple = SAPTO_MAX_MARRIED_2004;
  SaptoSince2000[4].lwr_single = SAPTO_LWR_SINGLE_2004;
  SaptoSince2000[4].lwr_couple = SAPTO_LWR_MARRIED_2004;
  SaptoSince2000[4].upr_single = SAPTO_LWR_SINGLE_2004 + SAPTO_MAX_SINGLE_2004 / SAPTO_TAPER;
  SaptoSince2000[4].upr_couple = SAPTO_LWR_MARRIED_2004 + SAPTO_MAX_MARRIED_2004 / SAPTO_TAPER;
  SaptoSince2000[4].taper = 1/8;
  
  SaptoSince2000[4].first_tax_rate = ORD_TAX_RATES_2004[1];
  SaptoSince2000[4].tax_free_thresh = ORD_TAX_BRACK_2004[1];
  SaptoSince2000[4].lito_max_offset = LITO_MAX_OFFSET_2004;
  
  return 0;
}
int set_Sapto2005() {
  SaptoSince2000[5].year = 2005;
  SaptoSince2000[5].pension_age = 65;
  SaptoSince2000[5].mxo_single = SAPTO_MAX_SINGLE_2005;
  SaptoSince2000[5].mxo_couple = SAPTO_MAX_MARRIED_2005;
  SaptoSince2000[5].lwr_single = SAPTO_LWR_SINGLE_2005;
  SaptoSince2000[5].lwr_couple = SAPTO_LWR_MARRIED_2005;
  SaptoSince2000[5].upr_single = SAPTO_LWR_SINGLE_2005 + SAPTO_MAX_SINGLE_2005 / SAPTO_TAPER;
  SaptoSince2000[5].upr_couple = SAPTO_LWR_MARRIED_2005 + SAPTO_MAX_MARRIED_2005 / SAPTO_TAPER;
  SaptoSince2000[5].taper = 1/8;
  
  SaptoSince2000[5].first_tax_rate = ORD_TAX_RATES_2005[1];
  SaptoSince2000[5].tax_free_thresh = ORD_TAX_BRACK_2005[1];
  SaptoSince2000[5].lito_max_offset = LITO_MAX_OFFSET_2005;
  
  return 0;
}
int set_Sapto2006() {
  SaptoSince2000[6].year = 2006;
  SaptoSince2000[6].pension_age = 65;
  SaptoSince2000[6].mxo_single = SAPTO_MAX_SINGLE_2006;
  SaptoSince2000[6].mxo_couple = SAPTO_MAX_MARRIED_2006;
  SaptoSince2000[6].lwr_single = SAPTO_LWR_SINGLE_2006;
  SaptoSince2000[6].lwr_couple = SAPTO_LWR_MARRIED_2006;
  SaptoSince2000[6].upr_single = SAPTO_LWR_SINGLE_2006 + SAPTO_MAX_SINGLE_2006 / SAPTO_TAPER;
  SaptoSince2000[6].upr_couple = SAPTO_LWR_MARRIED_2006 + SAPTO_MAX_MARRIED_2006 / SAPTO_TAPER;
  SaptoSince2000[6].taper = 1/8;
  
  SaptoSince2000[6].first_tax_rate = ORD_TAX_RATES_2006[1];
  SaptoSince2000[6].tax_free_thresh = ORD_TAX_BRACK_2006[1];
  SaptoSince2000[6].lito_max_offset = LITO_MAX_OFFSET_2006;
  
  return 0;
}
int set_Sapto2007() {
  SaptoSince2000[7].year = 2007;
  SaptoSince2000[7].pension_age = 65;
  SaptoSince2000[7].mxo_single = SAPTO_MAX_SINGLE_2007;
  SaptoSince2000[7].mxo_couple = SAPTO_MAX_MARRIED_2007;
  SaptoSince2000[7].lwr_single = SAPTO_LWR_SINGLE_2007;
  SaptoSince2000[7].lwr_couple = SAPTO_LWR_MARRIED_2007;
  SaptoSince2000[7].upr_single = SAPTO_LWR_SINGLE_2007 + SAPTO_MAX_SINGLE_2007 / SAPTO_TAPER;
  SaptoSince2000[7].upr_couple = SAPTO_LWR_MARRIED_2007 + SAPTO_MAX_MARRIED_2007 / SAPTO_TAPER;
  SaptoSince2000[7].taper = 1/8;
  
  SaptoSince2000[7].first_tax_rate = ORD_TAX_RATES_2007[1];
  SaptoSince2000[7].tax_free_thresh = ORD_TAX_BRACK_2007[1];
  SaptoSince2000[7].lito_max_offset = LITO_MAX_OFFSET_2007;
  
  return 0;
}
int set_Sapto2008() {
  SaptoSince2000[8].year = 2008;
  SaptoSince2000[8].pension_age = 65;
  SaptoSince2000[8].mxo_single = SAPTO_MAX_SINGLE_2008;
  SaptoSince2000[8].mxo_couple = SAPTO_MAX_MARRIED_2008;
  SaptoSince2000[8].lwr_single = SAPTO_LWR_SINGLE_2008;
  SaptoSince2000[8].lwr_couple = SAPTO_LWR_MARRIED_2008;
  SaptoSince2000[8].upr_single = SAPTO_LWR_SINGLE_2008 + SAPTO_MAX_SINGLE_2008 / SAPTO_TAPER;
  SaptoSince2000[8].upr_couple = SAPTO_LWR_MARRIED_2008 + SAPTO_MAX_MARRIED_2008 / SAPTO_TAPER;
  SaptoSince2000[8].taper = 1/8;
  
  SaptoSince2000[8].first_tax_rate = ORD_TAX_RATES_2008[1];
  SaptoSince2000[8].tax_free_thresh = ORD_TAX_BRACK_2008[1];
  SaptoSince2000[8].lito_max_offset = LITO_MAX_OFFSET_2008;
  
  return 0;
}
int set_Sapto2009() {
  SaptoSince2000[9].year = 2009;
  SaptoSince2000[9].pension_age = 65;
  SaptoSince2000[9].mxo_single = SAPTO_MAX_SINGLE_2009;
  SaptoSince2000[9].mxo_couple = SAPTO_MAX_MARRIED_2009;
  SaptoSince2000[9].lwr_single = SAPTO_LWR_SINGLE_2009;
  SaptoSince2000[9].lwr_couple = SAPTO_LWR_MARRIED_2009;
  SaptoSince2000[9].upr_single = SAPTO_LWR_SINGLE_2009 + SAPTO_MAX_SINGLE_2009 / SAPTO_TAPER;
  SaptoSince2000[9].upr_couple = SAPTO_LWR_MARRIED_2009 + SAPTO_MAX_MARRIED_2009 / SAPTO_TAPER;
  SaptoSince2000[9].taper = 1/8;
  
  SaptoSince2000[9].first_tax_rate = ORD_TAX_RATES_2009[1];
  SaptoSince2000[9].tax_free_thresh = ORD_TAX_BRACK_2009[1];
  SaptoSince2000[9].lito_max_offset = LITO_MAX_OFFSET_2009;
  
  return 0;
}
int set_Sapto2010() {
  SaptoSince2000[10].year = 2010;
  SaptoSince2000[10].pension_age = 65;
  SaptoSince2000[10].mxo_single = SAPTO_MAX_SINGLE_2010;
  SaptoSince2000[10].mxo_couple = SAPTO_MAX_MARRIED_2010;
  SaptoSince2000[10].lwr_single = SAPTO_LWR_SINGLE_2010;
  SaptoSince2000[10].lwr_couple = SAPTO_LWR_MARRIED_2010;
  SaptoSince2000[10].upr_single = SAPTO_LWR_SINGLE_2010 + SAPTO_MAX_SINGLE_2010 / SAPTO_TAPER;
  SaptoSince2000[10].upr_couple = SAPTO_LWR_MARRIED_2010 + SAPTO_MAX_MARRIED_2010 / SAPTO_TAPER;
  SaptoSince2000[10].taper = 1/8;
  
  SaptoSince2000[10].first_tax_rate = ORD_TAX_RATES_2010[1];
  SaptoSince2000[10].tax_free_thresh = ORD_TAX_BRACK_2010[1];
  SaptoSince2000[10].lito_max_offset = LITO_MAX_OFFSET_2010;
  
  return 0;
}
int set_Sapto2011() {
  SaptoSince2000[11].year = 2011;
  SaptoSince2000[11].pension_age = 65;
  SaptoSince2000[11].mxo_single = SAPTO_MAX_SINGLE_2011;
  SaptoSince2000[11].mxo_couple = SAPTO_MAX_MARRIED_2011;
  SaptoSince2000[11].lwr_single = SAPTO_LWR_SINGLE_2011;
  SaptoSince2000[11].lwr_couple = SAPTO_LWR_MARRIED_2011;
  SaptoSince2000[11].upr_single = SAPTO_LWR_SINGLE_2011 + SAPTO_MAX_SINGLE_2011 / SAPTO_TAPER;
  SaptoSince2000[11].upr_couple = SAPTO_LWR_MARRIED_2011 + SAPTO_MAX_MARRIED_2011 / SAPTO_TAPER;
  SaptoSince2000[11].taper = 1/8;
  
  SaptoSince2000[11].first_tax_rate = ORD_TAX_RATES_2011[1];
  SaptoSince2000[11].tax_free_thresh = ORD_TAX_BRACK_2011[1];
  SaptoSince2000[11].lito_max_offset = LITO_MAX_OFFSET_2011;
  
  return 0;
}
int set_Sapto2012() {
  SaptoSince2000[12].year = 2012;
  SaptoSince2000[12].pension_age = 65;
  SaptoSince2000[12].mxo_single = SAPTO_MAX_SINGLE_2012;
  SaptoSince2000[12].mxo_couple = SAPTO_MAX_MARRIED_2012;
  SaptoSince2000[12].lwr_single = SAPTO_LWR_SINGLE_2012;
  SaptoSince2000[12].lwr_couple = SAPTO_LWR_MARRIED_2012;
  SaptoSince2000[12].upr_single = SAPTO_LWR_SINGLE_2012 + SAPTO_MAX_SINGLE_2012 / SAPTO_TAPER;
  SaptoSince2000[12].upr_couple = SAPTO_LWR_MARRIED_2012 + SAPTO_MAX_MARRIED_2012 / SAPTO_TAPER;
  SaptoSince2000[12].taper = 1/8;
  
  SaptoSince2000[12].first_tax_rate = ORD_TAX_RATES_2012[1];
  SaptoSince2000[12].tax_free_thresh = ORD_TAX_BRACK_2012[1];
  SaptoSince2000[12].lito_max_offset = LITO_MAX_OFFSET_2012;
  
  return 0;
}
int set_Sapto2013() {
  SaptoSince2000[13].year = 2013;
  SaptoSince2000[13].pension_age = 65;
  SaptoSince2000[13].mxo_single = SAPTO_MAX_SINGLE_2013;
  SaptoSince2000[13].mxo_couple = SAPTO_MAX_MARRIED_2013;
  SaptoSince2000[13].lwr_single = SAPTO_LWR_SINGLE_2013;
  SaptoSince2000[13].lwr_couple = SAPTO_LWR_MARRIED_2013;
  SaptoSince2000[13].upr_single = SAPTO_LWR_SINGLE_2013 + SAPTO_MAX_SINGLE_2013 / SAPTO_TAPER;
  SaptoSince2000[13].upr_couple = SAPTO_LWR_MARRIED_2013 + SAPTO_MAX_MARRIED_2013 / SAPTO_TAPER;
  SaptoSince2000[13].taper = 1/8;
  
  SaptoSince2000[13].first_tax_rate = ORD_TAX_RATES_2013[1];
  SaptoSince2000[13].tax_free_thresh = ORD_TAX_BRACK_2013[1];
  SaptoSince2000[13].lito_max_offset = LITO_MAX_OFFSET_2013;
  
  return 0;
}
int set_Sapto2014() {
  SaptoSince2000[14].year = 2014;
  SaptoSince2000[14].pension_age = 65;
  SaptoSince2000[14].mxo_single = SAPTO_MAX_SINGLE_2014;
  SaptoSince2000[14].mxo_couple = SAPTO_MAX_MARRIED_2014;
  SaptoSince2000[14].lwr_single = SAPTO_LWR_SINGLE_2014;
  SaptoSince2000[14].lwr_couple = SAPTO_LWR_MARRIED_2014;
  SaptoSince2000[14].upr_single = SAPTO_LWR_SINGLE_2014 + SAPTO_MAX_SINGLE_2014 / SAPTO_TAPER;
  SaptoSince2000[14].upr_couple = SAPTO_LWR_MARRIED_2014 + SAPTO_MAX_MARRIED_2014 / SAPTO_TAPER;
  SaptoSince2000[14].taper = 1/8;
  
  SaptoSince2000[14].first_tax_rate = ORD_TAX_RATES_2014[1];
  SaptoSince2000[14].tax_free_thresh = ORD_TAX_BRACK_2014[1];
  SaptoSince2000[14].lito_max_offset = LITO_MAX_OFFSET_2014;
  
  return 0;
}
int set_Sapto2015() {
  SaptoSince2000[15].year = 2015;
  SaptoSince2000[15].pension_age = 65;
  SaptoSince2000[15].mxo_single = SAPTO_MAX_SINGLE_2015;
  SaptoSince2000[15].mxo_couple = SAPTO_MAX_MARRIED_2015;
  SaptoSince2000[15].lwr_single = SAPTO_LWR_SINGLE_2015;
  SaptoSince2000[15].lwr_couple = SAPTO_LWR_MARRIED_2015;
  SaptoSince2000[15].upr_single = SAPTO_LWR_SINGLE_2015 + SAPTO_MAX_SINGLE_2015 / SAPTO_TAPER;
  SaptoSince2000[15].upr_couple = SAPTO_LWR_MARRIED_2015 + SAPTO_MAX_MARRIED_2015 / SAPTO_TAPER;
  SaptoSince2000[15].taper = 1/8;
  
  SaptoSince2000[15].first_tax_rate = ORD_TAX_RATES_2015[1];
  SaptoSince2000[15].tax_free_thresh = ORD_TAX_BRACK_2015[1];
  SaptoSince2000[15].lito_max_offset = LITO_MAX_OFFSET_2015;
  
  return 0;
}
int set_Sapto2016() {
  SaptoSince2000[16].year = 2016;
  SaptoSince2000[16].pension_age = 65;
  SaptoSince2000[16].mxo_single = SAPTO_MAX_SINGLE_2016;
  SaptoSince2000[16].mxo_couple = SAPTO_MAX_MARRIED_2016;
  SaptoSince2000[16].lwr_single = SAPTO_LWR_SINGLE_2016;
  SaptoSince2000[16].lwr_couple = SAPTO_LWR_MARRIED_2016;
  SaptoSince2000[16].upr_single = SAPTO_LWR_SINGLE_2016 + SAPTO_MAX_SINGLE_2016 / SAPTO_TAPER;
  SaptoSince2000[16].upr_couple = SAPTO_LWR_MARRIED_2016 + SAPTO_MAX_MARRIED_2016 / SAPTO_TAPER;
  SaptoSince2000[16].taper = 1/8;
  
  SaptoSince2000[16].first_tax_rate = ORD_TAX_RATES_2016[1];
  SaptoSince2000[16].tax_free_thresh = ORD_TAX_BRACK_2016[1];
  SaptoSince2000[16].lito_max_offset = LITO_MAX_OFFSET_2016;
  
  return 0;
}
int set_Sapto2017() {
  SaptoSince2000[17].year = 2017;
  SaptoSince2000[17].pension_age = 65;
  SaptoSince2000[17].mxo_single = SAPTO_MAX_SINGLE_2017;
  SaptoSince2000[17].mxo_couple = SAPTO_MAX_MARRIED_2017;
  SaptoSince2000[17].lwr_single = SAPTO_LWR_SINGLE_2017;
  SaptoSince2000[17].lwr_couple = SAPTO_LWR_MARRIED_2017;
  SaptoSince2000[17].upr_single = SAPTO_LWR_SINGLE_2017 + SAPTO_MAX_SINGLE_2017 / SAPTO_TAPER;
  SaptoSince2000[17].upr_couple = SAPTO_LWR_MARRIED_2017 + SAPTO_MAX_MARRIED_2017 / SAPTO_TAPER;
  SaptoSince2000[17].taper = 1/8;
  
  SaptoSince2000[17].first_tax_rate = ORD_TAX_RATES_2017[1];
  SaptoSince2000[17].tax_free_thresh = ORD_TAX_BRACK_2017[1];
  SaptoSince2000[17].lito_max_offset = LITO_MAX_OFFSET_2017;
  
  return 0;
}
int set_Sapto2018() {
  SaptoSince2000[18].year = 2018;
  SaptoSince2000[18].pension_age = 65;
  SaptoSince2000[18].mxo_single = SAPTO_MAX_SINGLE_2018;
  SaptoSince2000[18].mxo_couple = SAPTO_MAX_MARRIED_2018;
  SaptoSince2000[18].lwr_single = SAPTO_LWR_SINGLE_2018;
  SaptoSince2000[18].lwr_couple = SAPTO_LWR_MARRIED_2018;
  SaptoSince2000[18].upr_single = SAPTO_LWR_SINGLE_2018 + SAPTO_MAX_SINGLE_2018 / SAPTO_TAPER;
  SaptoSince2000[18].upr_couple = SAPTO_LWR_MARRIED_2018 + SAPTO_MAX_MARRIED_2018 / SAPTO_TAPER;
  SaptoSince2000[18].taper = 1/8;
  
  SaptoSince2000[18].first_tax_rate = ORD_TAX_RATES_2018[1];
  SaptoSince2000[18].tax_free_thresh = ORD_TAX_BRACK_2018[1];
  SaptoSince2000[18].lito_max_offset = LITO_MAX_OFFSET_2018;
  
  return 0;
}
int set_Sapto2019() {
  SaptoSince2000[19].year = 2019;
  SaptoSince2000[19].pension_age = 65;
  SaptoSince2000[19].mxo_single = SAPTO_MAX_SINGLE_2019;
  SaptoSince2000[19].mxo_couple = SAPTO_MAX_MARRIED_2019;
  SaptoSince2000[19].lwr_single = SAPTO_LWR_SINGLE_2019;
  SaptoSince2000[19].lwr_couple = SAPTO_LWR_MARRIED_2019;
  SaptoSince2000[19].upr_single = SAPTO_LWR_SINGLE_2019 + SAPTO_MAX_SINGLE_2019 / SAPTO_TAPER;
  SaptoSince2000[19].upr_couple = SAPTO_LWR_MARRIED_2019 + SAPTO_MAX_MARRIED_2019 / SAPTO_TAPER;
  SaptoSince2000[19].taper = 1/8;
  
  SaptoSince2000[19].first_tax_rate = ORD_TAX_RATES_2019[1];
  SaptoSince2000[19].tax_free_thresh = ORD_TAX_BRACK_2019[1];
  SaptoSince2000[19].lito_max_offset = LITO_MAX_OFFSET_2019;
  
  return 0;
}
int set_Sapto2020() {
  SaptoSince2000[20].year = 2020;
  SaptoSince2000[20].pension_age = 65;
  SaptoSince2000[20].mxo_single = SAPTO_MAX_SINGLE_2020;
  SaptoSince2000[20].mxo_couple = SAPTO_MAX_MARRIED_2020;
  SaptoSince2000[20].lwr_single = SAPTO_LWR_SINGLE_2020;
  SaptoSince2000[20].lwr_couple = SAPTO_LWR_MARRIED_2020;
  SaptoSince2000[20].upr_single = SAPTO_LWR_SINGLE_2020 + SAPTO_MAX_SINGLE_2020 / SAPTO_TAPER;
  SaptoSince2000[20].upr_couple = SAPTO_LWR_MARRIED_2020 + SAPTO_MAX_MARRIED_2020 / SAPTO_TAPER;
  SaptoSince2000[20].taper = 1/8;
  
  SaptoSince2000[20].first_tax_rate = ORD_TAX_RATES_2020[1];
  SaptoSince2000[20].tax_free_thresh = ORD_TAX_BRACK_2020[1];
  SaptoSince2000[20].lito_max_offset = LITO_MAX_OFFSET_2020;
  
  return 0;
}
int set_Sapto2021() {
  SaptoSince2000[21].year = 2021;
  SaptoSince2000[21].pension_age = 65;
  SaptoSince2000[21].mxo_single = SAPTO_MAX_SINGLE_2021;
  SaptoSince2000[21].mxo_couple = SAPTO_MAX_MARRIED_2021;
  SaptoSince2000[21].lwr_single = SAPTO_LWR_SINGLE_2021;
  SaptoSince2000[21].lwr_couple = SAPTO_LWR_MARRIED_2021;
  SaptoSince2000[21].upr_single = SAPTO_LWR_SINGLE_2021 + SAPTO_MAX_SINGLE_2021 / SAPTO_TAPER;
  SaptoSince2000[21].upr_couple = SAPTO_LWR_MARRIED_2021 + SAPTO_MAX_MARRIED_2021 / SAPTO_TAPER;
  SaptoSince2000[21].taper = 1/8;
  
  SaptoSince2000[21].first_tax_rate = ORD_TAX_RATES_2021[1];
  SaptoSince2000[21].tax_free_thresh = ORD_TAX_BRACK_2021[1];
  SaptoSince2000[21].lito_max_offset = LITO_MAX_OFFSET_2021;
  
  return 0;
}
int set_Sapto2022() {
  SaptoSince2000[22].year = 2022;
  SaptoSince2000[22].pension_age = 65;
  SaptoSince2000[22].mxo_single = SAPTO_MAX_SINGLE_2022;
  SaptoSince2000[22].mxo_couple = SAPTO_MAX_MARRIED_2022;
  SaptoSince2000[22].lwr_single = SAPTO_LWR_SINGLE_2022;
  SaptoSince2000[22].lwr_couple = SAPTO_LWR_MARRIED_2022;
  SaptoSince2000[22].upr_single = SAPTO_LWR_SINGLE_2022 + SAPTO_MAX_SINGLE_2022 / SAPTO_TAPER;
  SaptoSince2000[22].upr_couple = SAPTO_LWR_MARRIED_2022 + SAPTO_MAX_MARRIED_2022 / SAPTO_TAPER;
  SaptoSince2000[22].taper = 1/8;
  
  SaptoSince2000[22].first_tax_rate = ORD_TAX_RATES_2022[1];
  SaptoSince2000[22].tax_free_thresh = ORD_TAX_BRACK_2022[1];
  SaptoSince2000[22].lito_max_offset = LITO_MAX_OFFSET_2022;
  
  return 0;
}
int set_Sapto2023() {
  SaptoSince2000[23].year = 2023;
  SaptoSince2000[23].pension_age = 65;
  SaptoSince2000[23].mxo_single = SAPTO_MAX_SINGLE_2023;
  SaptoSince2000[23].mxo_couple = SAPTO_MAX_MARRIED_2023;
  SaptoSince2000[23].lwr_single = SAPTO_LWR_SINGLE_2023;
  SaptoSince2000[23].lwr_couple = SAPTO_LWR_MARRIED_2023;
  SaptoSince2000[23].upr_single = SAPTO_LWR_SINGLE_2023 + SAPTO_MAX_SINGLE_2023 / SAPTO_TAPER;
  SaptoSince2000[23].upr_couple = SAPTO_LWR_MARRIED_2023 + SAPTO_MAX_MARRIED_2023 / SAPTO_TAPER;
  SaptoSince2000[23].taper = 1/8;
  
  SaptoSince2000[23].first_tax_rate = ORD_TAX_RATES_2023[1];
  SaptoSince2000[23].tax_free_thresh = ORD_TAX_BRACK_2023[1];
  SaptoSince2000[23].lito_max_offset = LITO_MAX_OFFSET_2023;
  
  return 0;
}
int set_Sapto2024() {
  SaptoSince2000[24].year = 2024;
  SaptoSince2000[24].pension_age = 65;
  SaptoSince2000[24].mxo_single = SAPTO_MAX_SINGLE_2024;
  SaptoSince2000[24].mxo_couple = SAPTO_MAX_MARRIED_2024;
  SaptoSince2000[24].lwr_single = SAPTO_LWR_SINGLE_2024;
  SaptoSince2000[24].lwr_couple = SAPTO_LWR_MARRIED_2024;
  SaptoSince2000[24].upr_single = SAPTO_LWR_SINGLE_2024 + SAPTO_MAX_SINGLE_2024 / SAPTO_TAPER;
  SaptoSince2000[24].upr_couple = SAPTO_LWR_MARRIED_2024 + SAPTO_MAX_MARRIED_2024 / SAPTO_TAPER;
  SaptoSince2000[24].taper = 1/8;
  
  SaptoSince2000[24].first_tax_rate = ORD_TAX_RATES_2024[1];
  SaptoSince2000[24].tax_free_thresh = ORD_TAX_BRACK_2024[1];
  SaptoSince2000[24].lito_max_offset = LITO_MAX_OFFSET_2024;
  
  return 0;
}
int set_Sapto2025() {
  SaptoSince2000[25].year = 2025;
  SaptoSince2000[25].pension_age = 65;
  SaptoSince2000[25].mxo_single = SAPTO_MAX_SINGLE_2025;
  SaptoSince2000[25].mxo_couple = SAPTO_MAX_MARRIED_2025;
  SaptoSince2000[25].lwr_single = SAPTO_LWR_SINGLE_2025;
  SaptoSince2000[25].lwr_couple = SAPTO_LWR_MARRIED_2025;
  SaptoSince2000[25].upr_single = SAPTO_LWR_SINGLE_2025 + SAPTO_MAX_SINGLE_2025 / SAPTO_TAPER;
  SaptoSince2000[25].upr_couple = SAPTO_LWR_MARRIED_2025 + SAPTO_MAX_MARRIED_2025 / SAPTO_TAPER;
  SaptoSince2000[25].taper = 1/8;
  
  SaptoSince2000[25].first_tax_rate = ORD_TAX_RATES_2025[1];
  SaptoSince2000[25].tax_free_thresh = ORD_TAX_BRACK_2025[1];
  SaptoSince2000[25].lito_max_offset = LITO_MAX_OFFSET_2025;
  
  return 0;
}
int set_Sapto2026() {
  SaptoSince2000[26].year = 2026;
  SaptoSince2000[26].pension_age = 65;
  SaptoSince2000[26].mxo_single = SAPTO_MAX_SINGLE_2026;
  SaptoSince2000[26].mxo_couple = SAPTO_MAX_MARRIED_2026;
  SaptoSince2000[26].lwr_single = SAPTO_LWR_SINGLE_2026;
  SaptoSince2000[26].lwr_couple = SAPTO_LWR_MARRIED_2026;
  SaptoSince2000[26].upr_single = SAPTO_LWR_SINGLE_2026 + SAPTO_MAX_SINGLE_2026 / SAPTO_TAPER;
  SaptoSince2000[26].upr_couple = SAPTO_LWR_MARRIED_2026 + SAPTO_MAX_MARRIED_2026 / SAPTO_TAPER;
  SaptoSince2000[26].taper = 1/8;
  
  SaptoSince2000[26].first_tax_rate = ORD_TAX_RATES_2026[1];
  SaptoSince2000[26].tax_free_thresh = ORD_TAX_BRACK_2026[1];
  SaptoSince2000[26].lito_max_offset = LITO_MAX_OFFSET_2026;
  
  return 0;
}
int set_Sapto2027() {
  SaptoSince2000[27].year = 2027;
  SaptoSince2000[27].pension_age = 65;
  SaptoSince2000[27].mxo_single = SAPTO_MAX_SINGLE_2027;
  SaptoSince2000[27].mxo_couple = SAPTO_MAX_MARRIED_2027;
  SaptoSince2000[27].lwr_single = SAPTO_LWR_SINGLE_2027;
  SaptoSince2000[27].lwr_couple = SAPTO_LWR_MARRIED_2027;
  SaptoSince2000[27].upr_single = SAPTO_LWR_SINGLE_2027 + SAPTO_MAX_SINGLE_2027 / SAPTO_TAPER;
  SaptoSince2000[27].upr_couple = SAPTO_LWR_MARRIED_2027 + SAPTO_MAX_MARRIED_2027 / SAPTO_TAPER;
  SaptoSince2000[27].taper = 1/8;
  
  SaptoSince2000[27].first_tax_rate = ORD_TAX_RATES_2027[1];
  SaptoSince2000[27].tax_free_thresh = ORD_TAX_BRACK_2027[1];
  SaptoSince2000[27].lito_max_offset = LITO_MAX_OFFSET_2027;
  
  return 0;
}
int set_Sapto2028() {
  SaptoSince2000[28].year = 2028;
  SaptoSince2000[28].pension_age = 65;
  SaptoSince2000[28].mxo_single = SAPTO_MAX_SINGLE_2028;
  SaptoSince2000[28].mxo_couple = SAPTO_MAX_MARRIED_2028;
  SaptoSince2000[28].lwr_single = SAPTO_LWR_SINGLE_2028;
  SaptoSince2000[28].lwr_couple = SAPTO_LWR_MARRIED_2028;
  SaptoSince2000[28].upr_single = SAPTO_LWR_SINGLE_2028 + SAPTO_MAX_SINGLE_2028 / SAPTO_TAPER;
  SaptoSince2000[28].upr_couple = SAPTO_LWR_MARRIED_2028 + SAPTO_MAX_MARRIED_2028 / SAPTO_TAPER;
  SaptoSince2000[28].taper = 1/8;
  
  SaptoSince2000[28].first_tax_rate = ORD_TAX_RATES_2028[1];
  SaptoSince2000[28].tax_free_thresh = ORD_TAX_BRACK_2028[1];
  SaptoSince2000[28].lito_max_offset = LITO_MAX_OFFSET_2028;
  
  return 0;
}
int set_Sapto2029() {
  SaptoSince2000[29].year = 2029;
  SaptoSince2000[29].pension_age = 65;
  SaptoSince2000[29].mxo_single = SAPTO_MAX_SINGLE_2029;
  SaptoSince2000[29].mxo_couple = SAPTO_MAX_MARRIED_2029;
  SaptoSince2000[29].lwr_single = SAPTO_LWR_SINGLE_2029;
  SaptoSince2000[29].lwr_couple = SAPTO_LWR_MARRIED_2029;
  SaptoSince2000[29].upr_single = SAPTO_LWR_SINGLE_2029 + SAPTO_MAX_SINGLE_2029 / SAPTO_TAPER;
  SaptoSince2000[29].upr_couple = SAPTO_LWR_MARRIED_2029 + SAPTO_MAX_MARRIED_2029 / SAPTO_TAPER;
  SaptoSince2000[29].taper = 1/8;
  
  SaptoSince2000[29].first_tax_rate = ORD_TAX_RATES_2029[1];
  SaptoSince2000[29].tax_free_thresh = ORD_TAX_BRACK_2029[1];
  SaptoSince2000[29].lito_max_offset = LITO_MAX_OFFSET_2029;
  
  return 0;
}
int set_Sapto2030() {
  SaptoSince2000[30].year = 2030;
  SaptoSince2000[30].pension_age = 65;
  SaptoSince2000[30].mxo_single = SAPTO_MAX_SINGLE_2030;
  SaptoSince2000[30].mxo_couple = SAPTO_MAX_MARRIED_2030;
  SaptoSince2000[30].lwr_single = SAPTO_LWR_SINGLE_2030;
  SaptoSince2000[30].lwr_couple = SAPTO_LWR_MARRIED_2030;
  SaptoSince2000[30].upr_single = SAPTO_LWR_SINGLE_2030 + SAPTO_MAX_SINGLE_2030 / SAPTO_TAPER;
  SaptoSince2000[30].upr_couple = SAPTO_LWR_MARRIED_2030 + SAPTO_MAX_MARRIED_2030 / SAPTO_TAPER;
  SaptoSince2000[30].taper = 1/8;
  
  SaptoSince2000[30].first_tax_rate = ORD_TAX_RATES_2030[1];
  SaptoSince2000[30].tax_free_thresh = ORD_TAX_BRACK_2030[1];
  SaptoSince2000[30].lito_max_offset = LITO_MAX_OFFSET_2030;
  
  return 0;
}

bool o = 
  set_Sapto2000() || 
  set_Sapto2001() || 
  set_Sapto2002() || 
  set_Sapto2003() || 
  set_Sapto2004() || 
  set_Sapto2005() || 
  set_Sapto2006() || 
  set_Sapto2007() || 
  set_Sapto2008() || 
  set_Sapto2009() || 
  set_Sapto2010() || 
  set_Sapto2011() || 
  set_Sapto2012() || 
  set_Sapto2013() || 
  set_Sapto2014() || 
  set_Sapto2015() || 
  set_Sapto2016() || 
  set_Sapto2017() || 
  set_Sapto2018() || 
  set_Sapto2019() || 
  set_Sapto2020() || 
  set_Sapto2021() || 
  set_Sapto2022() || 
  set_Sapto2023() || 
  set_Sapto2024() || 
  set_Sapto2025() || 
  set_Sapto2026() || 
  set_Sapto2027() || 
  set_Sapto2028() || 
  set_Sapto2029() || 
  set_Sapto2030();

