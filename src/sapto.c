#include "grattan.h"

// s12 of Income Tax Assessment (1936 Act) Regulation 2015
// specfies 6000 and 0.15 in the regulations
// http://classic.austlii.edu.au/au/legis/cth/consol_reg/ita1936ar2015352/s12.html
int SAPTO_S12_THRESH = 6000;

double SAPTO_S12_TAPER = 0.15;
double SAPTO_TAPER = 0.125;

Sapto SaptoSince2000[31];

int set_Sapto2000() {
  SaptoSince2000[0].year = 2000;
  SaptoSince2000[0].pension_age = 65;
  SaptoSince2000[0].mxo_single = SAPTO_MAX_SINGLE_2000;
  SaptoSince2000[0].mxo_couple = SAPTO_MAX_MARRIED_2000;
  SaptoSince2000[0].lwr_single = SAPTO_LWR_SINGLE_2000;
  SaptoSince2000[0].lwr_couple = SAPTO_LWR_MARRIED_2000;
  SaptoSince2000[0].upr_single = SAPTO_LWR_SINGLE_2000 + SAPTO_MAX_SINGLE_2000 / SAPTO_TAPER;
  SaptoSince2000[0].upr_couple = SAPTO_LWR_MARRIED_2000 + SAPTO_MAX_MARRIED_2000 / SAPTO_TAPER;
  SaptoSince2000[0].taper = 0.125;
  
  SaptoSince2000[0].first_tax_rate = ORDINARY_TAX_RATES_2000[1];
  SaptoSince2000[0].tax_free_thresh = ORDINARY_TAX_BRACKETS_2000[1];
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
  SaptoSince2000[1].taper = 0.125;
  
  SaptoSince2000[1].first_tax_rate = ORDINARY_TAX_RATES_2001[1];
  SaptoSince2000[1].tax_free_thresh = ORDINARY_TAX_BRACKETS_2001[1];
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
  SaptoSince2000[2].taper = 0.125;
  
  SaptoSince2000[2].first_tax_rate = ORDINARY_TAX_RATES_2002[1];
  SaptoSince2000[2].tax_free_thresh = ORDINARY_TAX_BRACKETS_2002[1];
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
  SaptoSince2000[3].taper = 0.125;
  
  SaptoSince2000[3].first_tax_rate = ORDINARY_TAX_RATES_2003[1];
  SaptoSince2000[3].tax_free_thresh = ORDINARY_TAX_BRACKETS_2003[1];
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
  SaptoSince2000[4].taper = 0.125;
  
  SaptoSince2000[4].first_tax_rate = ORDINARY_TAX_RATES_2004[1];
  SaptoSince2000[4].tax_free_thresh = ORDINARY_TAX_BRACKETS_2004[1];
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
  SaptoSince2000[5].taper = 0.125;
  
  SaptoSince2000[5].first_tax_rate = ORDINARY_TAX_RATES_2005[1];
  SaptoSince2000[5].tax_free_thresh = ORDINARY_TAX_BRACKETS_2005[1];
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
  SaptoSince2000[6].taper = 0.125;
  
  SaptoSince2000[6].first_tax_rate = ORDINARY_TAX_RATES_2006[1];
  SaptoSince2000[6].tax_free_thresh = ORDINARY_TAX_BRACKETS_2006[1];
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
  SaptoSince2000[7].taper = 0.125;
  
  SaptoSince2000[7].first_tax_rate = ORDINARY_TAX_RATES_2007[1];
  SaptoSince2000[7].tax_free_thresh = ORDINARY_TAX_BRACKETS_2007[1];
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
  SaptoSince2000[8].taper = 0.125;
  
  SaptoSince2000[8].first_tax_rate = ORDINARY_TAX_RATES_2008[1];
  SaptoSince2000[8].tax_free_thresh = ORDINARY_TAX_BRACKETS_2008[1];
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
  SaptoSince2000[9].taper = 0.125;
  
  SaptoSince2000[9].first_tax_rate = ORDINARY_TAX_RATES_2009[1];
  SaptoSince2000[9].tax_free_thresh = ORDINARY_TAX_BRACKETS_2009[1];
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
  SaptoSince2000[10].taper = 0.125;
  
  SaptoSince2000[10].first_tax_rate = ORDINARY_TAX_RATES_2010[1];
  SaptoSince2000[10].tax_free_thresh = ORDINARY_TAX_BRACKETS_2010[1];
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
  SaptoSince2000[11].taper = 0.125;
  
  SaptoSince2000[11].first_tax_rate = ORDINARY_TAX_RATES_2011[1];
  SaptoSince2000[11].tax_free_thresh = ORDINARY_TAX_BRACKETS_2011[1];
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
  SaptoSince2000[12].taper = 0.125;
  
  SaptoSince2000[12].first_tax_rate = ORDINARY_TAX_RATES_2012[1];
  SaptoSince2000[12].tax_free_thresh = ORDINARY_TAX_BRACKETS_2012[1];
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
  SaptoSince2000[13].taper = 0.125;
  
  SaptoSince2000[13].first_tax_rate = ORDINARY_TAX_RATES_2013[1];
  SaptoSince2000[13].tax_free_thresh = ORDINARY_TAX_BRACKETS_2013[1];
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
  SaptoSince2000[14].taper = 0.125;
  
  SaptoSince2000[14].first_tax_rate = ORDINARY_TAX_RATES_2014[1];
  SaptoSince2000[14].tax_free_thresh = ORDINARY_TAX_BRACKETS_2014[1];
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
  SaptoSince2000[15].taper = 0.125;
  
  SaptoSince2000[15].first_tax_rate = ORDINARY_TAX_RATES_2015[1];
  SaptoSince2000[15].tax_free_thresh = ORDINARY_TAX_BRACKETS_2015[1];
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
  SaptoSince2000[16].taper = 0.125;
  
  SaptoSince2000[16].first_tax_rate = ORDINARY_TAX_RATES_2016[1];
  SaptoSince2000[16].tax_free_thresh = ORDINARY_TAX_BRACKETS_2016[1];
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
  SaptoSince2000[17].taper = 0.125;
  
  SaptoSince2000[17].first_tax_rate = ORDINARY_TAX_RATES_2017[1];
  SaptoSince2000[17].tax_free_thresh = ORDINARY_TAX_BRACKETS_2017[1];
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
  SaptoSince2000[18].taper = 0.125;
  
  SaptoSince2000[18].first_tax_rate = ORDINARY_TAX_RATES_2018[1];
  SaptoSince2000[18].tax_free_thresh = ORDINARY_TAX_BRACKETS_2018[1];
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
  SaptoSince2000[19].taper = 0.125;
  
  SaptoSince2000[19].first_tax_rate = ORDINARY_TAX_RATES_2019[1];
  SaptoSince2000[19].tax_free_thresh = ORDINARY_TAX_BRACKETS_2019[1];
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
  SaptoSince2000[20].taper = 0.125;
  
  SaptoSince2000[20].first_tax_rate = ORDINARY_TAX_RATES_2020[1];
  SaptoSince2000[20].tax_free_thresh = ORDINARY_TAX_BRACKETS_2020[1];
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
  SaptoSince2000[21].taper = 0.125;
  
  SaptoSince2000[21].first_tax_rate = ORDINARY_TAX_RATES_2021[1];
  SaptoSince2000[21].tax_free_thresh = ORDINARY_TAX_BRACKETS_2021[1];
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
  SaptoSince2000[22].taper = 0.125;
  
  SaptoSince2000[22].first_tax_rate = ORDINARY_TAX_RATES_2022[1];
  SaptoSince2000[22].tax_free_thresh = ORDINARY_TAX_BRACKETS_2022[1];
  SaptoSince2000[22].lito_max_offset = LITO_MAX_OFFSET_2022;
  
  return 0;
}

bool saptos_set = false;

void set_Saptos() {
  set_Sapto2000(); 
  set_Sapto2001(); 
  set_Sapto2002(); 
  set_Sapto2003(); 
  set_Sapto2004(); 
  set_Sapto2005(); 
  set_Sapto2006(); 
  set_Sapto2007(); 
  set_Sapto2008(); 
  set_Sapto2009(); 
  set_Sapto2010(); 
  set_Sapto2011(); 
  set_Sapto2012(); 
  set_Sapto2013(); 
  set_Sapto2014(); 
  set_Sapto2015(); 
  set_Sapto2016(); 
  set_Sapto2017(); 
  set_Sapto2018(); 
  set_Sapto2019(); 
  set_Sapto2020(); 
  set_Sapto2021(); 
  set_Sapto2022();
  saptos_set = true;
}


Sapto yr2Sapto(int yr) {
  if (!saptos_set) {
    set_Saptos();
  }
  
  if (yr < 2000 || yr > 2030) {
    Sapto S;
    S.year = yr;
    S.pension_age = 65;
    S.mxo_single = 0;
    S.mxo_couple = 0;
    S.lwr_single = 0;
    S.lwr_couple = 0;
    S.upr_single = 0;
    S.upr_couple = 0;
    S.first_tax_rate = 0;
    S.tax_free_thresh = 0;
    S.lito_max_offset = 0;
    return S;
  }
  if (yr > 2022) {
    return SaptoSince2000[2022 - 2000];
  }
  
  return SaptoSince2000[yr - 2000];
}


static double do_1_sapto_sf(int x, int y, int age, bool is_married, Sapto S) {
  // x is rebate income
  // y is spouse rebate income
  if (age < S.pension_age) {
    // ineligible
    return 0;
  }
  
  
  int max_offset = is_married ? S.mxo_couple : S.mxo_single;
  int lwr_thresh = is_married ? S.lwr_couple : S.lwr_single;
  double taper = S.taper;
  
  double o = x < lwr_thresh ? max_offset : dmax0(max_offset - taper * (x - lwr_thresh));
  if (!is_married) {
    return o;
  }
  
  // The transfer of unused SAPTO is very complex and frankly unknown, even
  // within govt.  This lines up 'better' than known models.
  
  // If the spouse's income is so high that no spouse SAPTO is 
  // transferrable, then we just fall back to the original 
  const int MAX_THR_SPOUSE_XFER_MARRIED = ceil(1602.0 / SAPTO_S12_TAPER + SAPTO_S12_THRESH);
  if (y > MAX_THR_SPOUSE_XFER_MARRIED) {
    return o;
  }
  
  double sp_unused_sapto = 
    (y < SAPTO_S12_THRESH) ? max_offset : dmax0(max_offset - SAPTO_S12_TAPER * (y - SAPTO_S12_THRESH));
  
  // https://www.ato.gov.au/individuals/income-and-deductions/in-detail/transferring-the-seniors-and-pensioners-tax-offset/
  // Following the lettering there
  double A = S.mxo_couple;
  double B = A + sp_unused_sapto;
  double C = B + S.lito_max_offset;
  double D = C / S.first_tax_rate;
  double E = D + S.tax_free_thresh;
  double adj_rebate_threshold = E;
  if (E > S.lito_1st_thresh) {
    double G = S.second_tax_rate - S.lito_1st_taper; // 0.34
    double H = G - S.first_tax_rate;                 // 0.15
    double I = H * S.lito_1st_thresh;                // 5550
    double J = S.first_tax_rate * S.tax_free_thresh; // 3458
    double K = J + S.lito_max_offset;                // 3903
    double L = K + max_offset;
    double M = L + sp_unused_sapto;
    double N = I + M;
    double O = G;
    double P = N / O;                                // 37226
    adj_rebate_threshold = P;
  }
  if (x < adj_rebate_threshold) {
    return B;
  }
  
  double DD = x - adj_rebate_threshold;
  double EE = DD * taper;
  double FF = B + EE;
  
  return dmax0(FF);
}

void apply_sapto(double * taxi, Person P, Sapto S) {
  double sapto = do_1_sapto_sf(P.ri, P.yi, P.agei, P.is_married, S);
  if (sapto >= *taxi) {
    *taxi = 0;
  } else {
    *taxi -= sapto;
  }
}

bool bw01(double x) {
  return !ISNAN(x) && x >= 0 && x <= 1;
}

static void errifnan(double x, bool warn, const char * var) {
  if (ISNAN(x)) {
    if (warn) {
      warning("%s is NaN", var);
    } else {
      error("%s was NaN", var);
    }
  }
}

int lwr_threshold(int mxo, int ord_thresh1, double ord_rate1, int max_lito) {
  double o = max_lito + mxo;
  o /= ord_rate1;
  o += ord_thresh1;
  return ceil(o); 
}

static bool valid_sapto_rel(int mxo, int lwr, int upr,
                            int ord_thresh1, double ord_rate1,
                            int max_lito, double taper) {
  int expected_lwr = lwr_threshold(mxo, ord_thresh1, ord_rate1, max_lito);
  if (expected_lwr != lwr) {
    return false;
  }
  if (taper == 0.125) {
    int expected_upr = lwr + (mxo << 3);
    if (expected_upr != upr) {
      return false;
    }
  } else {
    int expected_upr = ceil(lwr + ((double)mxo) / taper);
    if (expected_upr != upr) {
      return false;
    }
  }
  return true;
}

void validate_sapto(Sapto * S, int fix) {
  int year = S->year;
  if (year < MIN_YEAR) {
    error("Sapto.year = %d but must be %d or later", year, MIN_YEAR);
  }
  
  double pension_age = S->pension_age;
  if (ISNAN(pension_age)) {
    error("pension_age was NaN.");
  }
  if (R_finite(pension_age)) {
    if (pension_age > 150) {
      if (fix) {
        warning("`Sapto.pension_age = %f` and so will be set to positive infinity");
        S->pension_age = R_PosInf;
      } else {
        error("`Sapto.pension_age = %f` which is an unlikely value.");
      }
    }
  }
  
  int mxo_single = S->mxo_single;
  int mxo_couple = S->mxo_couple;
  
  int lwr_single = S->lwr_single;
  int lwr_couple = S->lwr_couple;
  
  int upr_single = S->upr_single;
  int upr_couple = S->upr_couple;
  double taper = S->taper;
  if (taper < 0) {
    if (fix) {
      warning("Sapto.taper < 0 and so sign will be reversed.");  
      S->taper = -taper;
    } else {
      error("S.taper < 0.");
    }
  }
  
  if (upr_single <= lwr_single) {
    S->upr_single = S->mxo_single / S->taper;
  }
  if (upr_couple <= lwr_couple) {
    S->upr_couple = S->mxo_couple / S->taper;
  }
  double first_tax_rate = S->first_tax_rate;
  double second_tax_rate = S->second_tax_rate;
  int tax_free_thresh = S->tax_free_thresh;
  int tax_2nd_thresh = S->tax_2nd_thresh;
  double lito_max_offset = S->lito_max_offset;
  double lito_1st_thresh = S->lito_1st_thresh;
  double lito_1st_taper = S->lito_1st_taper;
  
  
  if (!bw01(second_tax_rate)) {
    error("Sapto.second_tax_rate not in [0, 1]");
  }
  if (!bw01(first_tax_rate) || first_tax_rate > second_tax_rate) {
    error("Sapto.first_tax_rate must be between 0 and S.second_tax_rate");
  }
  
  
  
}



