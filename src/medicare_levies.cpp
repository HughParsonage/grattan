#include "grattanMedicareLevy.h"
#include "Medicare.h"


Medicare medicare_levies(int yr) {
  switch(yr) {
  case 1984:{
  Medicare M1984;
  M1984.lwr_single = ML_LWR_THRESHOLD_SINGLE_1984;
  M1984.upr_single = ML_UPR_THRESHOLD_SINGLE_1984;
  M1984.lwr_family = ML_LWR_THRESHOLD_FAMILY_1984;
  M1984.upr_family = ML_UPR_THRESHOLD_FAMILY_1984;
  M1984.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1984;
  M1984.taper = ML_TAPER_1984;
  M1984.rate = ML_RATE_1984;
  M1984.has_sapto_thr = 0;
  M1984.sapto_age = 65;
  return M1984;
}
  case 1985:{
    Medicare M1985;
    M1985.lwr_single = ML_LWR_THRESHOLD_SINGLE_1985;
    M1985.upr_single = ML_UPR_THRESHOLD_SINGLE_1985;
    M1985.lwr_family = ML_LWR_THRESHOLD_FAMILY_1985;
    M1985.upr_family = ML_UPR_THRESHOLD_FAMILY_1985;
    M1985.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1985;
    M1985.taper = ML_TAPER_1985;
    M1985.rate = ML_RATE_1985;
    M1985.has_sapto_thr = 0;
    M1985.sapto_age = 65;
    return M1985;
  }
  case 1986:{
    Medicare M1986;
    M1986.lwr_single = ML_LWR_THRESHOLD_SINGLE_1986;
    M1986.upr_single = ML_UPR_THRESHOLD_SINGLE_1986;
    M1986.lwr_family = ML_LWR_THRESHOLD_FAMILY_1986;
    M1986.upr_family = ML_UPR_THRESHOLD_FAMILY_1986;
    M1986.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1986;
    M1986.taper = ML_TAPER_1986;
    M1986.rate = ML_RATE_1986;
    M1986.has_sapto_thr = 0;
    M1986.sapto_age = 65;
    return M1986;
  }
  case 1987:{
    Medicare M1987;
    M1987.lwr_single = ML_LWR_THRESHOLD_SINGLE_1987;
    M1987.upr_single = ML_UPR_THRESHOLD_SINGLE_1987;
    M1987.lwr_family = ML_LWR_THRESHOLD_FAMILY_1987;
    M1987.upr_family = ML_UPR_THRESHOLD_FAMILY_1987;
    M1987.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1987;
    M1987.taper = ML_TAPER_1987;
    M1987.rate = ML_RATE_1987;
    M1987.has_sapto_thr = 0;
    M1987.sapto_age = 65;
    return M1987;
  }
  case 1988:{
    Medicare M1988;
    M1988.lwr_single = ML_LWR_THRESHOLD_SINGLE_1988;
    M1988.upr_single = ML_UPR_THRESHOLD_SINGLE_1988;
    M1988.lwr_family = ML_LWR_THRESHOLD_FAMILY_1988;
    M1988.upr_family = ML_UPR_THRESHOLD_FAMILY_1988;
    M1988.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1988;
    M1988.taper = ML_TAPER_1988;
    M1988.rate = ML_RATE_1988;
    M1988.has_sapto_thr = 0;
    M1988.sapto_age = 65;
    return M1988;
  }
  case 1989:{
    Medicare M1989;
    M1989.lwr_single = ML_LWR_THRESHOLD_SINGLE_1989;
    M1989.upr_single = ML_UPR_THRESHOLD_SINGLE_1989;
    M1989.lwr_family = ML_LWR_THRESHOLD_FAMILY_1989;
    M1989.upr_family = ML_UPR_THRESHOLD_FAMILY_1989;
    M1989.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1989;
    M1989.taper = ML_TAPER_1989;
    M1989.rate = ML_RATE_1989;
    M1989.has_sapto_thr = 0;
    M1989.sapto_age = 65;
    return M1989;
  }
  case 1990:{
    Medicare M1990;
    M1990.lwr_single = ML_LWR_THRESHOLD_SINGLE_1990;
    M1990.upr_single = ML_UPR_THRESHOLD_SINGLE_1990;
    M1990.lwr_family = ML_LWR_THRESHOLD_FAMILY_1990;
    M1990.upr_family = ML_UPR_THRESHOLD_FAMILY_1990;
    M1990.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1990;
    M1990.taper = ML_TAPER_1990;
    M1990.rate = ML_RATE_1990;
    M1990.has_sapto_thr = 0;
    M1990.sapto_age = 65;
    return M1990;
  }
  case 1991:{
    Medicare M1991;
    M1991.lwr_single = ML_LWR_THRESHOLD_SINGLE_1991;
    M1991.upr_single = ML_UPR_THRESHOLD_SINGLE_1991;
    M1991.lwr_family = ML_LWR_THRESHOLD_FAMILY_1991;
    M1991.upr_family = ML_UPR_THRESHOLD_FAMILY_1991;
    M1991.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1991;
    M1991.taper = ML_TAPER_1991;
    M1991.rate = ML_RATE_1991;
    M1991.has_sapto_thr = 0;
    M1991.sapto_age = 65;
    return M1991;
  }
  case 1992:{
    Medicare M1992;
    M1992.lwr_single = ML_LWR_THRESHOLD_SINGLE_1992;
    M1992.upr_single = ML_UPR_THRESHOLD_SINGLE_1992;
    M1992.lwr_family = ML_LWR_THRESHOLD_FAMILY_1992;
    M1992.upr_family = ML_UPR_THRESHOLD_FAMILY_1992;
    M1992.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1992;
    M1992.taper = ML_TAPER_1992;
    M1992.rate = ML_RATE_1992;
    M1992.has_sapto_thr = 0;
    M1992.sapto_age = 65;
    return M1992;
  }
  case 1993:{
    Medicare M1993;
    M1993.lwr_single = ML_LWR_THRESHOLD_SINGLE_1993;
    M1993.upr_single = ML_UPR_THRESHOLD_SINGLE_1993;
    M1993.lwr_family = ML_LWR_THRESHOLD_FAMILY_1993;
    M1993.upr_family = ML_UPR_THRESHOLD_FAMILY_1993;
    M1993.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1993;
    M1993.taper = ML_TAPER_1993;
    M1993.rate = ML_RATE_1993;
    M1993.has_sapto_thr = 0;
    M1993.sapto_age = 65;
    return M1993;
  }
  case 1994:{
    Medicare M1994;
    M1994.lwr_single = ML_LWR_THRESHOLD_SINGLE_1994;
    M1994.upr_single = ML_UPR_THRESHOLD_SINGLE_1994;
    M1994.lwr_family = ML_LWR_THRESHOLD_FAMILY_1994;
    M1994.upr_family = ML_UPR_THRESHOLD_FAMILY_1994;
    M1994.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1994;
    M1994.taper = ML_TAPER_1994;
    M1994.rate = ML_RATE_1994;
    M1994.has_sapto_thr = 0;
    M1994.sapto_age = 65;
    return M1994;
  }
  case 1995:{
    Medicare M1995;
    M1995.lwr_single = ML_LWR_THRESHOLD_SINGLE_1995;
    M1995.upr_single = ML_UPR_THRESHOLD_SINGLE_1995;
    M1995.lwr_family = ML_LWR_THRESHOLD_FAMILY_1995;
    M1995.upr_family = ML_UPR_THRESHOLD_FAMILY_1995;
    M1995.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1995;
    M1995.taper = ML_TAPER_1995;
    M1995.rate = ML_RATE_1995;
    M1995.has_sapto_thr = 0;
    M1995.sapto_age = 65;
    return M1995;
  }
  case 1996:{
    Medicare M1996;
    M1996.lwr_single = ML_LWR_THRESHOLD_SINGLE_1996;
    M1996.upr_single = ML_UPR_THRESHOLD_SINGLE_1996;
    M1996.lwr_family = ML_LWR_THRESHOLD_FAMILY_1996;
    M1996.upr_family = ML_UPR_THRESHOLD_FAMILY_1996;
    M1996.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1996;
    M1996.taper = ML_TAPER_1996;
    M1996.rate = ML_RATE_1996;
    M1996.has_sapto_thr = 0;
    M1996.sapto_age = 65;
    return M1996;
  }
  case 1997:{
    Medicare M1997;
    M1997.lwr_single = ML_LWR_THRESHOLD_SINGLE_1997;
    M1997.upr_single = ML_UPR_THRESHOLD_SINGLE_1997;
    M1997.lwr_family = ML_LWR_THRESHOLD_FAMILY_1997;
    M1997.upr_family = ML_UPR_THRESHOLD_FAMILY_1997;
    M1997.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1997;
    M1997.taper = ML_TAPER_1997;
    M1997.rate = ML_RATE_1997;
    M1997.has_sapto_thr = 0;
    M1997.sapto_age = 65;
    return M1997;
  }
  case 1998:{
    Medicare M1998;
    M1998.lwr_single = ML_LWR_THRESHOLD_SINGLE_1998;
    M1998.upr_single = ML_UPR_THRESHOLD_SINGLE_1998;
    M1998.lwr_family = ML_LWR_THRESHOLD_FAMILY_1998;
    M1998.upr_family = ML_UPR_THRESHOLD_FAMILY_1998;
    M1998.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1998;
    M1998.taper = ML_TAPER_1998;
    M1998.rate = ML_RATE_1998;
    M1998.has_sapto_thr = 0;
    M1998.sapto_age = 65;
    return M1998;
  }
  case 1999:{
    Medicare M1999;
    M1999.lwr_single = ML_LWR_THRESHOLD_SINGLE_1999;
    M1999.upr_single = ML_UPR_THRESHOLD_SINGLE_1999;
    M1999.lwr_family = ML_LWR_THRESHOLD_FAMILY_1999;
    M1999.upr_family = ML_UPR_THRESHOLD_FAMILY_1999;
    M1999.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1999;
    M1999.taper = ML_TAPER_1999;
    M1999.rate = ML_RATE_1999;
    M1999.has_sapto_thr = 0;
    M1999.sapto_age = 65;
    return M1999;
  }
  case 2000:{
    Medicare M2000;
    M2000.lwr_single = ML_LWR_THRESHOLD_SINGLE_2000;
    M2000.upr_single = ML_UPR_THRESHOLD_SINGLE_2000;
    M2000.lwr_family = ML_LWR_THRESHOLD_FAMILY_2000;
    M2000.upr_family = ML_UPR_THRESHOLD_FAMILY_2000;
    M2000.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2000;
    M2000.taper = ML_TAPER_2000;
    M2000.rate = ML_RATE_2000;
    M2000.has_sapto_thr = 0;
    M2000.sapto_age = 65;
    return M2000;
  }
  case 2001:{
    Medicare M2001;
    M2001.lwr_single = ML_LWR_THRESHOLD_SINGLE_2001;
    M2001.upr_single = ML_UPR_THRESHOLD_SINGLE_2001;
    M2001.lwr_family = ML_LWR_THRESHOLD_FAMILY_2001;
    M2001.upr_family = ML_UPR_THRESHOLD_FAMILY_2001;
    M2001.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2001;
    M2001.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2001;
    M2001.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2001;
    M2001.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2001;
    M2001.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2001;
    M2001.taper = ML_TAPER_2001;
    M2001.rate = ML_RATE_2001;
    M2001.has_sapto_thr = 1;
    M2001.sapto_age = 65;
    return M2001;
  }
  case 2002:{
    Medicare M2002;
    M2002.lwr_single = ML_LWR_THRESHOLD_SINGLE_2002;
    M2002.upr_single = ML_UPR_THRESHOLD_SINGLE_2002;
    M2002.lwr_family = ML_LWR_THRESHOLD_FAMILY_2002;
    M2002.upr_family = ML_UPR_THRESHOLD_FAMILY_2002;
    M2002.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2002;
    M2002.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2002;
    M2002.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2002;
    M2002.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2002;
    M2002.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2002;
    M2002.taper = ML_TAPER_2002;
    M2002.rate = ML_RATE_2002;
    M2002.has_sapto_thr = 1;
    M2002.sapto_age = 65;
    return M2002;
  }
  case 2003:{
    Medicare M2003;
    M2003.lwr_single = ML_LWR_THRESHOLD_SINGLE_2003;
    M2003.upr_single = ML_UPR_THRESHOLD_SINGLE_2003;
    M2003.lwr_family = ML_LWR_THRESHOLD_FAMILY_2003;
    M2003.upr_family = ML_UPR_THRESHOLD_FAMILY_2003;
    M2003.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2003;
    M2003.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2003;
    M2003.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2003;
    M2003.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2003;
    M2003.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2003;
    M2003.taper = ML_TAPER_2003;
    M2003.rate = ML_RATE_2003;
    M2003.has_sapto_thr = 1;
    M2003.sapto_age = 65;
    return M2003;
  }
  case 2004:{
    Medicare M2004;
    M2004.lwr_single = ML_LWR_THRESHOLD_SINGLE_2004;
    M2004.upr_single = ML_UPR_THRESHOLD_SINGLE_2004;
    M2004.lwr_family = ML_LWR_THRESHOLD_FAMILY_2004;
    M2004.upr_family = ML_UPR_THRESHOLD_FAMILY_2004;
    M2004.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2004;
    M2004.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2004;
    M2004.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2004;
    M2004.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2004;
    M2004.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2004;
    M2004.taper = ML_TAPER_2004;
    M2004.rate = ML_RATE_2004;
    M2004.has_sapto_thr = 1;
    M2004.sapto_age = 65;
    return M2004;
  }
  case 2005:{
    Medicare M2005;
    M2005.lwr_single = ML_LWR_THRESHOLD_SINGLE_2005;
    M2005.upr_single = ML_UPR_THRESHOLD_SINGLE_2005;
    M2005.lwr_family = ML_LWR_THRESHOLD_FAMILY_2005;
    M2005.upr_family = ML_UPR_THRESHOLD_FAMILY_2005;
    M2005.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2005;
    M2005.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2005;
    M2005.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2005;
    M2005.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2005;
    M2005.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2005;
    M2005.taper = ML_TAPER_2005;
    M2005.rate = ML_RATE_2005;
    M2005.has_sapto_thr = 1;
    M2005.sapto_age = 65;
    return M2005;
  }
  case 2006:{
    Medicare M2006;
    M2006.lwr_single = ML_LWR_THRESHOLD_SINGLE_2006;
    M2006.upr_single = ML_UPR_THRESHOLD_SINGLE_2006;
    M2006.lwr_family = ML_LWR_THRESHOLD_FAMILY_2006;
    M2006.upr_family = ML_UPR_THRESHOLD_FAMILY_2006;
    M2006.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2006;
    M2006.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2006;
    M2006.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2006;
    M2006.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2006;
    M2006.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2006;
    M2006.taper = ML_TAPER_2006;
    M2006.rate = ML_RATE_2006;
    M2006.has_sapto_thr = 1;
    M2006.sapto_age = 65;
    return M2006;
  }
  case 2007:{
    Medicare M2007;
    M2007.lwr_single = ML_LWR_THRESHOLD_SINGLE_2007;
    M2007.upr_single = ML_UPR_THRESHOLD_SINGLE_2007;
    M2007.lwr_family = ML_LWR_THRESHOLD_FAMILY_2007;
    M2007.upr_family = ML_UPR_THRESHOLD_FAMILY_2007;
    M2007.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2007;
    M2007.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2007;
    M2007.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2007;
    M2007.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2007;
    M2007.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2007;
    M2007.taper = ML_TAPER_2007;
    M2007.rate = ML_RATE_2007;
    M2007.has_sapto_thr = 1;
    M2007.sapto_age = 65;
    return M2007;
  }
  case 2008:{
    Medicare M2008;
    M2008.lwr_single = ML_LWR_THRESHOLD_SINGLE_2008;
    M2008.upr_single = ML_UPR_THRESHOLD_SINGLE_2008;
    M2008.lwr_family = ML_LWR_THRESHOLD_FAMILY_2008;
    M2008.upr_family = ML_UPR_THRESHOLD_FAMILY_2008;
    M2008.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2008;
    M2008.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2008;
    M2008.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2008;
    M2008.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2008;
    M2008.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2008;
    M2008.taper = ML_TAPER_2008;
    M2008.rate = ML_RATE_2008;
    M2008.has_sapto_thr = 1;
    M2008.sapto_age = 65;
    return M2008;
  }
  case 2009:{
    Medicare M2009;
    M2009.lwr_single = ML_LWR_THRESHOLD_SINGLE_2009;
    M2009.upr_single = ML_UPR_THRESHOLD_SINGLE_2009;
    M2009.lwr_family = ML_LWR_THRESHOLD_FAMILY_2009;
    M2009.upr_family = ML_UPR_THRESHOLD_FAMILY_2009;
    M2009.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2009;
    M2009.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2009;
    M2009.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2009;
    M2009.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2009;
    M2009.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2009;
    M2009.taper = ML_TAPER_2009;
    M2009.rate = ML_RATE_2009;
    M2009.has_sapto_thr = 1;
    M2009.sapto_age = 65;
    return M2009;
  }
  case 2010:{
    Medicare M2010;
    M2010.lwr_single = ML_LWR_THRESHOLD_SINGLE_2010;
    M2010.upr_single = ML_UPR_THRESHOLD_SINGLE_2010;
    M2010.lwr_family = ML_LWR_THRESHOLD_FAMILY_2010;
    M2010.upr_family = ML_UPR_THRESHOLD_FAMILY_2010;
    M2010.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2010;
    M2010.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2010;
    M2010.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2010;
    M2010.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2010;
    M2010.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2010;
    M2010.taper = ML_TAPER_2010;
    M2010.rate = ML_RATE_2010;
    M2010.has_sapto_thr = 1;
    M2010.sapto_age = 65;
    return M2010;
  }
  case 2011:{
    Medicare M2011;
    M2011.lwr_single = ML_LWR_THRESHOLD_SINGLE_2011;
    M2011.upr_single = ML_UPR_THRESHOLD_SINGLE_2011;
    M2011.lwr_family = ML_LWR_THRESHOLD_FAMILY_2011;
    M2011.upr_family = ML_UPR_THRESHOLD_FAMILY_2011;
    M2011.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2011;
    M2011.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2011;
    M2011.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2011;
    M2011.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2011;
    M2011.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2011;
    M2011.taper = ML_TAPER_2011;
    M2011.rate = ML_RATE_2011;
    M2011.has_sapto_thr = 1;
    M2011.sapto_age = 65;
    return M2011;
  }
  case 2012:{
    Medicare M2012;
    M2012.lwr_single = ML_LWR_THRESHOLD_SINGLE_2012;
    M2012.upr_single = ML_UPR_THRESHOLD_SINGLE_2012;
    M2012.lwr_family = ML_LWR_THRESHOLD_FAMILY_2012;
    M2012.upr_family = ML_UPR_THRESHOLD_FAMILY_2012;
    M2012.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2012;
    M2012.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2012;
    M2012.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2012;
    M2012.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2012;
    M2012.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2012;
    M2012.taper = ML_TAPER_2012;
    M2012.rate = ML_RATE_2012;
    M2012.has_sapto_thr = 1;
    M2012.sapto_age = 65;
    return M2012;
  }
  case 2013:{
    Medicare M2013;
    M2013.lwr_single = ML_LWR_THRESHOLD_SINGLE_2013;
    M2013.upr_single = ML_UPR_THRESHOLD_SINGLE_2013;
    M2013.lwr_family = ML_LWR_THRESHOLD_FAMILY_2013;
    M2013.upr_family = ML_UPR_THRESHOLD_FAMILY_2013;
    M2013.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2013;
    M2013.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2013;
    M2013.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2013;
    M2013.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2013;
    M2013.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2013;
    M2013.taper = ML_TAPER_2013;
    M2013.rate = ML_RATE_2013;
    M2013.has_sapto_thr = 1;
    M2013.sapto_age = 65;
    return M2013;
  }
  case 2014:{
    Medicare M2014;
    M2014.lwr_single = ML_LWR_THRESHOLD_SINGLE_2014;
    M2014.upr_single = ML_UPR_THRESHOLD_SINGLE_2014;
    M2014.lwr_family = ML_LWR_THRESHOLD_FAMILY_2014;
    M2014.upr_family = ML_UPR_THRESHOLD_FAMILY_2014;
    M2014.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2014;
    M2014.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2014;
    M2014.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2014;
    M2014.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2014;
    M2014.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2014;
    M2014.taper = ML_TAPER_2014;
    M2014.rate = ML_RATE_2014;
    M2014.has_sapto_thr = 1;
    M2014.sapto_age = 65;
    return M2014;
  }
  case 2015:{
    Medicare M2015;
    M2015.lwr_single = ML_LWR_THRESHOLD_SINGLE_2015;
    M2015.upr_single = ML_UPR_THRESHOLD_SINGLE_2015;
    M2015.lwr_family = ML_LWR_THRESHOLD_FAMILY_2015;
    M2015.upr_family = ML_UPR_THRESHOLD_FAMILY_2015;
    M2015.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2015;
    M2015.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2015;
    M2015.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2015;
    M2015.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2015;
    M2015.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2015;
    M2015.taper = ML_TAPER_2015;
    M2015.rate = ML_RATE_2015;
    M2015.has_sapto_thr = 1;
    M2015.sapto_age = 65;
    return M2015;
  }
  case 2016:{
    Medicare M2016;
    M2016.lwr_single = ML_LWR_THRESHOLD_SINGLE_2016;
    M2016.upr_single = ML_UPR_THRESHOLD_SINGLE_2016;
    M2016.lwr_family = ML_LWR_THRESHOLD_FAMILY_2016;
    M2016.upr_family = ML_UPR_THRESHOLD_FAMILY_2016;
    M2016.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2016;
    M2016.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2016;
    M2016.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2016;
    M2016.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2016;
    M2016.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2016;
    M2016.taper = ML_TAPER_2016;
    M2016.rate = ML_RATE_2016;
    M2016.has_sapto_thr = 1;
    M2016.sapto_age = 65;
    return M2016;
  }
  case 2017:{
    Medicare M2017;
    M2017.lwr_single = ML_LWR_THRESHOLD_SINGLE_2017;
    M2017.upr_single = ML_UPR_THRESHOLD_SINGLE_2017;
    M2017.lwr_family = ML_LWR_THRESHOLD_FAMILY_2017;
    M2017.upr_family = ML_UPR_THRESHOLD_FAMILY_2017;
    M2017.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2017;
    M2017.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2017;
    M2017.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2017;
    M2017.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2017;
    M2017.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2017;
    M2017.taper = ML_TAPER_2017;
    M2017.rate = ML_RATE_2017;
    M2017.has_sapto_thr = 1;
    M2017.sapto_age = 65;
    return M2017;
  }
  case 2018:{
    Medicare M2018;
    M2018.lwr_single = ML_LWR_THRESHOLD_SINGLE_2018;
    M2018.upr_single = ML_UPR_THRESHOLD_SINGLE_2018;
    M2018.lwr_family = ML_LWR_THRESHOLD_FAMILY_2018;
    M2018.upr_family = ML_UPR_THRESHOLD_FAMILY_2018;
    M2018.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2018;
    M2018.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2018;
    M2018.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2018;
    M2018.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2018;
    M2018.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2018;
    M2018.taper = ML_TAPER_2018;
    M2018.rate = ML_RATE_2018;
    M2018.has_sapto_thr = 1;
    M2018.sapto_age = 65;
    return M2018;
  }
  case 2019:{
    Medicare M2019;
    M2019.lwr_single = ML_LWR_THRESHOLD_SINGLE_2019;
    M2019.upr_single = ML_UPR_THRESHOLD_SINGLE_2019;
    M2019.lwr_family = ML_LWR_THRESHOLD_FAMILY_2019;
    M2019.upr_family = ML_UPR_THRESHOLD_FAMILY_2019;
    M2019.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2019;
    M2019.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2019;
    M2019.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2019;
    M2019.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2019;
    M2019.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2019;
    M2019.taper = ML_TAPER_2019;
    M2019.rate = ML_RATE_2019;
    M2019.has_sapto_thr = 1;
    M2019.sapto_age = 65;
    return M2019;
  }
  case 2020:{
    Medicare M2020;
    M2020.lwr_single = ML_LWR_THRESHOLD_SINGLE_2020;
    M2020.upr_single = ML_UPR_THRESHOLD_SINGLE_2020;
    M2020.lwr_family = ML_LWR_THRESHOLD_FAMILY_2020;
    M2020.upr_family = ML_UPR_THRESHOLD_FAMILY_2020;
    M2020.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2020;
    M2020.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2020;
    M2020.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2020;
    M2020.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2020;
    M2020.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2020;
    M2020.taper = ML_TAPER_2020;
    M2020.rate = ML_RATE_2020;
    M2020.has_sapto_thr = 1;
    M2020.sapto_age = 65;
    return M2020;
  }
  case 2021:{
    Medicare M2021;
    M2021.lwr_single = ML_LWR_THRESHOLD_SINGLE_2021;
    M2021.upr_single = ML_UPR_THRESHOLD_SINGLE_2021;
    M2021.lwr_family = ML_LWR_THRESHOLD_FAMILY_2021;
    M2021.upr_family = ML_UPR_THRESHOLD_FAMILY_2021;
    M2021.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2021;
    M2021.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2021;
    M2021.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2021;
    M2021.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2021;
    M2021.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2021;
    M2021.taper = ML_TAPER_2021;
    M2021.rate = ML_RATE_2021;
    M2021.has_sapto_thr = 1;
    M2021.sapto_age = 65;
    return M2021;
  }
  case 2022:{
    Medicare M2022;
    M2022.lwr_single = ML_LWR_THRESHOLD_SINGLE_2022;
    M2022.upr_single = ML_UPR_THRESHOLD_SINGLE_2022;
    M2022.lwr_family = ML_LWR_THRESHOLD_FAMILY_2022;
    M2022.upr_family = ML_UPR_THRESHOLD_FAMILY_2022;
    M2022.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2022;
    M2022.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2022;
    M2022.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2022;
    M2022.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2022;
    M2022.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2022;
    M2022.taper = ML_TAPER_2022;
    M2022.rate = ML_RATE_2022;
    M2022.has_sapto_thr = 1;
    M2022.sapto_age = 65;
    return M2022;
  }
  case 2023:{
    Medicare M2023;
    M2023.lwr_single = ML_LWR_THRESHOLD_SINGLE_2023;
    M2023.upr_single = ML_UPR_THRESHOLD_SINGLE_2023;
    M2023.lwr_family = ML_LWR_THRESHOLD_FAMILY_2023;
    M2023.upr_family = ML_UPR_THRESHOLD_FAMILY_2023;
    M2023.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2023;
    M2023.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2023;
    M2023.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2023;
    M2023.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2023;
    M2023.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2023;
    M2023.taper = ML_TAPER_2023;
    M2023.rate = ML_RATE_2023;
    M2023.has_sapto_thr = 1;
    M2023.sapto_age = 65;
    return M2023;
  }
  case 2024:{
    Medicare M2024;
    M2024.lwr_single = ML_LWR_THRESHOLD_SINGLE_2024;
    M2024.upr_single = ML_UPR_THRESHOLD_SINGLE_2024;
    M2024.lwr_family = ML_LWR_THRESHOLD_FAMILY_2024;
    M2024.upr_family = ML_UPR_THRESHOLD_FAMILY_2024;
    M2024.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2024;
    M2024.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2024;
    M2024.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2024;
    M2024.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2024;
    M2024.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2024;
    M2024.taper = ML_TAPER_2024;
    M2024.rate = ML_RATE_2024;
    M2024.has_sapto_thr = 1;
    M2024.sapto_age = 65;
    return M2024;
  }
  case 2025:{
    Medicare M2025;
    M2025.lwr_single = ML_LWR_THRESHOLD_SINGLE_2025;
    M2025.upr_single = ML_UPR_THRESHOLD_SINGLE_2025;
    M2025.lwr_family = ML_LWR_THRESHOLD_FAMILY_2025;
    M2025.upr_family = ML_UPR_THRESHOLD_FAMILY_2025;
    M2025.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2025;
    M2025.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2025;
    M2025.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2025;
    M2025.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2025;
    M2025.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2025;
    M2025.taper = ML_TAPER_2025;
    M2025.rate = ML_RATE_2025;
    M2025.has_sapto_thr = 1;
    M2025.sapto_age = 65;
    return M2025;
  }
  case 2026:{
    Medicare M2026;
    M2026.lwr_single = ML_LWR_THRESHOLD_SINGLE_2026;
    M2026.upr_single = ML_UPR_THRESHOLD_SINGLE_2026;
    M2026.lwr_family = ML_LWR_THRESHOLD_FAMILY_2026;
    M2026.upr_family = ML_UPR_THRESHOLD_FAMILY_2026;
    M2026.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2026;
    M2026.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2026;
    M2026.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2026;
    M2026.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2026;
    M2026.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2026;
    M2026.taper = ML_TAPER_2026;
    M2026.rate = ML_RATE_2026;
    M2026.has_sapto_thr = 1;
    M2026.sapto_age = 65;
    return M2026;
  }
  case 2027:{
    Medicare M2027;
    M2027.lwr_single = ML_LWR_THRESHOLD_SINGLE_2027;
    M2027.upr_single = ML_UPR_THRESHOLD_SINGLE_2027;
    M2027.lwr_family = ML_LWR_THRESHOLD_FAMILY_2027;
    M2027.upr_family = ML_UPR_THRESHOLD_FAMILY_2027;
    M2027.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2027;
    M2027.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2027;
    M2027.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2027;
    M2027.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2027;
    M2027.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2027;
    M2027.taper = ML_TAPER_2027;
    M2027.rate = ML_RATE_2027;
    M2027.has_sapto_thr = 1;
    M2027.sapto_age = 65;
    return M2027;
  }
  case 2028:{
    Medicare M2028;
    M2028.lwr_single = ML_LWR_THRESHOLD_SINGLE_2028;
    M2028.upr_single = ML_UPR_THRESHOLD_SINGLE_2028;
    M2028.lwr_family = ML_LWR_THRESHOLD_FAMILY_2028;
    M2028.upr_family = ML_UPR_THRESHOLD_FAMILY_2028;
    M2028.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2028;
    M2028.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2028;
    M2028.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2028;
    M2028.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2028;
    M2028.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2028;
    M2028.taper = ML_TAPER_2028;
    M2028.rate = ML_RATE_2028;
    M2028.has_sapto_thr = 1;
    M2028.sapto_age = 65;
    return M2028;
  }
  case 2029:{
    Medicare M2029;
    M2029.lwr_single = ML_LWR_THRESHOLD_SINGLE_2029;
    M2029.upr_single = ML_UPR_THRESHOLD_SINGLE_2029;
    M2029.lwr_family = ML_LWR_THRESHOLD_FAMILY_2029;
    M2029.upr_family = ML_UPR_THRESHOLD_FAMILY_2029;
    M2029.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2029;
    M2029.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2029;
    M2029.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2029;
    M2029.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2029;
    M2029.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2029;
    M2029.taper = ML_TAPER_2029;
    M2029.rate = ML_RATE_2029;
    M2029.has_sapto_thr = 1;
    M2029.sapto_age = 65;
    return M2029;
  }
  case 2030:{
    Medicare M2030;
    M2030.lwr_single = ML_LWR_THRESHOLD_SINGLE_2030;
    M2030.upr_single = ML_UPR_THRESHOLD_SINGLE_2030;
    M2030.lwr_family = ML_LWR_THRESHOLD_FAMILY_2030;
    M2030.upr_family = ML_UPR_THRESHOLD_FAMILY_2030;
    M2030.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2030;
    M2030.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2030;
    M2030.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2030;
    M2030.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2030;
    M2030.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2030;
    M2030.taper = ML_TAPER_2030;
    M2030.rate = ML_RATE_2030;
    M2030.has_sapto_thr = 1;
    M2030.sapto_age = 65;
    return M2030;
  }
  }
  
  Medicare M2030;
  M2030.lwr_single = ML_LWR_THRESHOLD_SINGLE_2030;
  M2030.upr_single = ML_UPR_THRESHOLD_SINGLE_2030;
  M2030.lwr_family = ML_LWR_THRESHOLD_FAMILY_2030;
  M2030.upr_family = ML_UPR_THRESHOLD_FAMILY_2030;
  M2030.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2030;
  M2030.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2030;
  M2030.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2030;
  M2030.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2030;
  M2030.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2030;
  M2030.taper = ML_TAPER_2030;
  M2030.rate = ML_RATE_2030;
  M2030.has_sapto_thr = 1;
  M2030.sapto_age = 65;
  return M2030;
}
