#include "grattan.h"
#include "grattanMedicareLevy.h"


double do_1_medicare_levy_1990(double xd, double yd, bool is_family, int n_dependants) {
  double threshold =
    is_family ? ML_LWR_THRESHOLD_FAMILY_1990 : ML_LWR_THRESHOLD_SINGLE_1990;
  double upper_threshold = 
    is_family ? ML_UPR_THRESHOLD_FAMILY_1990 : ML_UPR_THRESHOLD_SINGLE_1990;
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_1990 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[1990 - 1984] * xd;
  }
  return ML_TAPER_1984_____[1990 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_1991(double xd, double yd, bool is_family, int n_dependants) {
  double threshold =
    is_family ? ML_LWR_THRESHOLD_FAMILY_1991 : ML_LWR_THRESHOLD_SINGLE_1991;
  double upper_threshold = 
    is_family ? ML_UPR_THRESHOLD_FAMILY_1991 : ML_UPR_THRESHOLD_SINGLE_1991;
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_1991 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[1991 - 1984] * xd;
  }
  return ML_TAPER_1984_____[1991 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_1992(double xd, double yd, bool is_family, int n_dependants) {
  double threshold =
    is_family ? ML_LWR_THRESHOLD_FAMILY_1992 : ML_LWR_THRESHOLD_SINGLE_1992;
  double upper_threshold = 
    is_family ? ML_UPR_THRESHOLD_FAMILY_1992 : ML_UPR_THRESHOLD_SINGLE_1992;
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_1992 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[1992 - 1984] * xd;
  }
  return ML_TAPER_1984_____[1992 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_1993(double xd, double yd, bool is_family, int n_dependants) {
  double threshold =
    is_family ? ML_LWR_THRESHOLD_FAMILY_1993 : ML_LWR_THRESHOLD_SINGLE_1993;
  double upper_threshold = 
    is_family ? ML_UPR_THRESHOLD_FAMILY_1993 : ML_UPR_THRESHOLD_SINGLE_1993;
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_1993 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[1993 - 1984] * xd;
  }
  return ML_TAPER_1984_____[1993 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_1994(double xd, double yd, bool is_family, int n_dependants) {
  double threshold =
    is_family ? ML_LWR_THRESHOLD_FAMILY_1994 : ML_LWR_THRESHOLD_SINGLE_1994;
  double upper_threshold = 
    is_family ? ML_UPR_THRESHOLD_FAMILY_1994 : ML_UPR_THRESHOLD_SINGLE_1994;
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_1994 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[1994 - 1984] * xd;
  }
  return ML_TAPER_1984_____[1994 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_1995(double xd, double yd, bool is_family, int n_dependants) {
  double threshold =
    is_family ? ML_LWR_THRESHOLD_FAMILY_1995 : ML_LWR_THRESHOLD_SINGLE_1995;
  double upper_threshold = 
    is_family ? ML_UPR_THRESHOLD_FAMILY_1995 : ML_UPR_THRESHOLD_SINGLE_1995;
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_1995 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[1995 - 1984] * xd;
  }
  return ML_TAPER_1984_____[1995 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_1996(double xd, double yd, bool is_family, int n_dependants) {
  double threshold =
    is_family ? ML_LWR_THRESHOLD_FAMILY_1996 : ML_LWR_THRESHOLD_SINGLE_1996;
  double upper_threshold = 
    is_family ? ML_UPR_THRESHOLD_FAMILY_1996 : ML_UPR_THRESHOLD_SINGLE_1996;
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_1996 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[1996 - 1984] * xd;
  }
  return ML_TAPER_1984_____[1996 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_1997(double xd, double yd, bool is_family, int n_dependants) {
  double threshold =
    is_family ? ML_LWR_THRESHOLD_FAMILY_1997 : ML_LWR_THRESHOLD_SINGLE_1997;
  double upper_threshold = 
    is_family ? ML_UPR_THRESHOLD_FAMILY_1997 : ML_UPR_THRESHOLD_SINGLE_1997;
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_1997 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[1997 - 1984] * xd;
  }
  return ML_TAPER_1984_____[1997 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_1998(double xd, double yd, bool is_family, int n_dependants) {
  double threshold =
    is_family ? ML_LWR_THRESHOLD_FAMILY_1998 : ML_LWR_THRESHOLD_SINGLE_1998;
  double upper_threshold = 
    is_family ? ML_UPR_THRESHOLD_FAMILY_1998 : ML_UPR_THRESHOLD_SINGLE_1998;
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_1998 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[1998 - 1984] * xd;
  }
  return ML_TAPER_1984_____[1998 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_1999(double xd, double yd, bool is_family, int n_dependants) {
  double threshold =
    is_family ? ML_LWR_THRESHOLD_FAMILY_1999 : ML_LWR_THRESHOLD_SINGLE_1999;
  double upper_threshold = 
    is_family ? ML_UPR_THRESHOLD_FAMILY_1999 : ML_UPR_THRESHOLD_SINGLE_1999;
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_1999 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[1999 - 1984] * xd;
  }
  return ML_TAPER_1984_____[1999 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2000(double xd, double yd, bool is_family, int n_dependants) {
  double threshold =
    is_family ? ML_LWR_THRESHOLD_FAMILY_2000 : ML_LWR_THRESHOLD_SINGLE_2000;
  double upper_threshold = 
    is_family ? ML_UPR_THRESHOLD_FAMILY_2000 : ML_UPR_THRESHOLD_SINGLE_2000;
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2000 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2000 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2000 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2001(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2001 : ML_LWR_THRESHOLD_FAMILY_2001) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2001 : ML_LWR_THRESHOLD_SINGLE_2001);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2001 : ML_UPR_THRESHOLD_FAMILY_2001) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2001 : ML_UPR_THRESHOLD_SINGLE_2001);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2001 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2001 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2001 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2002(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2002 : ML_LWR_THRESHOLD_FAMILY_2002) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2002 : ML_LWR_THRESHOLD_SINGLE_2002);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2002 : ML_UPR_THRESHOLD_FAMILY_2002) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2002 : ML_UPR_THRESHOLD_SINGLE_2002);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2002 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2002 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2002 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2003(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2003 : ML_LWR_THRESHOLD_FAMILY_2003) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2003 : ML_LWR_THRESHOLD_SINGLE_2003);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2003 : ML_UPR_THRESHOLD_FAMILY_2003) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2003 : ML_UPR_THRESHOLD_SINGLE_2003);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2003 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2003 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2003 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2004(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2004 : ML_LWR_THRESHOLD_FAMILY_2004) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2004 : ML_LWR_THRESHOLD_SINGLE_2004);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2004 : ML_UPR_THRESHOLD_FAMILY_2004) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2004 : ML_UPR_THRESHOLD_SINGLE_2004);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2004 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2004 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2004 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2005(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2005 : ML_LWR_THRESHOLD_FAMILY_2005) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2005 : ML_LWR_THRESHOLD_SINGLE_2005);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2005 : ML_UPR_THRESHOLD_FAMILY_2005) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2005 : ML_UPR_THRESHOLD_SINGLE_2005);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2005 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2005 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2005 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2006(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2006 : ML_LWR_THRESHOLD_FAMILY_2006) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2006 : ML_LWR_THRESHOLD_SINGLE_2006);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2006 : ML_UPR_THRESHOLD_FAMILY_2006) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2006 : ML_UPR_THRESHOLD_SINGLE_2006);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2006 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2006 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2006 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2007(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2007 : ML_LWR_THRESHOLD_FAMILY_2007) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2007 : ML_LWR_THRESHOLD_SINGLE_2007);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2007 : ML_UPR_THRESHOLD_FAMILY_2007) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2007 : ML_UPR_THRESHOLD_SINGLE_2007);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2007 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2007 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2007 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2008(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2008 : ML_LWR_THRESHOLD_FAMILY_2008) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2008 : ML_LWR_THRESHOLD_SINGLE_2008);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2008 : ML_UPR_THRESHOLD_FAMILY_2008) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2008 : ML_UPR_THRESHOLD_SINGLE_2008);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2008 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2008 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2008 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2009(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2009 : ML_LWR_THRESHOLD_FAMILY_2009) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2009 : ML_LWR_THRESHOLD_SINGLE_2009);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2009 : ML_UPR_THRESHOLD_FAMILY_2009) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2009 : ML_UPR_THRESHOLD_SINGLE_2009);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2009 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2009 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2009 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2010(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2010 : ML_LWR_THRESHOLD_FAMILY_2010) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2010 : ML_LWR_THRESHOLD_SINGLE_2010);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2010 : ML_UPR_THRESHOLD_FAMILY_2010) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2010 : ML_UPR_THRESHOLD_SINGLE_2010);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2010 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2010 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2010 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2011(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2011 : ML_LWR_THRESHOLD_FAMILY_2011) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2011 : ML_LWR_THRESHOLD_SINGLE_2011);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2011 : ML_UPR_THRESHOLD_FAMILY_2011) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2011 : ML_UPR_THRESHOLD_SINGLE_2011);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2011 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2011 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2011 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2012(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2012 : ML_LWR_THRESHOLD_FAMILY_2012) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2012 : ML_LWR_THRESHOLD_SINGLE_2012);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2012 : ML_UPR_THRESHOLD_FAMILY_2012) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2012 : ML_UPR_THRESHOLD_SINGLE_2012);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2012 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2012 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2012 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2013(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2013 : ML_LWR_THRESHOLD_FAMILY_2013) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2013 : ML_LWR_THRESHOLD_SINGLE_2013);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2013 : ML_UPR_THRESHOLD_FAMILY_2013) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2013 : ML_UPR_THRESHOLD_SINGLE_2013);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2013 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2013 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2013 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2014(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2014 : ML_LWR_THRESHOLD_FAMILY_2014) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2014 : ML_LWR_THRESHOLD_SINGLE_2014);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2014 : ML_UPR_THRESHOLD_FAMILY_2014) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2014 : ML_UPR_THRESHOLD_SINGLE_2014);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2014 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2014 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2014 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2015(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2015 : ML_LWR_THRESHOLD_FAMILY_2015) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2015 : ML_LWR_THRESHOLD_SINGLE_2015);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2015 : ML_UPR_THRESHOLD_FAMILY_2015) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2015 : ML_UPR_THRESHOLD_SINGLE_2015);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2015 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2015 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2015 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2016(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2016 : ML_LWR_THRESHOLD_FAMILY_2016) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2016 : ML_LWR_THRESHOLD_SINGLE_2016);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2016 : ML_UPR_THRESHOLD_FAMILY_2016) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2016 : ML_UPR_THRESHOLD_SINGLE_2016);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2016 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2016 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2016 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2017(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2017 : ML_LWR_THRESHOLD_FAMILY_2017) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2017 : ML_LWR_THRESHOLD_SINGLE_2017);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2017 : ML_UPR_THRESHOLD_FAMILY_2017) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2017 : ML_UPR_THRESHOLD_SINGLE_2017);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2017 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2017 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2017 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2018(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2018 : ML_LWR_THRESHOLD_FAMILY_2018) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2018 : ML_LWR_THRESHOLD_SINGLE_2018);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2018 : ML_UPR_THRESHOLD_FAMILY_2018) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2018 : ML_UPR_THRESHOLD_SINGLE_2018);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2018 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2018 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2018 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2019(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2019 : ML_LWR_THRESHOLD_FAMILY_2019) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2019 : ML_LWR_THRESHOLD_SINGLE_2019);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2019 : ML_UPR_THRESHOLD_FAMILY_2019) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2019 : ML_UPR_THRESHOLD_SINGLE_2019);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2019 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2019 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2019 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2020(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2020 : ML_LWR_THRESHOLD_FAMILY_2020) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2020 : ML_LWR_THRESHOLD_SINGLE_2020);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2020 : ML_UPR_THRESHOLD_FAMILY_2020) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2020 : ML_UPR_THRESHOLD_SINGLE_2020);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2020 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2020 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2020 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2021(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2021 : ML_LWR_THRESHOLD_FAMILY_2021) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2021 : ML_LWR_THRESHOLD_SINGLE_2021);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2021 : ML_UPR_THRESHOLD_FAMILY_2021) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2021 : ML_UPR_THRESHOLD_SINGLE_2021);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2021 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2021 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2021 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2022(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2022 : ML_LWR_THRESHOLD_FAMILY_2022) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2022 : ML_LWR_THRESHOLD_SINGLE_2022);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2022 : ML_UPR_THRESHOLD_FAMILY_2022) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2022 : ML_UPR_THRESHOLD_SINGLE_2022);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2022 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2022 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2022 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2023(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2023 : ML_LWR_THRESHOLD_FAMILY_2023) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2023 : ML_LWR_THRESHOLD_SINGLE_2023);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2023 : ML_UPR_THRESHOLD_FAMILY_2023) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2023 : ML_UPR_THRESHOLD_SINGLE_2023);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2023 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2023 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2023 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2024(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2024 : ML_LWR_THRESHOLD_FAMILY_2024) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2024 : ML_LWR_THRESHOLD_SINGLE_2024);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2024 : ML_UPR_THRESHOLD_FAMILY_2024) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2024 : ML_UPR_THRESHOLD_SINGLE_2024);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2024 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2024 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2024 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2025(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2025 : ML_LWR_THRESHOLD_FAMILY_2025) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2025 : ML_LWR_THRESHOLD_SINGLE_2025);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2025 : ML_UPR_THRESHOLD_FAMILY_2025) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2025 : ML_UPR_THRESHOLD_SINGLE_2025);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2025 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2025 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2025 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2026(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2026 : ML_LWR_THRESHOLD_FAMILY_2026) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2026 : ML_LWR_THRESHOLD_SINGLE_2026);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2026 : ML_UPR_THRESHOLD_FAMILY_2026) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2026 : ML_UPR_THRESHOLD_SINGLE_2026);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2026 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2026 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2026 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2027(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2027 : ML_LWR_THRESHOLD_FAMILY_2027) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2027 : ML_LWR_THRESHOLD_SINGLE_2027);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2027 : ML_UPR_THRESHOLD_FAMILY_2027) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2027 : ML_UPR_THRESHOLD_SINGLE_2027);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2027 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2027 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2027 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2028(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2028 : ML_LWR_THRESHOLD_FAMILY_2028) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2028 : ML_LWR_THRESHOLD_SINGLE_2028);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2028 : ML_UPR_THRESHOLD_FAMILY_2028) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2028 : ML_UPR_THRESHOLD_SINGLE_2028);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2028 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2028 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2028 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2029(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2029 : ML_LWR_THRESHOLD_FAMILY_2029) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2029 : ML_LWR_THRESHOLD_SINGLE_2029);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2029 : ML_UPR_THRESHOLD_FAMILY_2029) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2029 : ML_UPR_THRESHOLD_SINGLE_2029);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2029 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2029 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2029 - 1984] * (xd - threshold);
}


double do_1_medicare_levy_2030(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {
  double threshold =
    is_family ?
    (pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_2030 : ML_LWR_THRESHOLD_FAMILY_2030) :
  (pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_2030 : ML_LWR_THRESHOLD_SINGLE_2030);
  double upper_threshold = 
    is_family ? 
    (pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_2030 : ML_UPR_THRESHOLD_FAMILY_2030) :
    (pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_2030 : ML_UPR_THRESHOLD_SINGLE_2030);
  if (xd < threshold) {
    return 0;
  }
  double upr_over_lwr = (double)upper_threshold / (double)threshold;
  if (n_dependants) {
    threshold += ML_LWR_THR_UP_PER_CHILD_2030 * n_dependants;
    upper_threshold *= upr_over_lwr;
    upper_threshold = floor(upper_threshold);
  }
  if (xd > upper_threshold) {
    return ML_RATE_1984_____[2030 - 1984] * xd;
  }
  return ML_TAPER_1984_____[2030 - 1984] * (xd - threshold);
}


// [[Rcpp::export(rng = false)]]
DoubleVector medicare_levy_2018(DoubleVector x, DoubleVector y) {
  R_xlen_t N = x.length();
  DoubleVector out = no_init(N);
  for (R_xlen_t i = 0; i < N; ++i) {
    // do_1_medicare_levy_2018(double xd, double yd, bool is_family, bool pensioner, int n_dependants)
    out[i] = do_1_medicare_levy_2018(x[i], y[i], true, false, 0);
  }
  return out;
}

