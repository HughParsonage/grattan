#ifndef grattan_offset_h
#define grattan_offset_h

struct Offset1 {
	int offset_1st;
	int thresh_1st;
	double taper_1st;
	bool refundable;
};

struct Offset2 {
  int offset_1st;
  int thresh_1st;
  double taper_1st;
  int thresh_2nd;
  double taper_2nd;
  bool refundable;
};

struct OffsetN {
  int offset_1st;
  IntegerVector Thresholds;
  DoubleVector Tapers;
  R_xlen_t nb;
  bool refundable;
};

#endif



