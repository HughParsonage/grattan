
# 1.5.2.1

# grattan 1.5.2.0
### 2017-10-19
* New internal C++ functions for `income_tax`, and related functions

# grattan 1.5.1.2
### 2017-10-15
* Optional argument `age` in `income_tax` now `NULL` rather than `42`.  
The default argument continues to result in SAPTO being not applied if `.dots.ATO`.
However, if `.dots.ATO` is supplied (and the age variable has not been removed from it),
the individuals' SAPTO eligibility is determined by the age variable in `.dots.ATO`, rather
than setting each individual's SAPTO to 0.

### 2017-08-30
* Update labour force data. Avoid segfault in separate package in unit test.
* Added a `NEWS.md` file to track changes to the package.

### 2017-08-16
* Update wage, CPI, labour force data

### 2017-07-02
* Update wage and labour force data
* Fix breaking build due to change in dplyr API





