#' Convenience functions for dealing with financial years
#' 
#' @name is.fy
#' @aliases fy.year yr2fy fy2yr fy2date date2fy
#' @param yr_ending An integer representing a year.
#' @param fy.yr A string suspected to be a financial year.
#' @param date A string or date for which the financial year is desired. Note that \code{yr2fy} does not check its argument is an integer.
#' @param assume1901_2100 For \code{yr2fy}, assume that \code{yr_ending} is between 1901 and 2100, 
#' for performance. By default, set to \code{getOption("grattan.assume1901_2100", TRUE)}.
#' @details The following forms are permitted: \code{2012-13}, \code{201213}, \code{2012 13}, only.
#' However, the \code{2012-13} form is preferred and will improve performance. 
#' 
#' @return For \code{is.fy}, a logical, whether its argument is a financial year.
#' The following forms are allowed: \code{2012-13}, \code{201213}, \code{2012 13}, only.
#' For \code{fy.year}, \code{yr2fy}, and \code{date2fy}, the financial year. 
#' For the inverses, a numeric corresponding to the year.
#' 
#' \code{fy.year} is a deprecated alias for \code{yr2fy}, the latter is slightly more efficient, as well as more declarative.
#' 
#' \code{fy2yr} converts a financial year to the year ending: \code{fy2yr("2016-17")} returns 2017. \code{yr2fy} is the inverse: \code{yr2fy(fy2yr("2016-17")) == "2016-17"}.
#' 
#' \code{fy2date} converts a financial year to the 30 June of the financial year ending.
#' 
#' \code{date2fy} converts a date to the corresponding financial year.
#' 
#' 
#' @examples
#' is.fy("2012-13")
#' is.fy("2012-14")
#' yr2fy(2012)
#' fy2yr("2015-16")
#' date2fy("2014-08-09")
#' @export is.fy fy.year yr2fy fy2yr fy2date date2fy 
NULL



is.fy <- function(fy.yr){
  out <- logical(length(fy.yr))
  potential_fys <- grepl("^([12][0-9]{3})[-\\s]?[0-9]{2}$", fy.yr, perl = TRUE)
  out[potential_fys] <- 
    {as.integer(sub("^([12][0-9]{3})[-\\s]?[0-9]{2}$", "\\1", fy.yr[potential_fys], perl = TRUE)) + 1L} %% 100L == as.integer(sub("^[12][0-9]{3}[-\\s]?([0-9]{2})$", "\\1", fy.yr[potential_fys], perl = TRUE))
  out
}

is_fy2 <- function(x) {
  a <- c("1897-98", "1898-99", "1899-00", "1900-01", "1901-02", "1902-03", 
         "1903-04", "1904-05", "1905-06", "1906-07", "1907-08", "1908-09", 
         "1909-10", "1910-11", "1911-12", "1912-13", "1913-14", "1914-15", 
         "1915-16", "1916-17", "1917-18", "1918-19", "1919-20", "1920-21", 
         "1921-22", "1922-23", "1923-24", "1924-25", "1925-26", "1926-27", 
         "1927-28", "1928-29", "1929-30", "1930-31", "1931-32", "1932-33", 
         "1933-34", "1934-35", "1935-36", "1936-37", "1937-38", "1938-39", 
         "1939-40", "1940-41", "1941-42", "1942-43", "1943-44", "1944-45", 
         "1945-46", "1946-47", "1947-48", "1948-49", "1949-50", "1950-51", 
         "1951-52", "1952-53", "1953-54", "1954-55", "1955-56", "1956-57", 
         "1957-58", "1958-59", "1959-60", "1960-61", "1961-62", "1962-63", 
         "1963-64", "1964-65", "1965-66", "1966-67", "1967-68", "1968-69", 
         "1969-70", "1970-71", "1971-72", "1972-73", "1973-74", "1974-75", 
         "1975-76", "1976-77", "1977-78", "1978-79", "1979-80", "1980-81", 
         "1981-82", "1982-83", "1983-84", "1984-85", "1985-86", "1986-87", 
         "1987-88", "1988-89", "1989-90", "1990-91", "1991-92", "1992-93", 
         "1993-94", "1994-95", "1995-96", "1996-97", "1997-98", "1998-99", 
         "1999-00", "2000-01", "2001-02", "2002-03", "2003-04", "2004-05", 
         "2005-06", "2006-07", "2007-08", "2008-09", "2009-10", "2010-11", 
         "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", 
         "2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23", 
         "2023-24", "2024-25", "2025-26", "2026-27", "2027-28", "2028-29", 
         "2029-30", "2030-31", "2031-32", "2032-33", "2033-34", "2034-35", 
         "2035-36", "2036-37", "2037-38", "2038-39", "2039-40", "2040-41", 
         "2041-42", "2042-43", "2043-44", "2044-45", "2045-46", "2046-47", 
         "2047-48", "2048-49", "2049-50", "2050-51", "2051-52", "2052-53")
  x %fin% a
}

all_fy <- function(x, permitted = NULL) {
  is.character(x) && length(x) && {
    
    
    a <- if (is.null(permitted)) {
      c("1897-98", "1898-99", "1899-00", "1900-01", "1901-02", "1902-03", 
        "1903-04", "1904-05", "1905-06", "1906-07", "1907-08", "1908-09", 
        "1909-10", "1910-11", "1911-12", "1912-13", "1913-14", "1914-15", 
        "1915-16", "1916-17", "1917-18", "1918-19", "1919-20", "1920-21", 
        "1921-22", "1922-23", "1923-24", "1924-25", "1925-26", "1926-27", 
        "1927-28", "1928-29", "1929-30", "1930-31", "1931-32", "1932-33", 
        "1933-34", "1934-35", "1935-36", "1936-37", "1937-38", "1938-39", 
        "1939-40", "1940-41", "1941-42", "1942-43", "1943-44", "1944-45", 
        "1945-46", "1946-47", "1947-48", "1948-49", "1949-50", "1950-51", 
        "1951-52", "1952-53", "1953-54", "1954-55", "1955-56", "1956-57", 
        "1957-58", "1958-59", "1959-60", "1960-61", "1961-62", "1962-63", 
        "1963-64", "1964-65", "1965-66", "1966-67", "1967-68", "1968-69", 
        "1969-70", "1970-71", "1971-72", "1972-73", "1973-74", "1974-75", 
        "1975-76", "1976-77", "1977-78", "1978-79", "1979-80", "1980-81", 
        "1981-82", "1982-83", "1983-84", "1984-85", "1985-86", "1986-87", 
        "1987-88", "1988-89", "1989-90", "1990-91", "1991-92", "1992-93", 
        "1993-94", "1994-95", "1995-96", "1996-97", "1997-98", "1998-99", 
        "1999-00", "2000-01", "2001-02", "2002-03", "2003-04", "2004-05", 
        "2005-06", "2006-07", "2007-08", "2008-09", "2009-10", "2010-11", 
        "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", 
        "2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23", 
        "2023-24", "2024-25", "2025-26", "2026-27", "2027-28", "2028-29", 
        "2029-30", "2030-31", "2031-32", "2032-33", "2033-34", "2034-35", 
        "2035-36", "2036-37", "2037-38", "2038-39", "2039-40", "2040-41", 
        "2041-42", "2042-43", "2043-44", "2044-45", "2045-46", "2046-47", 
        "2047-48", "2048-49", "2049-50", "2050-51", "2051-52", "2052-53")
    } else {
      permitted
    }
    if (anyNA(fmatch(x, a))) {
      a <- NULL
      FALSE
    } else {
      a <- NULL
      TRUE
    }
  }
}

range_fy2yr <- function(x) {
  if (length(x) == 1L) {
    y <- fmatch(x, fys1901) + 1900L
    return(rep(y, times = 2L))
  }
  if (!is.null(g_min_yr <- attr(x, "grattan_min_yr")) &&
      !is.null(g_max_yr <- attr(x, "grattan_max_yr"))) {
    return(c(g_min_yr, g_max_yr))
  }
  y <- fmatch(x, fys1901) + 1900L
  miny <- min(y, na.rm = TRUE)
  maxy <- max(y, na.rm = TRUE)
  setattr(x, "grattan_min_yr", miny)
  setattr(x, "grattan_max_yr", maxy)
  c(miny, maxy)
}

min_fy2yr <- function(x) {
  range_fy2yr(x)[1L]
}

max_fy2yr <- function(x) {
  range_fy2yr(x)[2L]
}

fy.year <- function(yr_ending){
  paste0(as.integer(yr_ending) - 1, "-", substr(yr_ending, 3, 4))
}

yr2fy <- function(yr_ending, assume1901_2100 = .getOption("grattan.assume1901_2100", TRUE)) {
  if (assume1901_2100 ||
      AND(min(yr_ending) > 1900L, 
          max(yr_ending) < 2100L)) {
    fys1901[yr_ending - 1900L]
  } else {
    .yr2fy(yr_ending)
  }
}

.yr2fy <- function(yr_ending) {
  if (length(yr_ending) > 10e3L) {
    # Apparently quicker for > 1000
    accel_repetitive_input(yr_ending, .yr2fy)
  } else {
    sprintf("%d-%02d", as.integer(yr_ending) - 1L, as.integer(yr_ending) %% 100L)
  }
}

fy2yr <- function(fy.yr){
  if (!all(is.fy(fy.yr))){
    stop("fy.yr contains non-FYs")
  } else  {
    1L + as.integer(gsub("^.*([12][0-9]{3}).?[0-9]{2}.*$", "\\1", fy.yr))
  }
}



fy2date <- function(x){
  if (!all(is.fy(x))){
    stop("fy.yr contains non-FYs")
  } else {
    date <- paste0(as.numeric(gsub("^([1-9][0-9]{3}).*", "\\1", x)) + 1, "-06-30") 
    as.Date(date)
  }
}


date2fy <- function(date) {
  if_else(month(date) < 7L, 
          yr2fy(year(date)), 
          yr2fy(year(date) + 1L))
}

qtr2fy <- function(yq) {
  if (inherits(yq, "yearqtr")) {
    yqn <- as.numeric(yq)
    o <- 
      yr2fy(if_else(yqn %% 1 >= 0.5,
                    yqn + 1, 
                    yqn))
    o
  } else if (is.character(yq)) {
    # Rely on the first element to determine the 
    # format
    first_yq <- yq[1L]
    if (is.na(first_yq)) {
      yq_is_na <- is.na(yq)
      first_yq <- first(yq[which.min(yq_is_na)])
    }
    
    y <- q <- NULL
    cm <- CJ(y = 1901:2099, q = 1:4)
    cm[, "YQ" := sprintf("%d%sQ%d", y, substr(first_yq, 5L, 5L), q)]
    cm[, "fy_year" := yr2fy(y + q %in% 3:4)]
    cmyq <- .subset2(cm, "YQ")
    o <- .subset2(cm, "fy_year")[fmatch(yq, cmyq)]
  } else {
    stop("Unknown class for `yq`.")
  }
  o
}






