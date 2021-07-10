wwget64 <- Sys.which("wget64")
if (!nzchar(wwget64)) {
  wwget64 <- r'{C:\Program Files\Git\mingw64\bin\wget64.exe}'
}

shell(paste(shQuote(wwget64), "-r --no-parent --page-requisites --convert-links http://guides.dss.gov.au"))
