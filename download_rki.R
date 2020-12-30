# download latest report

print_fmt <- function(msg) print(glue::glue("--- {msg} ---"))

args <- commandArgs(trailingOnly = TRUE)
arg_split <- stringr::str_split_fixed(args[1], pattern = "=", 2)

if (arg_split[1] == "wd") {
  wd <- arg_split[2]
  print(wd)
} else {
  stop("Please set a working directory with `wd`={path}")
}


# Source: https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Impfquoten-Tab.html
url <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Impfquotenmonitoring.xlsx;jsessionid=?__blob=publicationFile"

print_fmt(glue::glue("Downloading file from {url}"))

rki_tab_path <- file.path(wd, glue::glue("Impfquotenmonitoring_{Sys.Date()}.xlsx"))

print_fmt(glue::glue("Saving file to {rki_tab_path}"))
download.file(url, destfile = rki_tab_path, mode = "wb")

