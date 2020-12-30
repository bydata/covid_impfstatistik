# devtools::install_github("bnosac/taskscheduleR")
library(taskscheduleR)

myscript <- file.path(getwd(), "download_rki.R")
save_to <- file.path(Sys.getenv("HOME"), "rki")
save_to <- stringr::str_replace_all(save_to, "/", "\\\\")


# ## run script once 
# taskname <- "download_rki_daily_test"
# taskscheduler_delete(taskname)
# taskscheduler_create(taskname = "download_rki_daily_test", rscript = myscript,
#                      rscript_args = glue::glue("wd={save_to}"),
#                      schedule = "ONCE", starttime = format(Sys.time() + 61, "%H:%M:%S"),
#                      debug = TRUE)


## set up daily schedule

# 10 am
taskname <- "download_rki_daily_10am"
taskscheduler_delete(taskname)
taskscheduler_create(taskname = "download_rki_daily_10am", rscript = myscript, 
                     rscript_args = glue::glue("wd={save_to}"),
                     schedule = "DAILY", starttime = "10:00", 
                     startdate = format(Sys.Date() + 1, "%d/%m/%Y"),
                     debug = TRUE)

# 12 pm
taskname <- "download_rki_daily_12pm"
taskscheduler_delete(taskname)
taskscheduler_create(taskname = "download_rki_daily_12pm", rscript = myscript, 
                     rscript_args = glue::glue("wd={save_to}"),
                     schedule = "DAILY", starttime = "12:00", 
                     startdate = format(Sys.Date() + 1, "%d/%m/%Y"),
                     debug = TRUE)
