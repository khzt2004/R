# https://cloudyr.github.io/googleComputeEngineR/articles/scheduled-rscripts.html#simple-scheduler
# https://cran.r-project.org/web/packages/googleComputeEngineR/vignettes/installation-and-authentication.html

  library(containeRit)


script <- system.file("schedulescripts", "schedule.R", package = "googleComputeEngineR")

## put the "schedule.R" script in the working directory
file.copy(script, getwd())


## it will run the script whilst making the dockerfile
container <- dockerfile("schedule.R",
                        copy = "script_dir",
                        cmd = CMD_Rscript("schedule.R"),
                        soft = TRUE)
write(container, file = "Dockerfile")