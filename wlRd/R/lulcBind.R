lulcBind <- function(file.path){
  # get list of files in path specified by user
  path.files <- list.files(file.path)
  # read in all the files, assign a new column that stamps on the file name, and bind altogether
  do.call("rbind", lapply(paste(file.path, path.files, sep = ""), function(x) {
    dat <- read.csv(x, header = T)
    dat$filename <- tools::file_path_sans_ext(basename(x))
    dat
    }))
  }
