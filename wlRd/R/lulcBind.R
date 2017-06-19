lulcBind <- function(file.path, score.type = NULL, ambient = F) {

# loop through file paths, define anonymous function
all.list <- lapply(1:length(file.path), function(x) {

path.files <- list.files(file.path[x])

# read in files
list.with.each.file <- lapply(paste(file.path, list.files(file.path), sep = ''), function(y) read.csv(y, header=TRUE))

# bind all files from each file.path into data frames
do.call("rbind.data.frame", list.with.each.file)
})

# bind files from all file.paths into one data frame
lulc <- do.call("rbind.data.frame", all.list)

return(lulc)
}