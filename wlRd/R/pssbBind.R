pssbBind <- function(file.path, score.type = NULL, ambient = F) {
  # validate lengths of file path vector and score type vector are the same
  if (!is.null(score.type)) {
  if (length(file.path) != length(score.type)) stop("The number of score types must be equal to the number of file paths")}
  # loop through file paths, define anonymous function
  all.list <- lapply(1:length(file.path), function(x) {
    path.files <- list.files(file.path[x])
    # read in files
    list.with.each.file <- lapply(paste(file.path, list.files(file.path), sep = ''), function(y) read.delim(y, header=TRUE))
    # append id to each file individually
    if (!is.null(score.type)) {
      list.with.each.file <- lapply(list.with.each.file, function(y) {
        y$id <- score.type[x]
        y})}

    # bind all files from each file.path into data frames
    do.call("rbind.data.frame", list.with.each.file)
  })
  # bind files from all file.paths into one data frame
  bibi <- do.call("rbind.data.frame", all.list)

  if (ambient == TRUE) {bibi <- droplevels(bibi[bibi$Agency=="King County - DNRP" & bibi$Project == 'Boise Ambient' | bibi$Project == 'Ambient Monitoring' | bibi$Project =='Vashon' | bibi$Project =='Seattle',])}

  return(bibi)
}
