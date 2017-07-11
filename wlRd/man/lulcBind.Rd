\name{lulcBind}
\alias{lulcBind}
\title{A function to compile multiple land use land cover csv files into an R data frame object}
\usage{
  pssbBind(file.path, score.type = NULL, ambient = F)
}
\arguments{
  \item{file.path}{The path to the folder directory containing your text files. Can be a vector of multiple paths}

  \item{score.type}{Adds a column to the dataframe idneitfying the score type found in each path. Primarily used when comparing things like coars vs. fine resolution, or 10-50 vs. 0-100 scale. Can be a vector of multiple score types but order must correspond to paths specified in file.path argument}
}
\description{
  This function allows you to merge together multiple text files downloaded from PSSB.
}
\examples{
  pssbBind
}
\keyword{PSSB,}
\keyword{files,}
\keyword{resolution,}
\keyword{score}
\keyword{text}
\keyword{type}
