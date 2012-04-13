\name{work.dir}
\alias{work.dir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a working directory
}
\description{
Generic function to create a directory in the current working directory, with copying all the files in the current directory to the newly formed one. When using the whole package 'muma' this function is recommended, as many files and directories are created. 
}
\usage{
work.dir(dir.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir.name}{
A character string indicating the name of the new directory.
}
}
\details{
When using the package 'muma' this function should be used at the beginning, as the newly formed directory became the working directory and all files generated and written will be putted here.
}

\author{
Edoardo Gaude, Dimitrios Spiliotopoulos, Francesca Chignola, Silvia Mari, Andrea Spitaleri and Michela Ghitti
}

\examples{

## The function is currently defined as
function (dir.name) 
{
    WorkinDir = paste(getwd(), "/", dir.name, "/", sep = "")
    dir.create(WorkinDir)
    file = list.files()
    file.copy(file, WorkinDir)
    setwd(WorkinDir)
    unlink("muma.R")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
