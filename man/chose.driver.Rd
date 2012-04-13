\name{chose.driver}
\alias{chose.driver}
\title{
List variables in the dataset
}
\description{
List the variables (column names) of the dataset provided, in order to select the variable against which ransy and/or stocsy1d have to be performed.
}
\usage{
chose.driver(scaling)
}
\arguments{
  \item{scaling}{
a character indicating the name of the scaling used within the function 'explore.data' (see Details).
}
}
\details{
The type of scaling to be used has to be indicated within the function 'explore.data' and, when re-calling the scaling the same name has to be used. The function 'chose.driver' can be used for listing the names of the variables (column names of the matrix/file provided) and for selecting the variable against which perform stocsy and /or ransy. For details see the functions 'ransy' and 'stocsy.1d'.
}
\author{
Edoardo Gaude, Dimitrios Spiliotopoulos, Francesca Chignola, Silvia Mari, Andrea Spitaleri and Michela Ghitti
}

\examples{

## The function is currently defined as
function (scaling) 
{
    pwd.n = paste(getwd(), "/Preprocessing_Data_", scaling, "/NormalizedTable.csv", 
        sep = "")
    x <- read.csv(pwd.n, sep = ",", header = TRUE)
    print(x[, 1])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
