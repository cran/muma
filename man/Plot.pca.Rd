\name{Plot.pca}
\alias{Plot.pca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
principal component analysis plotting
}
\description{
Visualize PCA score and loading plots.
}
\usage{
Plot.pca(pcx, pcy, scaling, test.outlier = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pcx}{
an integer indicating the principal component to be plotted in x
}
  \item{pcy}{
an integer indicating the principal component to be plotted in y
}
  \item{scaling}{
a character string indicating the name of the scaling previously specified in the function 'explore.data'
}
  \item{test.outlier}{
logical, indicating whether the geometric outlier testing has to be performed. By default is 'TRUE'.
}
}
\details{
'test.outlier' results in a printed string indicating whether outliers were detected or not; if detected, the samples (rownames) identified as outliers are printed. Principal components to be plotted can be identified through the function 'explore.data'.

A directory called 'PCA-Data' is automatically created in the working directory. Within this directory are written PCA score and loading matrix and every PCA plot generated with the function 'plot.pca'.
}

\author{
Edoardo Gaude, Dimitrios Spiliotopoulos, Francesca Chignola, Silvia Mari, Andrea Spitaleri and Michela Ghitti
}

\examples{

## The function is currently defined as
function (pcx, pcy, scaling, test.outlier = TRUE) 
{
    Plot.pca.score(pcx, pcy, scaling)
    Plot.pca.loading(pcx, pcy, scaling)
    if (test.outlier) {
        outlier(pcx, pcy, scaling)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
