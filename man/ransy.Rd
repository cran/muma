\name{ransy}
\alias{ransy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ratio Analysis NMR SpectroscopY
}
\description{
Starting from a specified variable (matrix column) called 'driver', it calculates the ratio between this variable and all the other variables in the dataset, returning a barplot indicating the ratio values calculated between the driver and the other variables. This function may help with NMR molecular identification and assignment. 
}
\usage{
ransy(scaling, driver.peak)
}

\arguments{
  \item{scaling}{
a character string indicating the name of the scaling previously used with the function 'explore.data'
}
  \item{driver.peak}{
An integer indicating the column number of the dataset to use as 'driver peak'.
}
}
\details{
'driver.peak' can be chosen through the function 'chose.driver' (see ?chose.driver), which lists all the variables (column names) present in the dataset.

A barplot is generated color-coded for the ratio values calculated between the driver and the other variables. This plot is written within the directory 'RANSY', in the working directory. 
}

\references{
Wei, S. et al. Ratio Analysis Nuclear Magnetic Resonance Spectroscopy for Selective Metabolite Identification in Complex Samples. (2011) Anal Chem 83(20):7616-7623.
}
\author{
Edoardo Gaude, Dimitrios Spiliotopoulos, Francesca Chignola, Silvia Mari, Andrea Spitaleri and Michela Ghitti
}

\seealso{
chose.driver, stocsy.1d
}
\examples{

## The function is currently defined as
function (scaling, driver.peak) 
{
    pwd.n = paste(getwd(), "/Preprocessing_Data_", scaling, "/ProcessedTable.csv", 
        sep = "")
    x <- read.csv(pwd.n, sep = ",", header = TRUE)
    x.x <- x[, 2:ncol(x)]
    rownames(x.x) <- x[, 1]
    x.t <- x.x
    one = matrix(rep(1, ncol(x.t)), nrow = 1)
    driver = x.t[, driver.peak] \%*\% one
    D = x.t/driver
    m = matrix(colMeans(D), nrow = 1)
    sd = matrix(apply(D, 2, sd), nrow = 1)
    R = m/sd
    R[, driver.peak] = 1
    R[, driver.peak] = max(R)
    Rt = t(R)
    library(gplots)
    plot(Rt, type = "h", main = paste("RANSY (", rownames(x.x)[driver.peak], 
        ")", sep = ""), ylab = paste("Mean/sd of ratio with ", 
        rownames(x.x)[driver.peak], sep = ""), xlab = "Variables")
    text(Rt, labels = colnames(x.x), cex = 0.6)
    dirout = paste(getwd(), "/RANSY/", sep = "")
    dir.create(dirout)
    out = paste(dirout, "ransy_", colnames(x.x)[driver.peak], 
        ".pdf", sep = "")
    dev.copy2pdf(file = out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
