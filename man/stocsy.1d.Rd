\name{stocsy.1d}
\alias{stocsy.1d}

\title{
Statistical TOtal Correlation SpectroscopY - monodimensional
}
\description{
Calculate correlations of a specified variable with all the other variables in a dataset and generate the corresponding barplot indicating each correlation value.
}
\usage{
stocsy.1d(scaling, driver.peak)
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
A barplot color-coded for positive (red), negative (blue) and null (green) correlations is generated and written in the directory 'STOCSY_1D'.
}

\references{
Cloarec, O et al. Statistical total correlation spectroscopy: an exploratory approach for latent biomarker identification from metabolic 1H NMR data sets. (2005) Anal Chem. 77(5):1282-9.
}
\author{
Edoardo Gaude, Dimitrios Spiliotopoulos, Francesca Chignola, Silvia Mari, Andrea Spitaleri and Michela Ghitti
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
    mycor = cor(x.t, method = c("pearson"))
    library(gplots)
    pal = matrix(rich.colors(41), nrow = 1)
    sec = matrix(seq(-1, 1, 0.05), nrow = 1)
    d = matrix(mycor[, driver.peak], ncol = 1)
    sec40 = ncol(sec) - 1
    col = c()
    for (i in 1:nrow(d)) {
        for (j in 1:sec40) {
            if (sec[, j] <= d[i, ] & d[i, ] <= sec[, j + 1]) {
                col = matrix(c(col, pal[, j]), ncol = 1)
            }
        }
    }
    plot(mycor[, driver.peak], type = "h", col = col, xlab = "Variables", 
        ylab = paste("Coefficient of correlation with ", rownames(x.x)[driver.peak], 
            sep = ""), main = paste("STOCSY 1D (", rownames(x.x)[driver.peak], 
            ")", sep = ""), ylim = c(-1, 1))
    text(mycor[, driver.peak], labels = colnames(x.x), cex = 0.5, 
        col = col)
    dirout = paste(getwd(), "/STOCSY_1D/", sep = "")
    dir.create(dirout)
    out = paste(dirout, "stocsy_1d_", colnames(x.x)[driver.peak], 
        ".pdf", sep = "")
    dev.copy2pdf(file = out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
