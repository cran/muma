\name{col.pvalues}
\alias{col.pvalues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Color-coding based on significance
}
\description{
This function is implemented in the unique function for univariate statistical analysis 'univariate'. This function generates a vector of characters specifying a color-code based on the significance (p-values < 0.05) of each variable.
}
\usage{
col.pvalues(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
a connection or a character string giving the name of the file containing the variables (matrix columns) to test.
}
}
\details{
For details see the function 'univariate'.
}
\references{
Goodpaster, AM, et al. Statistical Significance analysis of nuclear magnetic resonance-based metabonomics data. (2010) Anal Biochem. 401:134-143.
}
\author{
Edoardo Gaude, Dimitrios Spiliotopoulos, Francesca Chignola, Silvia Mari, Andrea Spitaleri and Michela Ghitti
}
\examples{

## The function is currently defined as
function (file) 
{
    pwdfile = paste(getwd(), "/Univariate/DataTable.csv", sep = "")
    file = pwdfile
    x <- read.csv(file, sep = ",", header = TRUE)
    x.x = x[, 3:ncol(x)]
    rownames(x.x) = x[, 2]
    k = matrix(x[, 1], ncol = 1)
    x.n = cbind(k, x.x)
    sorted = x.n[order(x.n[, 1]), ]
    g = c()
    for (i in 1:nrow(sorted)) {
        if (any(g == sorted[i, 1])) {
            g = g
        }
        else {
            g = matrix(c(g, sorted[i, 1]), ncol = 1)
        }
    }
    NoF = nrow(g)
    all.pvalues = matrix(rep(1, ncol(sorted) - 1), ncol = 1)
    dirout.col = paste(getwd(), "/Univariate/Pvalues/", sep = "")
    fin = ncol(sorted) - 1
    for (i in 1:NoF) {
        for (j in 1:NoF) {
            if (i < j) {
                ni = paste("Pvalues_", i, "vs", j, ".csv", sep = "")
                pwdi = paste(getwd(), "/Univariate/Pvalues/", 
                  ni, sep = "")
                I = read.csv(pwdi, header = TRUE)
                I = matrix(I[, -1])
                for (q in 1:fin) {
                  if (I[q, ] < 0.05 & all.pvalues[q, ] == 1) {
                    all.pvalues[q, ] = I[q, ]
                  }
                  else {
                    all.pvalues[q, ] = all.pvalues[q, ]
                  }
                }
            }
        }
    }
    colp = matrix(rep(NA, ncol(sorted) - 1), ncol = 1)
    for (i in 1:fin) {
        if (all.pvalues[i, ] < 1) {
            colp[i, ] = "red"
        }
        else {
            colp[i, ] = "black"
        }
        colnam = "Colors_Pvalues"
        assign(colnam, colp)
        write.csv(colp, paste(dirout.col, colnam, sep = ""))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
