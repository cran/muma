\name{wmw}
\alias{wmw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wilcoxon-Mann Whitney U test
}
\description{
This function is implemented in the unique function for univariate statistical analysis 'univariate'. This function performs Wilcoxon-mann Whitney U test of each variable in the data set according to the class definition provided in the second column of the file.
}
\usage{
wmw(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
a connection or a character string giving the name of the file containing the variables (matrix columns) to test.
}
}
\details{
For details see ?univariate.
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
    dirout.wm = paste(getwd(), "/Univariate/Mann-Whitney_Tests/", 
        sep = "")
    dir.create(dirout.wm)
    for (i in 1:NoF) {
        for (j in 1:NoF) {
            if (i < j) {
                ni = paste("r.", i, ".out", sep = "")
                nj = paste("r.", j, ".out", sep = "")
                pwdi = paste(getwd(), "/Univariate/Groups/", 
                  ni, sep = "")
                pwdj = paste(getwd(), "/Univariate/Groups/", 
                  nj, sep = "")
                I = read.csv(pwdi, header = TRUE)
                J = read.csv(pwdj, header = TRUE)
                I = I[, -1]
                J = J[, -1]
                fin = ncol(sorted) - 1
                wilx.pv <- matrix(rep(NA, fin))
                for (q in 1:fin) {
                  wilx.pv[q, ] <- wilcox.test(I[, q], J[, q], 
                    paired = FALSE, exact = NULL, correct = FALSE, 
                    conf.level = 0.95, alternative = "two.sided")$p.value
                }
                wmw.ij.pv = paste("WMWTest_pvalues_", i, "vs", 
                  j, ".out", sep = "")
                assign(wmw.ij.pv, wilx.pv)
                write.csv(wilx.pv, paste(dirout.wm, wmw.ij.pv, 
                  sep = ""))
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
