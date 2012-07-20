\name{welch}
\alias{welch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Welch's T test
}
\description{
This function is implemented in the unique function for univariate statistical analysis 'univariate'. This function performs Welch's T test of each variable in the data set according to the class definition provided in the second column of the file.  
}
\usage{
welch(file)
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
    dirout.w = paste(getwd(), "/Univariate/Welch_Tests", sep = "")
    dir.create(dirout.w)
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
                we <- matrix(rep(NA, fin))
                for (q in 1:fin) {
                  we[q, ] <- t.test(I[, q], J[, q], var.equal = F, 
                    conf.level = 0.95, alternative = "two.sided")$p.value
                }
                welch.ij = paste("WelchTest_", i, "vs", j, ".out", 
                  sep = "")
                assign(welch.ij, we)
                write.csv(we, paste(dirout.w, welch.ij, sep = "/"))
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
