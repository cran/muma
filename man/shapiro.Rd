\name{shapiro}
\alias{shapiro}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Shapiro Wilk's test
}
\description{
This function is implemented in the unique function for univariate statistical analysis 'univariate'. Performs Shapiro Wilk's test for normality on the dataset according to the class definition provided in the second column of the file. For details see 'univariate'.
}
\usage{
shapiro(file, norm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
a connection or a character string giving the name of the file containing the variables (matrix columns) to test.
}
  \item{norm}{
logical, default is 'TRUE'. Performs normalization, prior to statistical tests, of each variable on total sum for each sample.
}
}

\author{
Edoardo Gaude, Dimitrios Spiliotopoulos, Francesca Chignola, Silvia Mari, Andrea Spitaleri and Michela Ghitti
}

\examples{

## The function is currently defined as
function (file, norm) 
{
    comp = read.csv(file, sep = ",", header = TRUE)
    comp.x = comp[, 3:ncol(comp)]
    comp.x = cbind(comp[, 2], comp[, 1], comp.x)
    x <- comp.x
    x.x = x[, 3:ncol(x)]
    rownames(x.x) = x[, 2]
    k = matrix(x[, 1], ncol = 1)
    if (norm) {
        x.t <- t(x.x)
        x.s <- matrix(colSums(x.t), nrow = 1)
        uni = matrix(rep(1, nrow(x.t)), ncol = 1)
        area.uni <- uni \%*\% x.s
        x.areanorm <- x.t/area.uni
        x.areanorm = t(x.areanorm)
        x.n = cbind(k, x.areanorm)
    }
    else {
        x.n = cbind(k, x.x)
    }
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
    dirout.r = paste(getwd(), "/Univariate/Groups", sep = "")
    dir.create(dirout.r)
    dirout.s = paste(getwd(), "/Univariate/Shapiro_Tests", sep = "")
    dir.create(dirout.s)
    for (i in 1:nrow(g)) {
        vuota <- c()
        fin = matrix(rep(NA, ncol(sorted)), nrow = 1)
        for (j in 1:nrow(sorted)) {
            if (sorted[j, 1] == i) {
                vuota <- matrix(sorted[j, ], nrow = 1)
                rownames(vuota) = rownames(sorted)[j]
                fin = rbind(fin, vuota)
            }
        }
        nam = paste("r", i, sep = ".")
        n = matrix(fin[-1, ], ncol = ncol(sorted))
        n.x = matrix(n[, -1], ncol = ncol(sorted) - 1)
        name = as.matrix(assign(nam, n.x))
        shapname = paste("shapiro", i, sep = ".")
        shapiro = matrix(rep(NA, ncol(n)))
        for (q in 1:ncol(name)) {
            notAlist = c()
            notAlist = matrix(unlist(name[, q]))
            shapiro[q, ] = shapiro.test(notAlist)$p.value
            assign(shapname, shapiro)
        }
        outputfile = paste("r.", i, ".out", sep = "")
        write.csv(name, paste(dirout.r, outputfile, sep = "/"))
        outshapiro = paste("ShapiroTest.", i, ".out", sep = "")
        write.csv(shapiro, paste(dirout.s, outshapiro, sep = "/"))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
