\name{plsda}
\alias{plsda}

\title{
Partial Least Square Discriminant Analysis
}
\description{
Perform PLS-DA according to the class subgroups.
}
\usage{
plsda(scaling)
}

\arguments{
  \item{scaling}{
a character string indicating the name of the scaling previously specified in the function 'explore.data'
}
}
\details{
The number of components to be calculated are defined as the number of classes - 1. A plot reporting pairwise representation of the components is graphically visualized and written in the directory 'PLS-DA', together with the PLS score and loading matrices.
}

\author{
Edoardo Gaude, Dimitrios Spiliotopoulos, Francesca Chignola, Silvia Mari, Andrea Spitaleri and Michela Ghitti
}

\examples{

## The function is currently defined as
function (scaling) 
{
    pwd.x = paste(getwd(), "/Preprocessing_Data_", scaling, "/ScaledTable_", 
        scaling, ".csv", sep = "")
    x = read.csv(pwd.x, header = TRUE)
    x.x = x[, 2:ncol(x)]
    rownames(x.x) = x[, 1]
    pwdK = paste(getwd(), "/Preprocessing_Data_", scaling, "/class.csv", 
        sep = "")
    k = read.csv(pwdK, header = TRUE)
    k.x = matrix(k[, -1], ncol = 1)
    x.n = cbind(k.x, x.x)
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
    dimB = nrow(g) * nrow(sorted)
    B = matrix(rep(NA, dimB), ncol = nrow(g))
    for (i in 1:nrow(sorted)) {
        for (j in 1:nrow(g)) {
            if (sorted[i, 1] == j) {
                B[i, j] = 1
            }
            else {
                B[i, j] = 0
            }
        }
    }
    library(pls)
    sorted.x = sorted[, -1]
    sorted.un = matrix(unlist(sorted.x), ncol = ncol(sorted.x))
    P = plsr(B ~ sorted.un, ncomp = nrow(g) - 1, method = c("kernelpls"), 
        validation = "CV")
    rownames(P$scores) = rownames(sorted.x)
    rownames(P$loadings) = colnames(sorted.x)
    dirout = paste(getwd(), "/PLS-DA/", sep = "")
    dir.create(dirout)
    out.score = paste(dirout, "PLSDA_Scores_", scaling, ".out", 
        sep = "")
    write.csv(P$scores, out.score)
    out.load = paste(dirout, "PLSDA_Loadings_", scaling, ".out", 
        sep = "")
    write.csv(P$loadings, out.load)
    k = matrix(sorted[, 1], ncol = 1)
    tutticolors = matrix(c(1, 2, 3, 4, 5, 6, 7, 8, "rosybrown4", 
        "green4", "navy", "purple2", "orange", "pink", "chocolate2", 
        "coral3", "khaki3", "thistle", "turquoise3", "palegreen1", 
        "moccasin", "olivedrab3", "azure4", "gold3", "deeppink"), 
        ncol = 1)
    col = c()
    for (i in 1:nrow(k)) {
        col = c(col, tutticolors[k[i, ], ])
    }
    pairs = c()
    if (ncol(P$scores) >= 10) {
        pairs = c(10)
    }
    else {
        pairs = c(ncol(P$scores))
    }
    pairs(P$scores[, 1:pairs], col = col)
    pairs = paste(dirout, "Pairs_PLSDA_", scaling, ".pdf", sep = "")
    dev.copy2pdf(file = pairs)
    p.v = matrix(P$Xvar, ncol = 1)
    p.v.out = paste(dirout, "PLSDA_P_", scaling, ".out", sep = "")
    write.csv(p.v, file = p.v.out)
    p.vtot = matrix(P$Xtotvar, ncol = 1)
    p.vtot.out = paste(dirout, "PLSDA_Ptot_", scaling, ".out", 
        sep = "")
    write.csv(p.vtot, file = p.vtot.out, row.names = FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
