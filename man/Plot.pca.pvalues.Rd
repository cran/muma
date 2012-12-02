\name{Plot.pca.pvalues}
\alias{Plot.pca.pvalues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PCA Loading plot color-coded for univariate significance
}
\description{
After the 'univariate' function has been performed, p-values of statistical significance for each variable are generated. These values are used to color the variables in a PCA loading plot according to their significance (p<0.05).
}
\usage{
Plot.pca.pvalues(pcx, pcy, scaling)
}

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
}
\details{
A PCA Loading plot is graphically visualized and written in the working directory. 
}

\author{
Edoardo Gaude, Dimitrios Spiliotopoulos, Francesca Chignola, Silvia Mari, Andrea Spitaleri and Michela Ghitti
}

\examples{

## The function is currently defined as
function (pcx, pcy, scaling) 
{
    loadi = paste(getwd(), "/PCA_Data_", scaling, "/PCA_LoadingsMatrix.csv", 
        sep = "")
    Loading <- read.csv(loadi, sep = ",", header = TRUE)
    Loading.x <- Loading[, 2:ncol(Loading)]
    rownames(Loading.x) <- Loading[, 1]
    load.xlab = paste("Loading PC", pcx)
    load.ylab = paste("Loading PC", pcy)
    lim.load = c()
    Max.pc1 = 1.1 * (max(Loading.x[, pcx]))
    Min.pc1 = 1.1 * (min(Loading.x[, pcy]))
    Mpc1 = c(Min.pc1 * 2, Max.pc1 * 2)
    Max.pc2 = 1.1 * (max(Loading.x[, pcx]))
    Min.pc2 = 1.1 * (min(Loading.x[, pcy]))
    Mpc2 = c(Min.pc2 * 2, Max.pc2 * 2)
    colcool = "Colors_Pvalues"
    pwdcol = paste(getwd(), "/Univariate/Pvalues/", colcool, 
        sep = "")
    col.pv = read.csv(pwdcol, header = TRUE)
    col.pv = matrix(col.pv[, -1], ncol = 1)
    dev.new()
    plot(Loading.x[, pcx], Loading.x[, pcy], col = col.pv, xlab = load.xlab, 
        ylab = load.ylab, xlim = c(Min.pc1, Max.pc1), ylim = c(Min.pc2, 
            Max.pc2), main = "PCA - Loading Plot (Significance-colored variables)", 
        sub = "Variables in red showed Pvalue < 0.05")
    axis(1, at = Mpc1, pos = c(0, 0), labels = FALSE, col = "grey", 
        lwd = 0.7)
    axis(2, at = Mpc2, pos = c(0, 0), labels = FALSE, col = "grey", 
        lwd = 0.7)
    text(Loading.x[, pcx], Loading.x[, pcy], labels = rownames(Loading.x), 
        cex = 0.6, pos = 1)
    E = paste(getwd(), "/PCA_Data_", scaling, "Loading_PC", pcx, 
        "vsPC", pcy, "_Pvalues-colored.pdf", sep = "")
    dev.copy2pdf(file = E)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
