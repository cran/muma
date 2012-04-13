\name{ostocsy}
\alias{ostocsy}
\title{
OSC Statistical TOtal Correlation SpectroscopY
}
\description{
Performs the stocsy analysis on the matrix deflated through the 'oplsda' function
}
\usage{
ostocsy(threshold = TRUE, pos.threshold, neg.threshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{threshold}{
logical, indicating whether positive and negative threshold must be specified. By default is 'TRUE'.
}
  \item{pos.threshold}{
The positive threshold for plotting positive correlations.
}
  \item{neg.threshold}{
The negative threshold for plotting negative correlations.
}
}
\details{
OSTOCSY correlation matrix is graphically visualized, with correlations ranging from -1 to 1 and color coded for negative correlations (blue), positive correlations (red) and no correlation (white). If 'threshold' is 'TRUE' an additional plot is generated with correlation values >= and =< to the specified positive and negative thresholds, respectively. All plots visualized are written in the directory 'OSTOCSY', together with the deflated (OSC-filtered) matrix. 
}

\references{
Blaise, B.J. et al. Orthogonal filtered recoupled-STOCSY to extract metabolic networkd associated with minor perturbations from NMR spectroscopy. (2011) J Proteome Res. 10(9):4342-8.
}
\author{
Edoardo Gaude, Dimitrios Spiliotopoulos, Francesca Chignola, Silvia Mari, Andrea Spitaleri and Michela Ghitti
}



\examples{

## The function is currently defined as
function (threshold = TRUE, pos.threshold, neg.threshold) 
{
    pwd.n = paste(getwd(), "/OPLS-DA/X_deflated.csv", sep = "")
    x <- read.csv(pwd.n, sep = ",", header = TRUE)
    x.x <- x[, 2:ncol(x)]
    rownames(x.x) <- x[, 1]
    x.t <- x.x
    mycor = cor(x.t, method = c("pearson"))
    library(gplots)
    col = colorpanel(50, "blue", "white", "red")
    image(mycor, axes = FALSE, col = col, main = "OSTOCSY")
    axis(side = 1, labels = colnames(mycor), at = seq(0, 1, length = length(colnames(mycor))), 
        las = 2, cex.axis = 0.4)
    axis(side = 2, labels = colnames(mycor), at = seq(0, 1, length = length(colnames(mycor))), 
        las = 2, cex.axis = 0.4)
    dirout = paste(getwd(), "/OPLS-DA/OSTOCSY/", sep = "")
    dir.create(dirout)
    o = paste(dirout, "OSTOCSY.pdf", sep = "")
    dev.copy2pdf(file = o)
    o.cor = paste(dirout, "CorrelationMatrix.csv", sep = "")
    write.csv(mycor, file = o.cor)
    if (threshold) {
        dev.new()
        image(mycor, axes = FALSE, zlim = c(pos.threshold, 1), 
            col = "red", main = paste("OSTOCSY <", neg.threshold, 
                " & >", pos.threshold, sep = ""))
        image(mycor, axes = FALSE, zlim = c(-1, neg.threshold), 
            col = "navy", add = TRUE)
        axis(side = 1, labels = colnames(mycor), at = seq(0, 
            1, length = length(colnames(mycor))), las = 2, cex.axis = 0.4)
        axis(side = 2, labels = colnames(mycor), at = seq(0, 
            1, length = length(colnames(mycor))), las = 2, cex.axis = 0.4)
        out = paste(dirout, "STOCSY_", pos.threshold, "_", neg.threshold, 
            ".pdf", sep = "")
        dev.copy2pdf(file = out)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
