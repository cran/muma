\name{stocsy}
\alias{stocsy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Statistical TOtal Correlation SpectroscopY
}
\description{
Function to calculate correlation matrix and plot corresponding color-coded heatmap, useful for NMR molecular identification and assignment. 
}
\usage{
stocsy(scaling, threshold = FALSE, pos.threshold, neg.threshold)
}

\arguments{
  \item{scaling}{
a character string indicating the name of the scaling previously used with the function 'explore.data'.
}
  \item{threshold}{
logical. The possibility to indicate or not a positive and negative threshold regulating the heatmap coloring. 
}
  \item{pos.threshold}{
a decimal (or integer) indicating the upper level beyond which correlation values are visualized. 
}
  \item{neg.threshold}{
a decimal (or integer) indicating the lower level below which correlation values are visualized.
}
}
\details{
'threshold' is 'FALSE' by default. If 'TRUE' positive and negative threshold values must be specified. In this case two plots are created, one with no color thresholds and one with the indicated thresholds; both these heatmaps are written in the directory 'STOCSY', automatically created in the working directory.
}

\references{
Cloarec, O et al. Statistical total correlation spectroscopy: an exploratory approach for latent biomarker identification from metabolic 1H NMR data sets. (2005) Anal Chem. 77(5):1282-9.
}
\author{
Edoardo Gaude, Dimitrios Spiliotopoulos, Francesca Chignola, Silvia Mari, Andrea Spitaleri and Michela Ghitti
}

\examples{

## The function is currently defined as
function (scaling, threshold = FALSE, pos.threshold, neg.threshold) 
{
    pwd.n = paste(getwd(), "/Preprocessing_Data_", scaling, "/NormalizedTable.csv", 
        sep = "")
    x <- read.csv(pwd.n, sep = ",", header = TRUE)
    x.x <- x[, 2:ncol(x)]
    rownames(x.x) <- x[, 1]
    x.t <- t(x.x)
    mycor = cor(x.t, method = c("pearson"))
    library(gplots)
    col = colorpanel(50, "blue", "white", "red")
    image(mycor, axes = FALSE, col = col, main = "STOCSY")
    axis(side = 1, labels = colnames(mycor), at = seq(0, 1, length = length(colnames(mycor))), 
        las = 2, cex.axis = 0.7)
    axis(side = 2, labels = colnames(mycor), at = seq(0, 1, length = length(colnames(mycor))), 
        las = 2, cex.axis = 0.7)
    dirout = paste(getwd(), "/STOCSY/", sep = "")
    dir.create(dirout)
    o = paste(dirout, "STOCSY.pdf", sep = "")
    dev.copy2pdf(file = o)
    o.cor = paste(dirout, "CorrelationMatrix.csv", sep = "")
    write.csv(mycor, file = o.cor)
    if (threshold) {
        dev.new()
        image(mycor, axes = FALSE, zlim = c(pos.threshold, 1), 
            col = "red", main = paste("STOCSY <", neg.threshold, 
                " & >", pos.threshold, sep = ""))
        image(mycor, axes = FALSE, zlim = c(-1, neg.threshold), 
            col = "navy", add = TRUE)
        axis(side = 1, labels = colnames(mycor), at = seq(0, 
            1, length = length(colnames(mycor))), las = 2, cex.axis = 0.7)
        axis(side = 2, labels = colnames(mycor), at = seq(0, 
            1, length = length(colnames(mycor))), las = 2, cex.axis = 0.7)
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
