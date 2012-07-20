\name{univariate}
\alias{univariate}

\title{
Univariate statistical analysis 
}
\description{
Function to perform univariate statistical analysis with parametric and non parametric testings. A decisional tree algorithm is implemented, assessing whether a variable is normally distributed or not (Shapiro Wilk's test) and performing Welch's T test or Wilcoxon-Mann Whitney U test, depending on normality. 
}
\usage{
univariate(file, imputation = FALSE, imput, normalize = TRUE, multi.test = TRUE, plot.volcano = FALSE, save.boxplot=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
a connection or a character string giving the name of the file containing the variables (matrix columns) to test.
}
  \item{imputation}{
logical, whether to perform imputation of missing values. Default is 'FALSE'.
}
  \item{imput}{
character vector indicating the type of imput that should be used to imput missing values. See details for imput options.
}
  \item{normalize}{
logical, whether to perform normalization. Default is 'TRUE'.
}
  \item{multi.test}{
logical, default is 'TRUE'. Performs correction for multiple testing with Benjamini-Hochberg method.
}
  \item{plot.volcano}{
logical, default is 'FALSE', visualizes volcano plots of each pairwise comparison between groups.
}
\item{save.boxplot}{
logical, default is 'TRUE', plots and saves boxplot.
}

}
\details{
The 'file' provided has to be a matrix in .csv form, formatted with the first column indicating the name of the samples and the second column indicating the class of belonging of each samples (e.g. treatment groups, healthy/diseased, ...). The header of the matrix must contain the name of each variable in the dataset.

A directory called 'Univariate' is automatically generated in the working directory. In this directory are stored all the results from the function 'univariate'.

There for options for imputing missing values: "mean", "minimum", "half.minimum", "zero". For specifying the type of imput to be used, the field 'imputation' must be turned to 'TRUE'.

If 'normalize' is 'TRUE', a normalization is performed on total intensity, i.e. the sum of all variables for each sample is calculated and used as normalizing factor for each variable.

Volcano plots are generated automatically and written in the directory 'Volcano_Plots', within the 'Univariate' directory. If 'plot.volcano' is 'TRUE' such plots are also visualized. Moreover, fold changes and p-values calculated for creating volcano plots are written in the directory 'Fold_Changes' and in the directory 'Pvalues' respectively, for each pairwise comparison between groups. 

The function is 'univariate' is a hub of univariate statistical tests. First, Shapiro Wilk's test for normality is performed for each variable, relying on class definition provided in the second column of the 'file'. If a variable results normally distributed, then Welch's T test is performed; otherwise, if a variable is not normally distributed, Wilcoxon-Mann Whitney U test is performed. For both these tests a threshold of 0.05 is used for assessing significance. After these testings a directory called 'Shapiro_Tests' is created containing the scores of every variable for normality; a directory called 'Welch_Tests' is created containing the p-values for each variable tested, for each pairwise comparison between gropus of samples; a directory called 'Mann-Whitney_Tests' is created containing the p-values for each variable tested, for each pairwise comparison between gropus of samples; a directory called 'Significant_Variables' is created reporting only the variables showing a p-value < 0.05, hence considered significantly different between groups.

Boxplots for each variable, plotted according to class definition provided in the second column of the 'file', are generated and written in the directory called 'BoxPlot', automatically generated in the working directory. 

}

\references{
Goodpaster, AM, et al. Statistical Significance analysis of nuclear magnetic resonance-based metabonomics data. (2010) Anal Biochem. 401:134-143. 
}
\author{
Edoardo Gaude, Dimitrios Spiliotopoulos, Francesca Chignola, Silvia Mari, Andrea Spitaleri and Michela Ghitti
}
\examples{

## The function is currently defined as
function (file, imputation = FALSE, imput, normalize = TRUE, 
    multi.test = TRUE, plot.volcano = FALSE, save.boxplot = TRUE) 
{
    dirout.uni = paste(getwd(), "/Univariate/", sep = "")
    dir.create(dirout.uni)
    comp = read.csv(file, sep = ",", header = TRUE)
    comp.x = comp[, 3:ncol(comp)]
    comp.x = cbind(comp[, 2], comp[, 1], comp.x)
    pwdfile = paste(getwd(), "/Univariate/DataTable.csv", sep = "")
    write.csv(comp.x, pwdfile, row.names = FALSE)
    if (imputation) {
        x <- read.csv(pwdfile, sep = ",", header = TRUE)
        x.x <- x[, 3:ncol(x)]
        rownames(x.x) <- x[, 2]
        y = x.x
        r = is.na(y)
        for (k in 1:ncol(r)) {
            vec = matrix(r[, k], ncol = 1)
            who.miss.rows = which(apply(vec, 1, function(i) {
                any(i)
            }))
            if (length(who.miss.rows) > nrow(y) * 0.8) {
                warning(paste("The variable -", colnames(y)[k], 
                  "- has a number of missing values > 80\%, therefore has been eliminated", 
                  sep = " "))
                y = y[, -k]
            }
        }
        r = is.na(y)
        who.miss.columns = c()
        for (i in 1:nrow(y)) {
            for (j in 1:ncol(y)) {
                if (r[i, j] == TRUE) {
                  if (imput == "mean") {
                    v2 = matrix(r[, j], ncol = 1)
                    who.miss.rows = which(apply(v2, 1, function(i) {
                      any(i)
                    }))
                    rmax = sd(y[-who.miss.rows, j])/1000
                    rmin = -sd(y[-who.miss.rows, j])/1000
                    random = sample(rmin:rmax, 1, replace = TRUE)
                    y[i, j] = mean(y[-who.miss.rows, j]) + random
                    print(paste("Imputing missing value of variable -", 
                      colnames(y)[j], "- for the observation -", 
                      rownames(y)[i], "- with", imput, "value", 
                      sep = " "))
                  }
                  else if (imput == "minimum") {
                    v2 = matrix(r[, j], ncol = 1)
                    who.miss.rows = which(apply(v2, 1, function(i) {
                      any(i)
                    }))
                    rmax = sd(y[-who.miss.rows, j])/1000
                    rmin = -sd(y[-who.miss.rows, j])/1000
                    random = sample(rmin:rmax, 1, replace = TRUE)
                    y[i, j] = min(y[-who.miss.rows, j]) + random
                    print(paste("Imputing missing value of variable -", 
                      colnames(y)[j], "- for the observation -", 
                      rownames(y)[i], "- with", imput, "value", 
                      sep = " "))
                  }
                  else if (imput == "half.minimum") {
                    v2 = matrix(r[, j], ncol = 1)
                    who.miss.rows = which(apply(v2, 1, function(i) {
                      any(i)
                    }))
                    rmax = sd(y[-who.miss.rows, j])/1000
                    rmin = -sd(y[-who.miss.rows, j])/1000
                    random = sample(rmin:rmax, 1, replace = TRUE)
                    y[i, j] = (min(y[-who.miss.rows, j])/2) + 
                      random
                    print(paste("Imputing missing value of variable -", 
                      colnames(y)[j], "- for the observation -", 
                      rownames(y)[i], "- with", imput, "value", 
                      sep = " "))
                  }
                  else if (imput == "zero") {
                    v2 = matrix(r[, j], ncol = 1)
                    who.miss.rows = which(apply(v2, 1, function(i) {
                      any(i)
                    }))
                    rmax = sd(y[-who.miss.rows, j])/1000
                    rmin = -sd(y[-who.miss.rows, j])/1000
                    random = sample(rmin:rmax, 1, replace = TRUE)
                    y[i, j] = 0 + random
                    print(paste("Imputing missing value of variable -", 
                      colnames(y)[j], "- for the observation -", 
                      rownames(y)[i], "- with", imput, "value", 
                      sep = " "))
                  }
                }
            }
        }
        y = cbind(x[, 1], x[, 2], y)
        write.csv(y, pwdfile, row.names = FALSE)
    }
    if (normalize) {
        x <- read.csv(pwdfile, sep = ",", header = TRUE)
        x.x <- x[, 3:ncol(x)]
        x.t <- t(x.x)
        x.s <- matrix(colSums(x.t), nrow = 1)
        uni = matrix(rep(1, nrow(x.t)), ncol = 1)
        area.uni <- uni \%*\% x.s
        x.areanorm <- x.t/area.uni
        x.areanorm = t(x.areanorm)
        x.n = cbind(x[, 1], x.areanorm)
        rownames(x.n) = x[, 2]
        write.csv(x.n, pwdfile)
        comp <- read.csv(pwdfile, sep = ",", header = TRUE)
        comp.x = comp[, 3:ncol(comp)]
        comp.x = cbind(comp[, 2], comp[, 1], comp.x)
        write.csv(comp.x, pwdfile, row.names = FALSE)
    }
    shapiro(file)
    welch(file)
    wmw(file)
    if (multi.test) {
        pvalues(file, mtc = TRUE)
    }
    else {
        pvalues(file, mtc = FALSE)
    }
    col.pvalues(file)
    if (plot.volcano) {
        volcano(file, plot.vol = TRUE)
    }
    else {
        volcano(file, plot.vol = FALSE)
    }
    if (save.boxplot) {
        box.plot(file)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
