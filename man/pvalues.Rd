\name{pvalues}
\alias{pvalues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
P-values reporting
}
\description{
This function is implemented in the unique function for univariate statistical analysis 'univariate'. This function reports final p-values foe each pairwise caomparison of sample groups, by merging the p-values from Welch's T test for normally distributed variables with p-values from Wilcoxon-Mann Whitney U test for not-normally distributed variables. 
}
\usage{
pvalues(file, mtc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
a connection or a character string giving the name of the file containing the variables
}
  \item{mtc}{
logical, default is 'TRUE'. Perform multiple testing correction with Banjamini-Hochberg method. 
}
}
\details{
A directory called 'Pvalues' is automatically generated in the directory 'Univariate' in the working directory. A report with merged p-values from each test is written.
}
\author{
Edoardo Gaude, Dimitrios Spiliotopoulos, Francesca Chignola, Silvia Mari, Andrea Spitaleri and Michela Ghitti
}
\examples{

## The function is currently defined as
function (file, mtc) 
{
    pwdfile = paste(getwd(), "/Univariate/DataTable.csv", sep = "")
    file = pwdfile
    x <- read.csv(file, sep = ",", header = TRUE)
    x.x = x[, 3:ncol(x)]
    rownames(x.x) = x[, 2]
    k = matrix(x[, 1], ncol = 1)
    x.n = cbind(k, x.x)
    sorted = x.n[order(x.n[, 1]), ]
    sorted.x = sorted[, -1]
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
    dirout.pv = paste(getwd(), "/Univariate/Pvalues/", sep = "")
    dir.create(dirout.pv)
    dirout.sign = paste(getwd(), "/Univariate/Significant_Variables/", 
        sep = "")
    dir.create(dirout.sign)
    for (i in 1:NoF) {
        for (j in 1:NoF) {
            if (i < j) {
                shapi = paste("ShapiroTest.", i, ".csv", sep = "")
                shapj = paste("ShapiroTest.", j, ".csv", sep = "")
                shap.pwdi = paste(getwd(), "/Univariate/Shapiro_Tests/", 
                  shapi, sep = "")
                shap.pwdj = paste(getwd(), "/Univariate/Shapiro_Tests/", 
                  shapj, sep = "")
                Si = read.csv(shap.pwdi, header = TRUE)
                Sj = read.csv(shap.pwdj, header = TRUE)
                Si = matrix(Si[, -1], ncol = 1)
                Sj = matrix(Sj[, -1], ncol = 1)
                welchij = paste("WelchTest_", i, "vs", j, ".csv", 
                  sep = "")
                welch.pwdij = paste(getwd(), "/Univariate/WelchTest/", 
                  welchij, sep = "")
                Wij = read.csv(welch.pwdij, header = TRUE)
                Wij = matrix(Wij[, -1], ncol = 1)
                wmwp = paste("WMWTest_pvalues_", i, "vs", j, 
                  ".csv", sep = "")
                wmw.pwd = paste(getwd(), "/Univariate/Mann-Whitney_Tests/", 
                  wmwp, sep = "")
                WMWp = read.csv(wmw.pwd, header = TRUE)
                WMWp = matrix(WMWp[, -1], ncol = 1)
                fin = ncol(sorted) - 1
                pvalues = matrix(rep(1, fin))
                for (q in 1:fin) {
                  if (Si[q, ] > 0.05 & Sj[q, ] > 0.05) {
                    pvalues[q, ] <- Wij[q, ]
                  }
                  else {
                    pvalues[q, ] <- WMWp[q, ]
                  }
                }
                if (mtc) {
                  pval.corr = matrix(p.adjust(pvalues, method = c("BH"), 
                    n = nrow(pvalues)), ncol = 1)
                  rownames(pval.corr) = colnames(x.x)
                  pvalfin = paste("Pvalues_", i, "vs", j, ".csv", 
                    sep = "")
                  assign(pvalfin, pval.corr)
                  write.csv(pval.corr, paste(dirout.pv, pvalfin, 
                    sep = ""))
                  sign = c()
                  for (q in 1:fin) {
                    if (pval.corr[q, ] < 0.05) {
                      sign = matrix(c(sign, colnames(sorted.x)[q]))
                    }
                  }
                  signnam = paste("Significant_Variables_", i, 
                    "vs", j, ".csv", sep = "")
                  assign(signnam, sign)
                  write.csv(sign, paste(dirout.sign, signnam, 
                    sep = ""), row.names = FALSE)
                }
                else {
                  rownames(pvalues) = colnames(x.x)
                  pvalfin = paste("Pvalues_", i, "vs", j, ".csv", 
                    sep = "")
                  assign(pvalfin, pvalues)
                  write.csv(pvalues, paste(dirout.pv, pvalfin, 
                    sep = ""))
                  sign = c()
                  for (q in 1:fin) {
                    if (pvalues[q, ] < 0.05) {
                      sign = matrix(c(sign, colnames(sorted.x)[q]))
                    }
                  }
                  signnam = paste("Significant_Variables_", i, 
                    "vs", j, ".csv", sep = "")
                  assign(signnam, sign)
                  write.csv(sign, paste(dirout.sign, signnam, 
                    sep = ""), row.names = FALSE)
                }
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
