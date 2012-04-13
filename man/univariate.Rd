\name{univariate}
\alias{univariate}

\title{
Univariate statistical analysis 
}
\description{
Function to perform univariate statistical analysis with parametric and non parametric testings. A decisional tree algorithm is implemented, assessing whether a variable is normally distributed or not (Shapiro Wilk's test) and performing Welch's T test or Wilcoxon-Mann Whitney U test, depending on normality. 
}
\usage{
univariate(file, plot.volcano = TRUE, multi.test = TRUE, normalize = TRUE, boxplot = TRUE)
}

\arguments{
  \item{file}{
a connection or a character string giving the name of the file containing the variables (matrix columns) to test.
}
  \item{plot.volcano}{
logical, default is 'TRUE', visualizes volcano plots of each pairwise comparison between groups.
}
  \item{multi.test}{
logical, default is 'TRUE'. Performs correction for multiple testing with Benjamini-Hochberg method. 
}
  \item{normalize}{
logical, default is 'TRUE'. Performs normalization, prior to statistical tests, of each variable on total sum for each sample. 
}
  \item{boxplot}{
logical, default is 'TRUE'. Generates boxplot of each variable in the dataset.
}
}
\details{
The 'file' provided has to be a matrix in .csv form, formatted with the first column indicating the name of the samples and the second column indicating the class of belonging of each samples (e.g. treatment groups, healthy/diseased, ...). The header of the matrix must contain the name of each variable in the dataset.

A directory called 'Univariate' is automatically generated in the working directory. In this directory are stored all the results from the function 'univariate'.

If 'normalize' is 'TRUE', a normalization is performed on total intensity, i.e. the sum of all variables for each sample is calculated and used as normalizing factor for each variable.

Volcano plots are generated automatically and written in the directory 'Volcano_Plots', within the 'Univariate' directory. If 'plot.volcano' is 'TRUE' such plots are also visualized. Moreover, fold changes and p-values calculated for creating volcano plots are written in the directory 'Fold_Changes' and in the directory 'Pvalues' respectively, for each pairwise comparison between groups. 

The function is 'univariate' is a hub of univariate statistical tests. First, Shapiro Wilk's test for normality is performed for each variable, relying on class definition provided in the second column of the 'file'. If a variable results normally distributed, then Welch's T test is performed; otherwise, if a variable is not normally distributed, Wilcoxon-Mann Whitney U test is performed. For both these tests a threshold of 0.05 is used for assessing significance. After these testings a directory called 'Shapiro_Tests' is created containing the scores of every variable for normality; a directory called 'Welch_Tests' is created containing the p-values for each variable tested, for each pairwise comparison between gropus of samples; a directory called 'Mann-Whitney_Tests' is created containing the p-values for each variable tested, for each pairwise comparison between gropus of samples; a directory called 'Significant_Variables' is created reporting only the variables showing a p-value < 0.05, hence considered significantly different between groups.

If 'boxplot' is 'TRUE' boxplots for each variable, plotted according to class definition provided in the second column of the 'file', are generated and written in the directory called 'BoxPlot', automatically generated in the working directory. 

}

\references{
Goodpaster, AM, et al. Statistical Significance analysis of nuclear magnetic resonance-based metabonomics data. (2010) Anal Biochem. 401:134-143. 
}
\author{
Edoardo Gaude, Dimitrios Spiliotopoulos, Francesca Chignola, Silvia Mari, Andrea Spitaleri and Michela Ghitti
}

\examples{

## The function is currently defined as
function (file, plot.volcano = TRUE, multi.test = TRUE, normalize = TRUE, 
    boxplot = TRUE) 
{
    dirout.uni = paste(getwd(), "/Univariate/", sep = "")
    dir.create(dirout.uni)
    if (normalize) {
        shapiro(file, norm = TRUE)
    }
    else {
        shapiro(file, norm = FALSE)
    }
    if (normalize) {
        welch(file, norm = TRUE)
    }
    else {
        welch(file, norm = FALSE)
    }
    if (normalize) {
        wmw(file, norm = TRUE)
    }
    else {
        wmw(file, norm = FALSE)
    }
    if (multi.test && normalize) {
        pvalues(file, mtc = TRUE, norm = TRUE)
    }
    else if (!multi.test && normalize) {
        pvalues(file, mtc = FALSE, norm = TRUE)
    }
    else if (multi.test && !normalize) {
        pvalues(file, mtc = TRUE, norm = FALSE)
    }
    if (normalize) {
        col.pvalues(file, norm = TRUE)
    }
    else {
        col.pvalues(file, norm = FALSE)
    }
    if (plot.volcano && normalize) {
        volcano(file, plot.vol = TRUE, norm = TRUE)
    }
    else if (!plot.volcano && normalize) {
        volcano(file, plot.vol = FALSE, norm = TRUE)
    }
    else if (plot.volcano && !normalize) {
        volcano(file, plot.vol = TRUE, norm = FALSE)
    }
    if (normalize) {
        box.plot(file, norm = TRUE)
    }
    else {
        box.plot(file, norm = FALSE)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
