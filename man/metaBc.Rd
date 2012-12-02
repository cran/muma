\name{MetaBc}
\alias{MetaBc}
\docType{data}
\title{
Metabolomics data of B cells
}
\description{
Data from metabolomics analysis of B cells harvested in different conditions and at different time points
}
\usage{data(MetaBc)}
\format{
  A data frame with 25 observations on the following 100 variables.
  \describe{
    \item{\code{Samples}}{a factor with levels \code{-LPS1} \code{-LPS2} \code{-LPS3} \code{-LPS4} \code{-LPS5} \code{DAY1.1} \code{DAY1.2} \code{DAY1.3} \code{DAY1.4} \code{DAY1.5} \code{DAY2.1} \code{DAY2.2} \code{DAY2.3} \code{DAY2.4} \code{DAY2.5} \code{DAY3.1} \code{DAY3.2} \code{DAY3.3} \code{DAY3.4} \code{DAY3.5} \code{DAY4.1} \code{DAY4.2} \code{DAY4.3} \code{DAY4.4} \code{DAY4.5}}
    \item{\code{Class}}{a numeric vector}
    \item{\code{X0.89}}{a numeric vector}
    \item{\code{X0.93}}{a numeric vector}
    \item{\code{X0.95}}{a numeric vector}
    \item{\code{X0.98}}{a numeric vector}
    \item{\code{X1}}{a numeric vector}
    \item{\code{X1.03}}{a numeric vector}
    \item{\code{X1.11}}{a numeric vector}
    \item{\code{X1.17}}{a numeric vector}
    \item{\code{X1.17.1}}{a numeric vector}
    \item{\code{X1.21}}{a numeric vector}
    \item{\code{X1.24}}{a numeric vector}
    \item{\code{X1.3}}{a numeric vector}
    \item{\code{X1.32}}{a numeric vector}
    \item{\code{X1.43}}{a numeric vector}
    \item{\code{X1.47}}{a numeric vector}
    \item{\code{X1.56}}{a numeric vector}
    \item{\code{X1.58}}{a numeric vector}
    \item{\code{X1.64}}{a numeric vector}
    \item{\code{X1.71}}{a numeric vector}
    \item{\code{X1.71.1}}{a numeric vector}
    \item{\code{X1.89}}{a numeric vector}
    \item{\code{X1.9}}{a numeric vector}
    \item{\code{X2.03}}{a numeric vector}
    \item{\code{X2}}{a numeric vector}
    \item{\code{X2.06}}{a numeric vector}
    \item{\code{X2.11}}{a numeric vector}
    \item{\code{X2.22}}{a numeric vector}
    \item{\code{X2.34}}{a numeric vector}
    \item{\code{X2.37}}{a numeric vector}
    \item{\code{X2.39}}{a numeric vector}
    \item{\code{X2.44}}{a numeric vector}
    \item{\code{X2.53}}{a numeric vector}
    \item{\code{X2.51}}{a numeric vector}
    \item{\code{X2.49}}{a numeric vector}
    \item{\code{X2.65}}{a numeric vector}
    \item{\code{X2.73}}{a numeric vector}
    \item{\code{X2.8}}{a numeric vector}
    \item{\code{X2.84}}{a numeric vector}
    \item{\code{X2.91}}{a numeric vector}
    \item{\code{X3.03}}{a numeric vector}
    \item{\code{X3.13}}{a numeric vector}
    \item{\code{X3.19}}{a numeric vector}
    \item{\code{X3.24}}{a numeric vector}
    \item{\code{X3.23}}{a numeric vector}
    \item{\code{X3.27}}{a numeric vector}
    \item{\code{X3.26}}{a numeric vector}
    \item{\code{X3.33}}{a numeric vector}
    \item{\code{X3.38}}{a numeric vector}
    \item{\code{X3.39}}{a numeric vector}
    \item{\code{X3.4}}{a numeric vector}
    \item{\code{X3.41}}{a numeric vector}
    \item{\code{X3.41.1}}{a numeric vector}
    \item{\code{X3.42}}{a numeric vector}
    \item{\code{X3.45}}{a numeric vector}
    \item{\code{X3.48}}{a numeric vector}
    \item{\code{X3.47}}{a numeric vector}
    \item{\code{X3.49}}{a numeric vector}
    \item{\code{X3.5}}{a numeric vector}
    \item{\code{X3.53}}{a numeric vector}
    \item{\code{X3.56}}{a numeric vector}
    \item{\code{X3.61}}{a numeric vector}
    \item{\code{X3.65}}{a numeric vector}
    \item{\code{X3.63}}{a numeric vector}
    \item{\code{X3.67}}{a numeric vector}
    \item{\code{X3.69}}{a numeric vector}
    \item{\code{X3.69.1}}{a numeric vector}
    \item{\code{X3.72}}{a numeric vector}
    \item{\code{X3.77}}{a numeric vector}
    \item{\code{X3.83}}{a numeric vector}
    \item{\code{X3.81}}{a numeric vector}
    \item{\code{X3.8}}{a numeric vector}
    \item{\code{X3.85}}{a numeric vector}
    \item{\code{X3.84}}{a numeric vector}
    \item{\code{X3.91}}{a numeric vector}
    \item{\code{X3.89}}{a numeric vector}
    \item{\code{X3.95}}{a numeric vector}
    \item{\code{X4}}{a numeric vector}
    \item{\code{X4.03}}{a numeric vector}
    \item{\code{X4.06}}{a numeric vector}
    \item{\code{X4.11}}{a numeric vector}
    \item{\code{X4.17}}{a numeric vector}
    \item{\code{X4.24}}{a numeric vector}
    \item{\code{X4.33}}{a numeric vector}
    \item{\code{X5.38}}{a numeric vector}
    \item{\code{X6.51}}{a numeric vector}
    \item{\code{X6.88}}{a numeric vector}
    \item{\code{X7.04}}{a numeric vector}
    \item{\code{X7.18}}{a numeric vector}
    \item{\code{X7.32}}{a numeric vector}
    \item{\code{X7.41}}{a numeric vector}
    \item{\code{X7.36}}{a numeric vector}
    \item{\code{X7.53}}{a numeric vector}
    \item{\code{X7.74}}{a numeric vector}
    \item{\code{X8.1}}{a numeric vector}
    \item{\code{X8.19}}{a numeric vector}
    \item{\code{X8.44}}{a numeric vector}
    \item{\code{X8.71}}{a numeric vector}
    \item{\code{X8.93}}{a numeric vector}
  }
}
\details{
Variables are named as NMR chemical shifts and values are the areas of NMR peaks, at any given chemical shift. The first column of the dataset represents the name of each sample and is informative of sample condition/treatment. The second column of the dataset reports the information regarding the belonging of each sample to a specific class of condition/treatment.
}
\source{
Garcia-Manteiga, J.M. Metabolomics of B to Plasma Cell Differentiation. (2011) J Proteom Res.10(9)4165-4176.
}
