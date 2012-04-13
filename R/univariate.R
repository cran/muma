univariate <-
function(file, plot.volcano=TRUE, multi.test=TRUE, normalize=TRUE, boxplot=TRUE) {
dirout.uni = paste(getwd(), "/Univariate/", sep="")
dir.create(dirout.uni)
if (normalize) {shapiro(file, norm=TRUE)} else {shapiro(file, norm=FALSE)}
if (normalize) {welch(file, norm=TRUE)} else {welch(file, norm=FALSE)}
if (normalize) {wmw(file, norm=TRUE)} else {wmw(file, norm=FALSE)}
if (multi.test && normalize) {pvalues(file, mtc=TRUE, norm=TRUE)} else if (!multi.test && normalize) {pvalues(file, mtc=FALSE, norm=TRUE)} else if (multi.test && !normalize) {pvalues(file, mtc=TRUE, norm=FALSE)}
if (normalize) {col.pvalues(file, norm=TRUE)} else {col.pvalues(file, norm=FALSE)}
if (plot.volcano && normalize) {volcano(file, plot.vol=TRUE, norm=TRUE)} else if (!plot.volcano && normalize) {volcano(file, plot.vol=FALSE, norm=TRUE)} else if (plot.volcano && !normalize) {volcano(file, plot.vol=TRUE, norm=FALSE)}
if (normalize) {box.plot(file, norm=TRUE)} else {box.plot(file, norm=FALSE)}
}
