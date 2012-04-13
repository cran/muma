shapiro <-
function(file, norm) {
 comp = read.csv(file, sep=",", header=TRUE)
     comp.x = comp[,3:ncol(comp)]
     comp.x = cbind(comp[,2], comp[,1], comp.x)
     x <- comp.x
 x.x = x[,3:ncol(x)]
 rownames(x.x) = x[,2]
 k = matrix(x[,1], ncol=1)
  if (norm) {
 x.t <- t(x.x)
 x.s <- matrix(colSums(x.t), nrow=1)
 uni = matrix(rep(1,nrow(x.t)), ncol=1)
 area.uni<-uni%*%x.s
 x.areanorm<-x.t/area.uni
 x.areanorm = t(x.areanorm)
 x.n = cbind(k, x.areanorm)
  } else {
   x.n = cbind(k, x.x)
    }
 sorted = x.n[order(x.n[,1]),]
 g = c()
 for (i in 1:nrow(sorted)) { 
  if (any(g == sorted[i,1])) {g=g} 
  else {g=matrix(c(g,sorted[i,1]), ncol=1)}
 }
  #Create directories for saving ouput files
 dirout.r = paste(getwd(), "/Univariate/Groups", sep="")
 dir.create(dirout.r)
 dirout.s = paste(getwd(), "/Univariate/Shapiro_Tests", sep="")
 dir.create(dirout.s)
            # r.? files are created according to the class:
 for (i in 1:nrow(g)) {
  vuota <- c()
  fin = matrix(rep(NA, ncol(sorted)), nrow=1)
  for (j in 1:nrow(sorted)) { 
   if (sorted[j,1] == i) { 
   vuota <- matrix(sorted[j,], nrow=1)
   rownames(vuota) = rownames(sorted)[j]
   fin = rbind(fin, vuota)
   } 
  }
  nam=paste("r", i, sep=".")
  	#creati r.i elementi, contenenti le osservazioni appartenenti ad ogni gruppo.
  n = matrix(fin[-1,], ncol=ncol(sorted))
  n.x = matrix(n[,-1], ncol=ncol(sorted)-1)
  name = as.matrix(assign(nam, n.x))
  shapname = paste("shapiro", i, sep=".")
  shapiro = matrix(rep(NA, ncol(n)))
  for (q in 1:ncol(name)) {
   notAlist = c()
   notAlist = matrix(unlist(name[,q]))
          #if (VariablesInOutput="y") {
          #valore=shapiro.test(notAlist)$p.value
          #assign(paste("p", q, sep="."), valore)
          #}
   shapiro[q,] = shapiro.test(notAlist)$p.value
 assign(shapname, shapiro)
 
      #Alla fine di questo vengono creati shapiro.i elementi, che contengono il 
      #pvalue di normalita per ogni variabile, all'interno di ogni gruppo i.
 }
 outputfile=paste("r.",i,".out",sep="")
 write.csv(name, paste(dirout.r, outputfile, sep="/"))
 outshapiro=paste("ShapiroTest.", i, ".out", sep="")
 write.csv(shapiro,paste(dirout.s, outshapiro, sep="/"))
 }
}
