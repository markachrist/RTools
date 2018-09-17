#
# Evaluates clustering by comparing cluster groups to a target categorical variable
# and computing within/between cluster quantitative metrics.
#
# Mark Christopher
# 2015
#

library(fpc)

options(echo = FALSE);
args = commandArgs(trailingOnly = TRUE);

distPath = args[1];
clusterPath = args[2];
targetPath = args[3];

dists = read.table(distPath, header = FALSE, sep = "\t");
clusters = as.integer(read.table(clusterPath, header = FALSE)[,1]);
target = as.integer(read.table(targetPath, header = FALSE)[,1] + 1);

# Compute cluster stats
t = table(target, clusters);
cstats = cluster.stats(dists, clusters)

# Report them
cat("within")
cat("\t")
cat("between")
cat("\t")
cat("wb.ratio")
cat("\t")
cat("p.value")
cat("\n")

cat(cstats$average.within)
cat("\t")
cat(cstats$average.between)
cat("\t")
cat(cstats$wb.ratio)
cat("\t")
cat(summary(t)$p.value)
cat("\n")

# Print formatted confusion matrix
cat("\n")
for(j in 1 : dim(t)[2]){
  cat("\t")
  cat(j)
}
cat("\n")

for(i in 1 : dim(t)[1]){
  
  cat(i)
  for(j in 1 : dim(t)[2]){
    
    cat("\t")
    cat(t[i, j])
    
  }
  cat("\n")
}
