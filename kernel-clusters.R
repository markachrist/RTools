#
# Uses R package kernlab to apply kernel k means or spectral clustering to the given input data.
#
# Mark Christopher
# 2015
#

library(Matrix)
library(kernlab)

# Parse command line args
options(echo = FALSE);
args = commandArgs(trailingOnly = TRUE);

distsPath = args[1];
k = strtoi(args[2]);
outputPath = args[3];

useSpectral = FALSE;
if(length(args) >= 4){
  useSpectral = TRUE;
}

dists = as.matrix(read.table(distsPath, header = FALSE));

# Convert to positve-definite matrix
distsPD = as.matrix(nearPD(dists)$mat);
kdists = as.kernelMatrix(as.matrix(distsPD));

if(useSpectral){
  clustering = specc(kdists, k);
} else{
  clustering = kkmeans(kdists, k);
}

write.table(as.factor(clustering), file = outputPath, row.names = FALSE, col.names = FALSE)
