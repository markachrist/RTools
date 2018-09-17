#
# Applies agglomerative heirarchal clustering to the given input data.
#
# Mark Christopher
# 2015
#

# Parse command line args
options(echo = FALSE);
args = commandArgs(trailingOnly = TRUE);

distsPath = args[1];
k = strtoi(args[2]);
outputPath = args[3];

distsPath = "/Users/mchristopher/Documents/Data/OHTS-Longitudinal/longitudinal/std-pcs/dtw-basic-dists/loess-weighted-dists.txt";

dists = as.matrix(read.table(distsPath, header = FALSE));

# Heirarchal clustering, agglomerative tree building using complete distance metric
clustering = hclust(as.dist(dists), method = "complete");
plot(clustering)

groups = as.factor(cutree(clustering, k = k));

write.table(groups, file = outputPath, row.names = FALSE, col.names = FALSE)
