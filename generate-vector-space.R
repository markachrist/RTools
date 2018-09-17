#
# Create a vector space in which data points have given pair-wise distances.
#
# Usage:
# 
# %Rscript generate-vector-space.R <output path> <dist 1 path> [<dist 2 path> ...]
#
# Mark Christopher
# 2015
#

library(argparse)

# Parse cl args
parser = ArgumentParser()

# Required
parser$add_argument("distances", nargs = 1, default = "", 
                    help = "Path to file containing pairwise distances between points. Should be square, symmetric matrix")

# Optional
parser$add_argument("-l", "--labels", type = "character", default = ",", 
                    help = "File containing labels for each data point (in same order as they appear in pairwise distance file)")
parser$add_argument("-n", "--newdim", type = "integer", default = 10, 
                    help = "Dimensionality of the generated vector space. Higher dimension -> more accurate representation of distances.")
parser$add_argument("-d", "--delim", type = "character", default = ",", help = "Delimitter separating columns in data files")
parser$add_argument("-o", "--output", type = "character", default = "", help = "Output destination")

args = parser$parse_args()

dists = read.table(args$distances, sep = args$delim)

n = dim(dists)[1]
x = cmdscale(dists, n - 1)

recon = x[, 1 : args$newdim]

# Report distance correlation...
recondist = as.matrix(dist(recon))
cat("Reconstructed distance correlation = ")
cat(cor(dists[lower.tri(dists)], recondist[lower.tri(recondist)]))
cat("\n")

# Save output
write.table(recon, file = args$output, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = args$delim)


