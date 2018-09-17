#
# Distance analysis.
#
# Usage:
# 
# %Rscript distances.R [options] <pairwise distances>
#
# Mark Christopher
# 2016
#

library(argparse)

# Parse cl args
parser = ArgumentParser()

# Required
parser$add_argument("distances", nargs = '+', default = c(""), 
                    help = "Path to file containing pairwise distances between points. Should be square, symmetric matrix")

# Optional
parser$add_argument("-l", "--labels", type = "character", default = ",", 
                    help = "File containing labels for each data point (in same order as they appear in pairwise distance file)")
parser$add_argument("-d", "--delim", type = "character", default = ",", help = "Delimitter separating columns in data files")
parser$add_argument("-o", "--output", type = "character", default = "", help = "Output destination")

args = parser$parse_args()

dists = read.table(args$distances[1], sep = ",")

other = list()

for(i in 2 : length(args$distances)){
  print(paste("Reading dists from: ", args$distances[i], sep = ""))
  other[[ i - 1 ]] = read.table(args$distances[i], sep = args$delim)
}

sums = matrix(0, nrow = dim(dists)[1], ncol = dim(dists)[2])
for(i in 1 : length(other)){
  sums = sums + other[[i]]
}


for(i in 1 : length(other)){
  cur = other[[i]]
  p = cor(dists[lower.tri(dists)], cur[lower.tri(cur)])
  cat(args$distances[i + 1])
  cat("\t")
  cat(p)
  cat("\n")
}





