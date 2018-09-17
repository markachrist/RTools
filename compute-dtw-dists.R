#
# Computes pairwise distances between a set of time series using dynamic time warping (DTW) alignment.
#
# Usage:
# 
# %Rscript compute-dtw-dists.R <data table> <list of columns> <output dir>
#
# Mark Christopher
# 2015
#

library(argparse)
library(dtw)

# Parse cl args
parser = ArgumentParser()

# Required
parser$add_argument("series_data", nargs = '+', default = c(""), help = "Path files containing time series data")

# Optional
parser$add_argument("-e", "--openend", action = "store_true", default = FALSE, 
                    help = "Semi-global alignment performed by not requiring alignment at end of sequences")
parser$add_argument("-w", "--window", type = "character", default = "none", help = "Window used to limit search for alignment")
parser$add_argument("-d", "--delim", type = "character", default = ",", help = "Delimitter separating columns in data files")
parser$add_argument("-n", "--normalized", action = "store_true", default = FALSE, help = "Output normalized alignment distances")
parser$add_argument("-o", "--output", type = "character", default = "", help = "Output destination")

args = parser$parse_args()

for(i in 1 : length(args$series_data)){
  
  cur = read.table(args$series_data[i], header = TRUE, sep = args$delim)
  
  if(i == 1){
    series = array(NA, c(dim(cur)[2], length(args$series_data), dim(cur)[1]))
  }
  
  series[,i,] = t(as.matrix(cur))
}

n = dim(series)[3]
dists = matrix(0, n, n)

defdists = matrix(0, n, n)

if(args$openend){
  cat("Performing semi-global alignment...")
} else{
  cat("Performing global alignment...")
}

for(i in 1 : (n - 1)){
  
  ref = series[-1,,i]
#   ref = series[i, -1]
#   ref = t(ref[,! is.na(ref)])
  
  for(j in i : n){
    
    query = series[-1,,j]
#     query = series[j, -1]
#     query = t(query[,! is.na(query)])
    
    d = 0
    align = dtw(query, ref, keep = FALSE,
                dist.method = "Euclidean",
                distance.only = TRUE,
                open.begin = FALSE, 
                open.end = args$openend, 
                window.type = args$window)
    
    if(args$normalized){
      d = align$normalizedDistance
    } else{
      d = align$distance
    }
    
    dists[i, j] = d
    dists[j, i] = d
  }
}

write.table(dists, file = args$output, sep = args$delim, col.names = FALSE, row.names = FALSE)
