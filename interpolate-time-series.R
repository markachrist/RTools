#
# Computes interpolated values from the given data using LOESS smoothing/interpolation.
#
# Mark Christopher
# 2016
#

library(argparse)

# Parse cl args
parser = ArgumentParser()

# Required
parser$add_argument("data_points", nargs = 1, default = "", help = "Path to CSV containing data values")
parser$add_argument("time_points", nargs = 1, default = "", help = "Path to CSV containing original time points")
parser$add_argument("interp_points", nargs = 1, default = "", help = "Comma-separated list of new time points at which data values should be interpolated")
parser$add_argument("output", nargs = 1, default = "interpolated-data.csv", help = "Output destination")

# Optional
parser$add_argument("-l", "--header", action = "store_true", default = FALSE, help = "Output should include header line")
parser$add_argument("-d", "--delim", type = "character", default = ",", help = "Delimitter separating columns in data files")

args = parser$parse_args()

interp_points = as.double(unlist(strsplit(args$interp_points, ",")))

data = read.table(args$data_points, header = args$header, sep = args$delim)
times = read.table(args$time_points, header = args$header, sep = args$delim)

# Exclude series that don't cover range of interpolated values
min_times = apply(times[,-1], 1, min, na.rm = TRUE)
max_times = apply(times[,-1], 1, max, na.rm = TRUE)

data = data[max_times >= max(interp_points) & min_times <= min(interp_points),]
times = times[max_times >= max(interp_points) & min_times <= min(interp_points),]

n = dim(data)[1]
p = length(interp_points)

# Perform interpolation
interpedY = matrix(0, nrow = n, ncol = p + 1)
interpedY[ ,1] = data[ ,1]

for(i in 1 : n){
  
  nas = ! is.na(data[i, -1])
  
  curS = data[i, -1]
  curS = t(curS[, nas])
  
  curT = times[i, -1]
  curT = t(curT[, nas])
  
  if(length(curS) != 0){
    lmodel = loess(curS ~ curT, span = 2.0/3.0)
    interpedY[i, -1] = predict(lmodel, newdata = data.frame(curT = interp_points))
  }
}

# Write ouput
out = data.frame(interpedY)
names(out) = c("id", interp_points)
write.table(out, file = args$output, row.names = FALSE, col.names = args$header, quote = FALSE, sep = args$delim)
