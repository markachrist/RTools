#
# Extracts time series data from a data set. 
#
# The first column should contain IDs and the column specified by <col> will be extracted and convert to a time series. Every
# row with a matching ID will be mapped to a single row in the output containing the ID and extracted values from <col> in
# the order they appear in <input data>.
#
# Mark Christopher
# 2015
#

library(argparse)

# Parse cl args
parser = ArgumentParser()

# Required
parser$add_argument("col", nargs = 1, type = 'integer', default = 2, 
                    help = "The column that should be extracted from the data file.")
parser$add_argument("data", nargs = 1, default = "", 
                    help = "Path to file containing data arranged into delimitted columns with IDs in the first col")

# Optional
parser$add_argument("-d", "--delim", type = "character", default = ",", help = "Delimitter separating columns in data files")
parser$add_argument("-s", "--standardize", action = "store_true", default = FALSE, help = "Data in extract column should be standardized.")
parser$add_argument("-o", "--output", type = "character", default = "", help = "Output destination")

args = parser$parse_args()

# dataPath = args[1]
# column = as.integer(args[2])
# outPath = args[3]

# Read input data
data = read.table(args$data, header = TRUE, sep = args$delim)

if(args$standardize){
  data[, args$col] = scale(data[ , args$col])
}

# Set up indices...
ids = sort(unique(data[,1]))
numids = length(ids)

# Separate data on ID values
dataByID = vector(mode = "list", length = numids)
maxLength = 0

for(i in 1:numids){
  dataByID[[i]] = data[data$ran_id == ids[i], args$col]
  
  if(maxLength < length(dataByID[[i]])){
    maxLength = length(dataByID[[i]])
  }
}

# Transfer to table
series = matrix(NA, numids, maxLength + 1)

for(i in 1:numids){
  curLength = length(dataByID[[i]])
  series[i, 1] = ids[i]
  series[i, 2 : (curLength + 1)] = dataByID[[i]]
}

write.table(series, file = args$output, row.names = FALSE, col.names = FALSE, sep = args$delim)

