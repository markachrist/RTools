#
# Create logistic regression models using specified predictor & response variables.
# Leave-one-out (or cross validation) can be applied by using the -i option. For each unique
# ID in the column indicated by -i, a model excluding data points with that ID is built.
#
# Mark Christopher
#
#

library(argparse)

# Parse cl args
parser = ArgumentParser()
parser$add_argument("data_path", type = "character", default = NULL,
                    help = "Path to input data.")
parser$add_argument("-i", "--id_idx", type = "integer", default = 1,
                    help = "Column index of ID used to partition data.")
parser$add_argument("-p", "--p_idxs", type = "character", default = "2,3,4,5,6,7,8,9,10,11",
                    help = "Comma-separated list of column indices of predictor variables.")
parser$add_argument("-r", "--r_idx", type = "integer", default = -1,
                    help = "Column indices of predictor variables.")
args = parser$parse_args(args)

# Get input data
data = read.table(args$data_path, header = TRUE, sep = ',')
p_idxs = as.integer(unlist(strsplit(args$p_idxs, ",")))

if (args$r_idx < 0){
  args$r_idx = dim(data)[2]
}

# Get unique set of IDs
ids = unique(data[, args$id_idx])

output = NA*matrix(1, dim(data)[1])

# For each ID
for (id in ids){
  
  omit_rows = which(data[,args$id_idx] == id)
  cur = data
  cur[omit_rows,] = NA
  
  to_fit = cur[,p_idxs]
  to_fit$response = data[, args$r_idx]
  
  model = glm(response ~ ., data = to_fit, family = binomial(link='logit'), na.action = na.omit)
  
  output[omit_rows] = as.matrix(predict(model, data[omit_rows,]))
}


