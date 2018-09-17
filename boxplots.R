
#
# Construct box plots for the given data columns. Groupings can be assgined to illustrate
# data distribution for multiple groups / conditions.
#
# Mark Christopher
# 2016
#

library(AUC)
library(lmodel2)
library(argparse)

args = commandArgs(TRUE)

# Parse cl args
parser = ArgumentParser()
parser$add_argument("data_path", type = "character", default = NULL,
                    help = "Path to csv file containing data.")
parser$add_argument("quant_cols", type = "character", default = NULL,
                    help = "Comma-separated list of cols for which box plots are constructed.")
parser$add_argument("group_col", type = "integer", default = 0,
                    help = "Column containing group assignments.")
parser$add_argument("-o", "--out_dir", type = "character", default = '.',
                    help = "Path to directory to store output.")

args = parser$parse_args(args)

data = read.csv(args$data_path, header = TRUE)
q_idxs = as.integer(unlist(strsplit(args$quant_cols, ",")))

cnames = colnames(data)

for(i in q_idxs){
  fname = paste(args$out_dir, '/', cnames[i], 'by', cnames[args$group_col], '-boxes.png', sep = '')
  png(filename = fname)
  boxplot(data[,i] ~ data[,args$group_col], main = "", xlab = "", ylab = "", border=c("red", "blue"), col=c("pink", "lightblue"), lwd = 4)
  dev.off()
}
