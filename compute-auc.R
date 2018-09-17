#
# Compute AUC from the given truth / prediction values.
#
# Mark Christopher
# 2016
#

suppressMessages(library(AUC))
suppressMessages(library(lmodel2))
suppressMessages(library(argparse))

args = commandArgs(TRUE)

# Parse cl args
parser = ArgumentParser()
parser$add_argument("predict_path", type = "character", default = NULL,
                    help = "Path to csv file containing data.")
parser$add_argument("truth_path", type = "character", default = NULL,
                    help = "Path file containing binary truth.")
parser$add_argument("--invert", type = "integer", default = 0,
                    help = "Invert class predictions.")
parser$add_argument("-a", "--predict_col", type = "integer", default = 1,
                    help = "Column of prediction file to use.")
parser$add_argument("-b", "--truth_col", type = "integer", default = 1,
                    help = "Column of truth file to use.")
args = parser$parse_args(args)

predict = read.csv(args$predict_path, header = FALSE)
truth = read.table(args$truth_path, sep = ' ', header = FALSE)

cat(auc(roc(predict[,args$predict_col], as.factor(truth[,args$truth_col]))))
cat('\n')

