#
# Smooths the given time series data using LOESS method.
#
# Mark Christopher
# 2016
#

library(argparse)

# Parse cl args
parser = ArgumentParser()

# Required
parser$add_argument("xdata", nargs = 1, default = "", help = "Path to CSV containing X (time) series")
parser$add_argument("ydata", nargs = 1, default = "", help = "Path to CSV containing Y series")

# Optional
parser$add_argument("-t", "--title", type = "character", default = "Time Series", help = "Main title for the plot")
parser$add_argument("-x", "--xlabel", type = "character", default = "X axis", help = "Label for x-axis")
parser$add_argument("-y", "--ylabel", type = "character", default = "Y axis", help = "Label for y-axis")
parser$add_argument("-c", "--color", type = "character", default = "1.0,0.0,0.0,1.0", help = "Color in which time series are plotted")
parser$add_argument("-o", "--output", type = "character", default = "", help = "Output destination")

args = parser$parse_args()

color = as.double(unlist(strsplit(args$color, ",")))




