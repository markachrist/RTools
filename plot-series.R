#
# Plot time series data.
#
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
parser$add_argument("-d", "--delim", type = "character", default = ",", help = "Delimitter separating columns in data files")
parser$add_argument("-l", "--header", action = "store_true", default = FALSE, help = "Data files have a header line that should be ignored")
parser$add_argument("-i", "--idcol", action = "store_true", default = FALSE, help = "Data files have an ID column that should be ignored")
parser$add_argument("-c", "--color", type = "character", default = "0.0,0.0,0.0,1.0", help = "Color in which time series are plotted")
parser$add_argument("-o", "--output", type = "character", default = "", help = "Output destination")

args = parser$parse_args()

# Get input data
xdata = read.table(args$xdata, header = args$header, sep = args$delim)
ydata = read.table(args$ydata, header = args$header, sep = args$delim)

if (args$idcol){
  xdata = xdata[,-1]
  ydata = ydata[,-1]
}

# Other option stuff...
color = as.double(unlist(strsplit(args$color, ",")))
color = rgb(color[1], color[2], color[3], color[4])

if (TRUE){
  xr = c(min(min(xdata, na.rm = TRUE)), max(max(xdata, na.rm = TRUE)));
  yr = c(min(min(ydata, na.rm = TRUE)), max(max(ydata, na.rm = TRUE)));
}

# Setup output
png(filename = args$output, width = 1500, height = 1000)

par(mar = c(5, 5, 5, 5))
plot(xr, yr, type = "n", ylab = args$ylabel, xlab = args$xlabel, main = args$title, cex.lab = 2.0, cex.main = 2.5)

n = dim(xdata)[1]

for(i in 1 : n){
  nas = ! is.na(ydata[i, -1])
  
  curX = xdata[i,]
  curX = curX[, nas]
  
  curY = ydata[i,]
  curY = curY[, nas]
  
  lines(t(curX), t(curY), type = "l", lwd = 1.0, col = color)
}

dev.off()

