# 
# Generate scatter plots given set of x,y points. Can specify plot parameters (labels, titles,
# range, etc.) and assign data points to groups for different plot colors. 
# 
# Mark Christopher
# 2016
#

library(argparse)
library(ggplot2)

# Parse cl args
parser = ArgumentParser()

# Required
parser$add_argument("data", nargs = 1, default = "", help = "Path to file containing data to plot")
parser$add_argument("xcol", nargs = 1, default = 1, help = "Column in file containing X data for plot")
parser$add_argument("ycol", nargs = 1, default = 1, help = "Column in file containing Y data for plot")

# Optional
parser$add_argument("-g", "--groups", type = "character", default = "", help = "Column or path to file indicating groupings of points")
parser$add_argument("-d", "--delim", type = "character", default = ",", help = "Delimitter separating columns in data files")
parser$add_argument("-l", "--header", action = "store_true", default = FALSE, help = "Data files have a header line that should be ignored")
parser$add_argument("-o", "--output", type = "character", default = "", help = "Output destination for plot image")
parser$add_argument("-t", "--title", type = "character", default = "Time Series", help = "Main title for the plot")
parser$add_argument("-x", "--xlabel", type = "character", default = "X axis", help = "Label for x-axis")
parser$add_argument("-y", "--ylabel", type = "character", default = "Y axis", help = "Label for y-axis")
parser$add_argument("--yrange", type = "character", default = "", help = "Range of y-axis in plot (min/max y values used if not specified)")
parser$add_argument("--xrange", type = "character", default = "", help = "Range of x-axis in plot (min/max y values used if not specified)")
parser$add_argument("--color1", type = "character", default = "0.0,0.0,1.0,1.0", help = "Color in which points are plotted.")
parser$add_argument("--color2", type = "character", default = "1.0,0.0,0.0,1.0", 
                    help = "If groups are provided, a gradient of colors between color1 & color2 are used to determine the colors used to plot each group.")

args = parser$parse_args()


data = read.table(args$data, header = args$header, sep = args$delim)
args$xcol = as.integer(args$xcol)
args$ycol = as.integer(args$ycol)

# Determine plot ranges
if (args$xrange != ""){
  xr = as.double(unlist(strsplit(args$xrange, ",")))
  cat("Using the xrange = ")
  cat(xr)
  cat("\n")
} else{
  xr = c(min(min(data[,args$xcol], na.rm = TRUE)), max(max(data[,args$xcol], na.rm = TRUE)))
}

if (args$yrange != ""){
  yr = as.double(unlist(strsplit(args$yrange, ",")))
  cat("Using the yrange = ")
  cat(yr)
  cat("\n")
} else{
  yr = c(min(min(data[,args$ycol], na.rm = TRUE)), max(max(data[,args$ycol], na.rm = TRUE)))
}

# Get groups, if provided
if (args$groups != ""){
  if(is.na(as.integer(args$groups))){
    groups = read.table(args$groups, header = args$header)
  } else{
    groups = data[, as.integer(args$groups)]
  }
} else {
  groups = matrix(0, dim(xdata)[1], 1)
}

# Get plot colors
c1 = as.double(unlist(strsplit(args$color1, ",")))
c1 = rgb(c1[1], c1[2], c1[3], c1[4])
c2 = as.double(unlist(strsplit(args$color2, ",")))
c2 = rgb(c2[1], c2[2], c2[3], c2[4])
colors <- colorRampPalette(c(c1, c2))

group_ids = unique(groups)
group_colors = colors(length(group_ids))

# Create those plots...
png(filename = args$output, width = 1500, height = 1000)

plt = ggplot(data.frame()) + xlim(xr) + ylim(yr) + ggtitle(args$title) + labs(x = args$xlabel, y = args$ylabel) + 
  theme(plot.title = element_text(face = "bold", size = 32), panel.background = element_rect(fill = "white")) +
  theme(axis.title = element_text(face="bold", size = 22))  + 
  theme(axis.text.x = element_text(face="bold", size = 14)) + 
  theme(axis.text.y = element_text(face="bold", size = 14))

points = list()

for(i in 1 : length(group_ids)){
  points[[i]] = geom_point(aes_q(x = data[groups == group_ids[i], args$xcol], y = data[groups == group_ids[i], args$ycol]), color = group_colors[i])
}

plt + points
dev.off()
