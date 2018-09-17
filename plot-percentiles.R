# 
# Plot data with specified percentile values shown. Can also optionally report percentile values
# as text output.
# 
# Mark Christopher
# 2016
#

library(argparse)
library(ggplot2)

# Parse cl args
parser = ArgumentParser()

# Required
parser$add_argument("xdata", nargs = 1, default = "", help = "Path to CSV containing X (time) series")
parser$add_argument("ydata", nargs = 1, default = "", help = "Path to CSV containing Y series")

# Optional
parser$add_argument("-v", "--groups", type = "character", default = "", help = "Path to file indicating groups used to compute averages")
parser$add_argument("-t", "--title", type = "character", default = "Time Series", help = "Main title for the plot")
parser$add_argument("-x", "--xlabel", type = "character", default = "X axis", help = "Label for x-axis")
parser$add_argument("-y", "--ylabel", type = "character", default = "Y axis", help = "Label for y-axis")
parser$add_argument("-k", "--key", type = "character", default = "", help = "Labels for plot legend")
parser$add_argument("-r", "--yrange", type = "character", default = "", help = "Range of y-axis in plot (min/max y values used if not specified)")
parser$add_argument("-p", "--percentiles", type = "character", default = "0.25,0.75", help = "Upper and lower percentiles to include in the graph")
parser$add_argument("-d", "--delim", type = "character", default = ",", help = "Delimitter separating columns in data files")
parser$add_argument("-l", "--header", action = "store_true", default = FALSE, help = "Data files have a header line that should be ignored")
parser$add_argument("-i", "--idcol", action = "store_true", default = FALSE, help = "Data files have an ID column that should be ignored")
parser$add_argument("-c", "--color", type = "character", default = "0.0,0.0,0.0,1.0", help = "Color in which time series are plotted")
parser$add_argument("-o", "--output", type = "character", default = "", help = "Output destination for plot image")
parser$add_argument("-q", "--quantout", type = "character", default = "", help = "Output destination for quantile values")

args = parser$parse_args()

# Get input data
xdata = read.table(args$xdata, header = args$header, sep = args$delim)
ydata = read.table(args$ydata, header = args$header, sep = args$delim)

if (args$idcol){
  xdata = xdata[,-1]
  ydata = ydata[,-1]
}

percentiles = as.double(unlist(strsplit(args$percentiles, ",")))

# Other option stuff...
xr = c(min(min(xdata, na.rm = TRUE)), max(max(xdata, na.rm = TRUE)))
if (args$yrange != ""){
  yr = as.double(unlist(strsplit(args$yrange, ",")))
} else{
  yr = c(min(min(ydata, na.rm = TRUE)), max(max(ydata, na.rm = TRUE)))
}

# Setup output
png(filename = args$output, width = 1500, height = 1000)

# Get groups, if available
if (args$groups != ""){
  groups = read.table(args$groups, header = args$header, sep = args$delim)
  
  if (args$idcol){
    groups = groups[,-1]
  }
  
} else {
  groups = matrix(0, dim(xdata)[1], 1)
}

# Get labels
if(args$key != ""){
  args$key = unlist(strsplit(args$key, ","))
} else{
  args$key = c()
}

group_ids = unique(groups)
makeColors = colorRampPalette(c("blue", "red"))
group_colors = makeColors(length(group_ids))

# Create those plots...
plt = ggplot(data.frame()) + xlim(xr) + ylim(yr) + ggtitle(args$title) + labs(x = args$xlabel, y = args$ylabel) + 
  theme(plot.title = element_text(face = "bold", size = 32), panel.background = element_rect(fill = "white")) +
  theme(axis.title = element_text(face = "bold", size = 22))  + 
  theme(axis.text.x = element_text(face = "bold", size = 14)) + 
  theme(axis.text.y = element_text(face = "bold", size = 14))

ribbons = list()
dottedlow = list()
dottedhigh = list()
meds = list()
ytiles = list()
xtiles = list()
legend_rows = list()

for(i in 1 : length(group_ids)){

  fill_color = col2rgb(group_colors[i]) / 255
  fill_color = rgb(fill_color[1], fill_color[2], fill_color[3], 0.20)
  
  ytiles[[i]] = apply(ydata[groups == group_ids[i],], 2, quantile, probs = c(percentiles[1], 0.50, percentiles[2]))
  xtiles[[i]] = apply(xdata[groups == group_ids[i],], 2, quantile, probs = c(percentiles[1], 0.50, percentiles[2]))
  
  if(! is.null(args$key)){
    meds[[i]] = geom_line(aes_q(x = xtiles[[i]][2,], y = ytiles[[i]][2,], color = args$key[[i]]))
  } else{
    meds[[i]] = geom_line(aes_q(x = xtiles[[i]][2,], y = ytiles[[i]][2,]), color = group_colors[i])
  }
  
  ribbons[[i]] = geom_ribbon(aes_q(x = xtiles[[i]][2,], ymin = ytiles[[i]][1,], ymax = ytiles[[i]][3,]), fill = fill_color)
  dottedlow[[i]] = geom_line(aes_q(x = xtiles[[i]][2,], y = ytiles[[i]][1,]), color = group_colors[i], linetype = "dotted")
  dottedhigh[[i]] = geom_line(aes_q(x = xtiles[[i]][2,], y = ytiles[[i]][3,]), color = group_colors[i], linetype = "dotted")
}

legend = NULL
if(! is.null(args$key)){
  legend = scale_color_manual(breaks = args$key, values = group_colors, guide = guide_legend(title = ""))
}

plt + ribbons + dottedlow + dottedhigh + meds + legend + 
  theme(panel.grid.minor = element_blank()) + 
  theme(axis.line = element_line(size = .5, colour = "black", linetype = "solid")) +
  theme(axis.ticks = element_line(size = .25, colour = "black", linetype = "solid")) +
  theme(axis.text = element_text(face = "plain", size = 8, color = "black")) + 
  # theme(axis.title.x = element_text(margin = margin(4))) +
  # theme(axis.title.y = element_text(margin = margin(b = 4))) +
  theme(legend.position = c(0.95,0.95), legend.direction = "vertical") +
  theme(legend.justification = "right", legend.key = element_blank()) +
  theme(legend.text = element_text(size = 14))

# guides(color=guide_legend(override.aes=list(fill=NA)))

dev.off()

# Write quantiles to text, if needed
if (args$quantout != ""){
  
  quant_data = data.frame()
  for(i in 1 : length(group_ids)){
    quant_data = rbind(quant_data, cbind(matrix(group_ids[i], nrow = dim(ytiles[[i]])[1], ncol = 1), ytiles[[i]]))
  }
  names(quant_data)[1] = "group"
  
  write.table(quant_data, file = args$quantout, sep = ",", quote = FALSE)
}

