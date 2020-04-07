# FIGURES PARAMETERS

# FIGURE 1: RAW CUMULATIVE NUMBER OF CASES

# conventions:

# each element is followed by .f1, ..., .fn for figure1, ..., figuren
# y.axti / x.axti = y/x axis ticks interval
# yseq / xseq = sequence of y/x axes ticks to plot
# y.cex / x.cex = font size of y / x axis
# y.font / x.font = font type of y / x axis (1 = normal, 2 = bold)

# ylab.cex = size of y-axis label
# ylab.f =   font of y-axis label
# ylab.line =  space between figure and y-axis label




date.format = "%d/%m"

start.date.f1 = as.Date("2020-03-08") # forced starting date for x-axis in fig1
x.axti.f1 = 10
y.axti.f1 = 500

yseq.f1 = seq(0, max(d.long$cumNumCases) - (max(d.long$cumNumCases) %% y.axti.f1), y.axti.f1)
xseq.f1 = seq.Date(from = start.date.f1, to = max(d$date), by = 7)

x.font.f1 = 2
y.font.f1 = 2

x.cex.f1 = 1
y.cex.f1 = 1

ylab.cex.f1 = 1
ylab.f.f1 = 2
ylab.line.f1 = 2.5 

xlab.cex.f1 = 1
xlab.f.f1 = 2
xlab.line.f1 = 2.5