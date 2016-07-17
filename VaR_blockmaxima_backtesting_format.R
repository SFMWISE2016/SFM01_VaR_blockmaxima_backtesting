# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("ismev")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load data
a = read.table("Bayer_close_0216.txt")
b = read.table("Bmw_close_0216.txt")
c = read.table("Siemens_close_0216.txt")
v = read.table("VaR_0216_R.txt")

# Size of window
h = 250
v = -v
V = a + b + c
D = dim(V)
L = V[-1, ] - V[1:(D[1] - 1), ]
T = length(L)


outlier = matrix(, 1, T - h)
exceedVaR = matrix(, , )

# Check for exceedances
exceedVaR = (L[(1 + h):(D[1] - 1)] < v[1:(T - h), ])

# Find exceedances
for (j in 1:(T - h)) {
    if (exceedVaR[j] == TRUE) {
        outlier[j] = L[j + h]
    }
}

K.1 = which(is.finite(outlier))
K = is.finite(outlier)
outlier = outlier[K.1]

# Calculate the exceedance ratio
p = round(sum(exceedVaR)/(T - h), 4)

# Plot the values, VaR estimation and the exceedances
# setwd('c:\\Users\\hp\\Desktop\\SFS01_VaR_blockmaxima_backtesing')
# #save photo png(file='VaR_blockmax_backtesting.png', bg='transparent')

plot(L[(h + 1):(D[1] - 1)], pch = 18, col = "blue", ylim = c(-25, 25), xlab = c(""), 
    ylab = c(""), axes = FALSE)
box()
axis(1, seq(0, length = 8, by = 480), seq(2002, 2016, by = 2))
axis(2)
title("Block Maxima Model")
points(K.1, outlier, pch = 18, col = "magenta")
lines(v, col = "red", lwd = 2)

yplus = K.1 * 0 + min(L[(h + 1):(D[1] - 1)]) - 2
points(K.1, t(yplus), pch = 3, col = "dark green")
legend("topright", c("Profit/Loss", "VaR", "Exceedances"), pch = c(18, 15, 18), 
    col = c("blue", "red", "magenta"))
# dev.off()

# Print the exceedances ratio
print(paste("Exceedances ratio:", "", p))
