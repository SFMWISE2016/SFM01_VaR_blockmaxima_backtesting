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

# Create portfolio
e = a + b + c
e = as.matrix(e)  # define as matrix
end = NROW(e)  # number of observations
x = e[2:end] - e[1:(end - 1)]  # returns
x = -x  # negative returns
T = length(x)  # number of observations 
h = 250  # observation window
p = 0.95  # quantile for VaR
n = 16  # observation window for estimating quantile in VaR
k = T/n

# Evaluate VaR
mu = matrix(, , , )
sigma = matrix(, , , )
gamma = matrix(, , , )
VaR = matrix(, , , )

for (i in 1:(T - 250)) {
    y = x[i:(i + 249)]
    z = matrix(, , , )
    
    for (j in 1:k) {
        r = y[((j - 1) * n + 1):(j * n)]
        z[j] = max(r)
    }
    w = sort(z)
    
    GEV = gev.fit(w)  # Fit the Generalized Extreme Value Distribution
    
    mu[i] <- GEV$mle[1]  # location parameter
    sigma[i] <- GEV$mle[2]  # scale parameter
    gamma[i] <- GEV$mle[3]  # shape parameter
    
    VaR[i] <- mu[i] + sigma[i]/gamma[i] * ((-log(1 - p^n))^(-gamma[i]) - 1)
}


#write(VaR, file = "VaR_0216_R.txt", ncolumns = 1, sep = "\t")

