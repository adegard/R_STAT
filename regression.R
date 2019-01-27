height = c(176, 154, 138, 196, 132, 176, 181, 169, 150, 175)
weight = c(82, 49, 53, 112, 47, 69, 77, 71, 62, 78)

#library(readr)
#DvsI <- read_delim("C:/Users/degar_000/Desktop/DvsI.csv", 
#                   ";", escape_double = FALSE, trim_ws = TRUE)
#View(DvsI)

#plot(DvsI$D, DvsI$I) #simple plot

#return stats
summary(height)
hist(height)


plot(weight, height, pch = 16, cex = 1.3, col = "red", main = "MY FIRST PLOT USING R", xlab = "WEIGHT (kg)", ylab = "HEIGHT (cm)")

# linear regression
lm(height~weight)

#fit line
abline(98.0054, 0.9528)


#learn more
mod <-  lm(height ~ weight)
summary(mod)

# create a vector of fitted values
regmodel <- predict(lm(height ~ weight))
regmodel

#plot it
plot(weight, height, pch = 16, cex = 1.3, col = "blue", main = "HEIGHT PLOTTED AGAINST BODY MASS", xlab = "BODY MASS (kg)", ylab = "HEIGHT (cm)")
abline(lm(height ~ weight)) 

#know number of data points
npoints <- length(height)
npoints

# draw the residuals
for (k in 1: npoints)  lines(c(weight[k], weight[k]), c(height[k], regmodel[k]))

