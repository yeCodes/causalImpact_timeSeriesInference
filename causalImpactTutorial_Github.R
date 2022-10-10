
# bayesian structural time series modelling
# gitHub tutorial - https://google.github.io/CausalImpact/CausalImpact.html

#install.packages("CausalImpact")
# update.packages()
library(CausalImpact)

# location where packages installed
.libPaths()


# Creating dummy data series
set.seed(1)         # for the different sampling schemes
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)  #https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/arima.sim
y <- 1.2 * x1 + rnorm(100)
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)


# dims
dim(data)

# line graph
matplot(data, type = "l")    # plot col of 1 matrix against another on same axes

# RUNNING ANALYSIS - specify training + test data
pre.period <- c(1, 70)
post.period <- c(71, 100)

impact <- CausalImpact(data, pre.period, post.period)

plot(impact)


#******************
# creating sequence of data from specified start date with intervals of 1 day
time.points <- seq.Date(as.Date("2014-01-01"), by = 1, length.out = 100)

# creating a 2D zoo series here. Putting data into time series format
data <- zoo(cbind(y, x1), time.points)
head(data)

# convert string to date format
pre.period <- as.Date(c("2014-01-01", "2014-03-11"))
post.period <- as.Date(c("2014-03-12", "2014-04-10"))

impact <- CausalImpact(data, pre.period, post.period)
plot(impact)


##### SUMMARY STATS
summary(impact)

summary(impact, "report")

##****************CUSTOM MODEL***********************#
#

# recall y is the "response variable" aka the target aka the dependent variable

# add NAs to post-event response
post.period <- c(71, 100)
# ths is the actual resposne that was observed
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA

# Simple BSTS model - stored bsts.model
ss <- AddLocalLevel(list(), y)
bsts.model <- bsts(y ~ x1, ss, niter = 1000)

# pass to CausalImpact - bsts.model with predictions in NA 
# spot presumably and the actual post-period response

impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)

plot(impact)
# print table of summart statistics
summary(impact)
# print useful text report 
summary(impact, "report")

# this shows inclusion probability of coefficients - useful. AKin to frequentist
# stat significance
plot(impact$model$bsts.model, "coefficients")



