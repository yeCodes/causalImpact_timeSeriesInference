# Inferring the impact of the VW scandal on VW stock price
# Tutorial - https://www.youtube.com/watch?v=BkD7IkCwP2I
# Credit to: Data Heroes

# start date definition

start = "2014-01-01"
treatment = "2015-09-01" 
end = "2015-12-31"

# training and treatment == training set, test set
# as.Date converts string format to date format
pre.period = as.Date(c(start, treatment))
post.period = as.Date(c(treatment, end))


##install.packages(tseries) - instead installed from Tools >> Install Packages panel!!
library(tseries)
volkswagen = get.hist.quote(instrument = "VOW.DE",
                            start = start,
                            end = end,
                            quote = "Close",
                            compression = "w")

meta = get.hist.quote(instrument = "META",
                      start = start,
                      end = end,
                      quote = "Close",
                      compression = "w")

disney = get.hist.quote(instrument = "DIS",
                        start = start,
                        end = end,
                        quote = "Close",
                        compression = "w")

novartis = get.hist.quote(instrument = "NWS",
                          start = start,
                          end = end,
                          quote = "Close",
                          compression = "w")

carlsberg = get.hist.quote(instrument = "CARL-B.CO",
                           start = start,
                           end = end,
                           quote = "Close",
                           compression = "w")

# getting stocks together
stocks <- cbind(volkswagen, meta, disney, carlsberg)

# check correlations in training period.
# AIM: correlations among signals to aid with prediction - 5-20 according to Kay
correlation <- window(stocks,start = start, end = treatment)
cor(correlation)


#####***************************DISPLAY HEATMAP******************************###
## https://www.geeksforgeeks.org/how-to-create-correlation-heatmap-in-r/

# Install and load reshape2 package
install.packages("reshape2")
library(reshape2)

# creating correlation matrix
corr_mat <- round(cor(correlation),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4)

#####************************************************************************###

library(CausalImpact)
# Observations - VW - not correlated to Meta much.

# Action - remove META
final_stocks <- cbind(cbind(volkswagen, disney, carlsberg))

impact <- CausalImpact(data = final_stocks, 
                       pre.period = pre.period,
                       post.period = post.period)

summary(impact)
plot(impact)