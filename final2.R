## Adam Roen ##
## GIS 470 ##
## Final Project ##

## set working directory ##
setwd("C:/Users/Adam/Desktop/470/final_proj")

## read in files ##
health_data <- read.csv("health_tracts_raw_only-3.csv")

## package installation ##
install.packages("ggplot2")
install.packages("jtools")
install.packages("ppcor")
install.packages("stargazer")

## library ##
library(ggplot2)
library(jtools)
library(ppcor)
library(stargazer)

## turn off e notation ##
options(scipen = 999)

## creating variable for difference in population ## 
health_data$popDiff <- health_data$Pop_2017 - health_data$Pop_2016

## population plots ##
## pop_2016 plot ##
popPlot1 <- ggplot(data = health_data, aes(x = Pop_2016))
print(popPlot1)

popPlot1 <- popPlot1 + geom_freqpoly()
print(popPlot1)

popPlot1 <- popPlot1 +
  xlab("Population in 2016") +
  ylab("Count across cities") +
  ggtitle("Population in 2016 across cities") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))
print(popPlot1)

summary(health_data$Pop_2016)

ggsave("pop_2016_line.png", plot = popPlot1, dpi = 300, width = 10, height = 6)

## pop_2017 plot ##
popPlot2 <- ggplot(data = health_data, aes(x = Pop_2017))
print(popPlot2)

popPlot2 <- popPlot2 + geom_freqpoly()
print(popPlot2)

popPlot2 <- popPlot2 +
  xlab("Population in 2017") +
  ylab("Count across cities") +
  ggtitle("Population in 2017 across cities") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))
print(popPlot2)

summary(health_data$Pop_2017)

ggsave("pop_2017_line.png", plot = popPlot2, dpi = 300, width = 10, height = 6)

## popDiff plot ##
popPlot3 <- ggplot(data = health_data, aes(x = popDiff))
print(popPlot3)

popPlot3 <- popPlot3 + geom_histogram(color = "black", fill = "blue")
print(popPlot3)

popPlot3 <- popPlot3 +
  xlab("Population change from 2016 to 2017") +
  ylab("Count across cities") +
  ggtitle("Population change in 2016 to 2017 across cities") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))
print(popPlot3)

summary(health_data$popDiff)

ggsave("popDiff_hist.png", plot = popPlot3, dpi = 300, width = 10, height = 6)

## amenities plot ##
## air quality plot ##
aqPlot <- ggplot(data = health_data, aes(x = AirQuality_v))
print(aqPlot)

aqPlot <- aqPlot + geom_histogram(color = "black", fill = "blue")
print(aqPlot)

aqPlot <- aqPlot +
  xlab("Air Quality") +
  ylab("Count across cities") +
  ggtitle("Air Quiality across cities") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12))
print(aqPlot)

ggsave("aq_cities_hist.png", plot = aqPlot, dpi = 300, width = 10, height = 6)

## greenness plot
greennessPlot <- ggplot(data = health_data, aes(x = Greenness_v))
print(greennessPlot)

greennessPlot <- greennessPlot + geom_histogram(color = "black", fill = "blue")
print(greennessPlot)

greennessPlot <- greennessPlot +
  xlab("Greenness") +
  ylab("Count across cities") +
  ggtitle("Greenness across cities") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))
print(greennessPlot)

ggsave("greenness_cities_hist.png", plot = greennessPlot, dpi = 300, width = 10, height = 6)

## economics plots ##
## income inequaliy plot ##
incomePlot <- ggplot(data = health_data, aes(x = IncomeInequality_v))
print(incomePlot)

incomePlot <- incomePlot + geom_histogram(color = "black", fill = "blue")
print(incomePlot)

incomePlot <- incomePlot +
  xlab("Income Inequality") +
  ylab("Count across cities") +
  ggtitle("Income Inequality across cities") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))
print(incomePlot)

ggsave("incomeInequality_cities_hist.png", plot = incomePlot, dpi = 300, width = 10, height = 6)

## college degree plot ##
collegePlot <- ggplot(data = health_data, aes(x = CollegeDegree_v))
print(collegePlot)

collegePlot <- collegePlot + geom_histogram(color = "black", fill = "blue")
print(collegePlot)

collegePlot <- collegePlot +
  xlab("College Degrees") +
  ylab("Count across cities") +
  ggtitle("College Degrees across cities") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))
print(collegePlot)

ggsave("CollegeDegrees_cities_hist.png", plot = collegePlot, dpi = 300, width = 10, height = 6)


## multiple regression analysis ##
## pop diff dependent with air quality + greenness ## 
a1 <- lm(popDiff ~ scale(AirQuality_v) + scale(Greenness_v), data = health_data)
summary(a1)
## pop diff dependent with air quality + income inequality ## 
a2 <- lm(popDiff ~ scale(AirQuality_v) + scale(IncomeInequality_v), data = health_data)
summary(a2)
## pop diff dependent with air quality + college degree ## 
a3 <- lm(popDiff ~ scale(AirQuality_v) + scale(CollegeDegree_v), data = health_data)
summary(a3)
## pop diff dependent with greenness + income inequality ## 
a4 <- lm(popDiff ~ scale(Greenness_v) + scale(IncomeInequality_v), data = health_data)
summary(a4)
## pop diff dependent with greenness + college degree ## 
a5 <- lm(popDiff ~ scale(Greenness_v) + scale(CollegeDegree_v), data = health_data)
summary(a5)
## pop diff dependent with income inequality + college degree
a6 <- lm(popDiff ~ scale(IncomeInequality_v) + scale(CollegeDegree_v), data = health_data)
summary(a6)


## interaction model ##
## turning air quality to dummy variable ##
health_data$AirQualityDummy <- ifelse(health_data$AirQuality_v >= 283.1, 1, 0)

## turning college degree to a dummy variable ##
health_data$CollegeDegreeDummy <- ifelse(health_data$CollegeDegree_v >= 35.92, 1, 0)

## air quality interaction model ##
intModel1 <- lm(popDiff ~ AirQualityDummy + scale(IncomeInequality_v) + AirQualityDummy:scale(IncomeInequality_v), data = health_data)
summary(intModel1)

intModel2 <- lm(popDiff ~ AirQualityDummy + scale(CollegeDegree_v) + AirQualityDummy:scale(CollegeDegree_v), data = health_data)
summary(intModel2)

## college degree interaction model ##
intModel3 <- lm(popDiff ~ CollegeDegreeDummy + scale(Greenness_v) + CollegeDegreeDummy:scale(Greenness_v), data = health_data)
summary(intModel3)

intModel4 <- lm(popDiff ~ CollegeDegreeDummy + scale(AirQuality_v) + CollegeDegreeDummy:scale(AirQuality_v), data = health_data)
summary(intModel4)


stargazer(intModel1, intModel2, intModel3, intModel4, type = "text", out = "interaction_regression_1.txt")
