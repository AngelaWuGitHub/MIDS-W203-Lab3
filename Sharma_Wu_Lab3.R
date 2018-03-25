

install.packages("stargazer")
library(stargazer)

load.libraries <- c('data.table', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr','knitr','reshape2','car','gridExtra','kableExtra','stargazer')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs)
sapply(load.libraries, require, character = TRUE)

S = read.csv("/Users/bhuvneshsharma/bhuvnesh/personal/Education/UCBerkeley/W203/Download_WK11/crime_v2.csv")
summary(S)

# convert numeric fields to the numeric type
convert_to_numeric = function(col) {
  return(as.numeric(sub("[\\$%,]","", col)))
}


setwd("/Users/bhuvneshsharma/bhuvnesh/personal/Education/UCBerkeley/W203/Download_WK11/")
library(dplyr)
library(car)
convert_to_numeric = function(col) {
  return(as.numeric(sub("[\\$%,]","", col)))
}

crimeData = read.table("crime_v2.csv",header = TRUE, sep = ",", stringsAsFactors = FALSE)

summary(crimeData)

length(crimeData)

crimeData2 <- crimeData[complete.cases(crimeData),]
length(crimeData2)

# Convert all the columns to numeric
NON_ID_DATA_START <- 1 # columns 1 and 2 are the country and country code data
crimeData2[NON_ID_DATA_START:length(crimeData2)] = lapply(crimeData2[NON_ID_DATA_START:length(crimeData2)], convert_to_numeric)

summary(crimeData2)
summary(crimeData2$prbconv)

crimeData2 <- transform(crimeData2, prbconv = as.numeric(as.character(prbconv)))
summary(crimeData2$prbconv)


# number of rows that prbarr > 1 
nrow(crimeData2[which(crimeData2$prbarr>1),])
# number of rows that prbconv > 1 
nrow(crimeData2[which(crimeData2$prbconv>1),])
# number of rows that prbconv > 1 
nrow(crimeData2[which(crimeData2$prbpris>1),])

crimeData2$pctmin80_2 <- crimeData2$pctmin80/100
summary(crimeData2$year)
hist(crimeData2$crmrte)

# Possibily look at crime rates w.r.t every thousand people in the county
crimeData2$crmrte_2 <- crimeData2$crmrte*1000
# Possibily look at police per capita  w.r.t every thousand people in the county
crimeData2$polpc_2 <- crimeData2$polpc*1000

summary(crimeData2$crmrte_2)
summary(crimeData2$prbarr)
summary(crimeData2$prbconv)
summary(crimeData2$prbpris)
summary(crimeData2$avgsen)
summary(crimeData2$polpc_2)
summary(crimeData2$density)
summary(crimeData2$taxpc)
summary(crimeData2$west)
summary(crimeData2$central)
summary(crimeData2$urban)
summary(crimeData2$pctmin80)


crimeData2_drop <- c("year") # extra column
crimeData3 = crimeData2[ , !(names(crimeData2) %in% crimeData2_drop)]
correlations <- cor(na.omit(crimeData3[,-1,]))

# correlations
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")

# Delete county with extreme wage number
crimeData2 <- crimeData2[which(crimeData2$wser<2000),]

# Average Sentencing not corelating with Crime rate
# Crime and policepercent has a very week relationship , almost not there
# Tax and police has week relationship
# Crime rate has high correlation with Density and urban
# western counties have negative corelation with crime rate
# crime rate has positive correlationship with weekly wages.


# Crime rate correlates with Arrests but does not correlates with police , how to explain this ? 
# if we have similar arrest and convition rates between 2 counties ,  police and  crime rate are positively correlated.
# Need to see how effective police is rather than number of police
# Crime rate positively corelated with taxPC
# Crime rate positively corelated with weekly wage index

cor(crimeData2$polpc , crimeData2$prbarr)
cor(crimeData2$wser , crimeData2$crmrte)



lm(crmrte ~ polpc+prbarr+prbconv+density, data=crimeData2)

plotCorr <- function(data_in, i){
  data <- data.frame(x = data_in[[i]], crmrte = data_in$crmrte)
  p <- ggplot(data, aes(x = x, y = crmrte)) + geom_point(shape = 1, na.rm = TRUE) + geom_smooth(method = lm ) + xlab(paste0(colnames(data_in)[i], '\n', 'R-Squared: ', round(cor(data_in[[i]], data$crmrte, use = 'complete.obs'), 2))) + theme_light()
  return(suppressWarnings(p))
}

highcorr <- c(names(correlations[,'crmrte'])[which(correlations[,'crmrte'] > 0.3)], names(correlations[,'crmrte'])[which(correlations[,'crmrte'] < -0.3)])
data_corr <- crimeData3[,highcorr,]
doPlots(data_corr, fun = plotCorr, ii = 1:14)


# what is the definition of crime
hist(crimeData2$crmrte_2,xlab=" Crime Rate ",main = 'Histogram of Crime Rate',col = "blue")
# has 1 record which is extreme and greater than 1
hist(crimeData2$prbarr,xlab="Ratio of # arrest to # of offense ",main = 'Histogram of Arrests to Offense',col = "blue")
hist(crimeData2$prbconv,xlab="Ratio of # conviction to # of arrest ",main = 'Histogram of Ratio of Conviction to Arrest',col = "purple")
hist(crimeData2$prbpris,xlab="Ratio of # arrest to # of offense ", main = 'Histogram of Arrests to Offense', col = "green")
hist(crimeData2$avgsen,xlab=" Number of days sentenced for", main = 'Number of days Sentenced', col = "royalblue1")

# Seems there is 1 county which has 5 times the mean
# Is this an highly leveraged ??
hist(crimeData2$polpc_2,xlab=" Police per 1000 people ", main = 'Police per 1000 people', col = "red")

hist(crimeData2$polpc,xlab=" Police per capita ", main = 'Police per 1000 people', col = "red")
hist(crimeData2$density,xlab=" People per square mile ", main = 'People per square mile', col = "royalblue1")

# Seems there is 1 county which has almost 4 times the mean , 3 times 3rd quart. Need to check if there is any relation with this variable and police per .
# Is this an highly leveraged ??
hist(crimeData2$taxpc,xlab=" Tax revenue per capita ", main = 'Tax revenue per capita', col = "purple")
# Dont think there  these variables 
#hist(crimeData2$west)
#hist(crimeData2$central)
#hist(crimeData2$urban)

hist(crimeData2$pctmin80,xlab=" Percent Minority in 1980 ", main = 'Percent Minority in 1980', col = "green")


ggplot(data = crimeData2, mapping = aes(x = crimeData2$polpc_2, y = crimeData2$taxpc)) +
  geom_boxplot()


ggplot(data = crimeData2, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

prbconv
prbpris
avgsen
polpc
density
taxpc
west
central
urban
pctmin80





scatterplotMatrix(~ crmrte+wcon+wtuc, data=crimeData2, diagonal="histogram")
scatterplotMatrix(~ crmrte+wtrd+wfir, data=crimeData2, diagonal="histogram")
scatterplotMatrix(~ crmrte+wser+wmfg, data=crimeData2, diagonal="histogram")
scatterplotMatrix(~ crmrte+wser+wmfg, data=crimeData3, diagonal="histogram")
scatterplotMatrix(~ crmrte+wfed+wsta+wloc, data=crimeData2, diagonal="histogram")
scatterplotMatrix(~ crmrte+mix+pctymle, data=crimeData2, diagonal="histogram")

