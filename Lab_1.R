# Getting data in
install.packages("Rcmdr")
library(Rcmdr)
library(readr)
EPI_data <- read_csv("~/Desktop/2010EPI_data.csv")
View(EPI_data)
attach(EPI_data) 	# sets the ‘default’ object
fix(EPI_data) 	# launches a simple data editor
EPI 			# prints out values EPI_data$EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array
E


#Exploring the distribution
summary(EPI) #stats
fivenum(EPI, na.rm=TRUE )
stem(EPI) # stem and leaf plot
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI)

#Fitting a distribution beyond histograms

#Cumulative Density Function
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
#Quantile-Quantile?
par(pty="s") 
qqnorm(EPI); qqline(EPI)
#Simulated data from t-distribution:
x <- rt(250, df = 5)
qqnorm(x); qqline(x)
#Make a Q-Q plot against the generating distribution by: x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

#Same exploration and fitting for another two variables in th EPI_data

summary(DALY) #stats
fivenum(DALY, na.rm=TRUE )
stem(DALY) # stem and leaf plot
hist(DALY)
hist(DALY, seq(0., 95.,2.0), prob=TRUE)
lines(density(DALY,na.rm=TRUE,bw=1.))
rug(DALY)
#Cumulative Density Function
plot(ecdf(DALY), do.points=FALSE, verticals=TRUE) 
#Quantile-Quantile?
par(pty="s") 
qqnorm(DALY); qqline(DALY)

summary(WATER_H) #stats
fivenum(WATER_H, na.rm=TRUE )
stem(WATER_H) # stem and leaf plot
hist(WATER_H)
hist(WATER_H, seq(0., 100.,2.0), prob=TRUE)
lines(density(WATER_H,na.rm=TRUE,bw=1.))
rug(WATER_H)
#Cumulative Density Function
plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE) 
#Quantile-Quantile?
par(pty="s") 
qqnorm(WATER_H); qqline(WATER_H)

#Try fitting other distributions...
#Simulated data from t-distribution:
x <- rt(500, df = 5)
qqnorm(x); qqline(x)
#Make a Q-Q plot against the generating distribution by: x<-seq(30,95,1)
qqplot(qt(ppoints(500), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
#Cumulative Density Function
plot(ecdf(x), do.points=FALSE, verticals=TRUE) 

#Comparing distributions
boxplot(EPI,DALY)
qqplot(EPI,DALY)
boxplot(ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H, AIR_E, WATER_E, BIODIVERSITY)
qqplot(ENVHEALTH, ECOSYSTEM)
qqplot(AIR_H, WATER_H)

#Filtering(poplulations)

#Conditional filtering:
EPILand <- EPI[!Landlock]
EPILand
Eland <- EPILand[!is.na(EPILand)]
Eland
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)

#Repeat exercise 1...

#Exploring the distribution
summary(Eland) #stats
fivenum(Eland, na.rm=TRUE )
stem(Eland) # stem and leaf plot
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
lines(density(Eland,na.rm=TRUE,bw=1.))
rug(Eland)

#Cumulative Density Function
plot(ecdf(Eland), do.points=FALSE, verticals=TRUE) 
#Quantile-Quantile?
par(pty="s") 
qqnorm(Eland); qqline(Eland)

#Other conditional filtering...
EPI_SubSaharan_Africa <- EPI[EPI_regions == "Sub-Saharan Africa"]
ESSA <- EPI_SubSaharan_Africa[!is.na(EPI_SubSaharan_Africa)]

#Repeat above processes for GPW3_GRUMP data...
GRUMP_data <- read_csv("~/Desktop/GPW3_GRUMP_SummaryInformation_2010.csv")
View(GRUMP_data)
attach(GRUMP_data) 	# sets the ‘default’ object

#Exploring the distribution
summary(Resolution) #stats
fivenum(Resolution, na.rm=TRUE )
stem(Resolution) # stem and leaf plot
hist(Resolution)
hist(Resolution, seq(30., 95., 1.0), prob=TRUE)
lines(density(Resolution,na.rm=TRUE,bw=1.))
rug(Resolution)

#Fitting a distribution beyond histograms

#Cumulative Density Function
plot(ecdf(Resolution), do.points=FALSE, verticals=TRUE) 
#Quantile-Quantile?
par(pty="s") 
qqnorm(Resolution); qqline(Resolution)

#Comparing distributions
boxplot(`P90E ('000)`,`P95E ('000)`)
qqplot(`P90E ('000)`,`P95E ('000)`)

#Filtering

#Conditional filtering:
Resolution_Asia <- Resolution[ContinentName=="Asia"]
Resolution_Asia 
RA <- Resolution_Asia[!is.na(Resolution_Asia )]
RA
hist(RA)
hist(RA, seq(0., 400., 10.0), prob=TRUE)

#Repeat above processes for water_treatment data...

water_treatment <- read_csv("~/Desktop/water-treatment.csv")
View(water_treatment)
install.packages("stringr")               # Install stringr package
library("stringr")                        # Load stringr package
# Remove the hyphen in the column names
names(water_treatment) <- str_replace_all(names(water_treatment), c("-" = ""))
View(water_treatment)
attach(GRUMP_data) 	# sets the ‘default’ object
#Exploring the distribution
summary(water_treatment) #stats
summary(water_treatment$CONDE) #stats
fivenum(water_treatment$CONDE, na.rm=TRUE )
stem(water_treatment$CONDE) # stem and leaf plot
hist(water_treatment$CONDE)
hist(water_treatment$CONDE, seq(650., 3250., 20.0), prob=TRUE)
lines(density(water_treatment$CONDE,na.rm=TRUE,bw=5.))
rug(water_treatment$CONDE)

#Fitting a distribution beyond histograms

#Cumulative Density Function
plot(ecdf(water_treatment$CONDE), do.points=FALSE, verticals=TRUE) 
#Quantile-Quantile?
par(pty="s") 
qqnorm(water_treatment$CONDE); qqline(water_treatment$CONDE)

#Comparing distributions
boxplot(water_treatment$CONDE,water_treatment$CONDD)
qqplot(water_treatment$CONDE,water_treatment$CONDD)

#Filtering

#Conditional filtering:
CONDE_greaterPHE <- water_treatment$CONDE["PHE" > 7.8]
CONDE_greaterPHE 
CgP <- CONDE_greaterPHE [!is.na(CONDE_greaterPHE )]
CgP
hist(CgP)
hist(CgP, seq(650., 3250., 20.0), prob=TRUE)
