# Title: Create regression lines with fixed endpoints
# Author: Nadja   Date: 05.05.2014

# Short description: 
# This script introduces an easy and flexible way to plot regression lines with defined ending points

# List all libraries that are used
# No special libraries needed

# par()              # view current settings
opar <- par()  # copy default settings

# We use the cat dataset from library MASS
library(MASS)
catdata<-cats
pairs(catdata) # Bwt and Hwt are correlated but the range of values is different for males and females
plot(catdata$Bwt~catdata$Hwt,col=as.factor(catdata$Sex))

# We create a seperate dataset for each sex
Males<-catdata[catdata$Sex=="M",]
Females<-catdata[catdata$Sex=="F",]

# But we stil want to plot them together in one graph with two regression lines
plot(Males$Bwt~Males$Hwt,col="blue",ylab="Bwt",xlab="Hwt")
points(Females$Bwt~Females$Hwt,col="red")
abline(lm(Males$Bwt~Males$Hwt),col="blue")
abline(lm(Females$Bwt~Females$Hwt),col="red")
# But the regression lines span the whole range of x-values

# We create regression lines as lines with defined end-points spanning the min and max values for each sex
summary(Males)
summary(Females)

tiff(filename = "vc_example_graph.tiff",width = 20,height = 20,units = cm)

plot(Males$Bwt~Males$Hwt,
     xlim=c(5,25),ylim=c(2,4), # here we set the limits based on the overall values
     col="#92c5de",
     xlab="Hwt",ylab="Bwt")
points(Females$Bwt~Females$Hwt,col="#f4a582")
# Now we use the min and max x-values for each sex
lines(c(min(Males$Hwt),max(Males$Hwt)),c(min(Males$Hwt)*as.numeric(coefficients(lm(Males$Bwt~Males$Hwt))[2],2)+as.numeric(coefficients(lm(Males$Bwt~Males$Hwt))[1],2),
                                         max(Males$Hwt)*as.numeric(coefficients(lm(Males$Bwt~Males$Hwt))[2],2)+as.numeric(coefficients(lm(Males$Bwt~Males$Hwt))[1],2)),
      col="#0571b0",lwd=2,lty=1)
lines(c(min(Females$Hwt),max(Females$Hwt)),c(min(Females$Hwt)*as.numeric(coefficients(lm(Females$Bwt~Females$Hwt))[2],2)+as.numeric(coefficients(lm(Females$Bwt~Females$Hwt))[1],2),
                                         max(Females$Hwt)*as.numeric(coefficients(lm(Females$Bwt~Females$Hwt))[2],2)+as.numeric(coefficients(lm(Females$Bwt~Females$Hwt))[1],2)),
      col="#ca0020",lwd=2,lty=1)

dev.off()

par(opar) # set parameters to default
