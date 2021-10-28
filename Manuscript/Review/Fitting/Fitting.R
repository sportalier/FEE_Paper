#### fitting ####
setwd("~/Master_Uottawa/Functional_Response/Fitting")
dat = read.csv('DataFitting.csv')
speed = read.csv('Speed.csv')
capture = subset(dat,dat$Trait=='Probability')
capture = droplevels(capture)
attack = subset(dat,dat$Trait=='Attack')
attack = droplevels(attack)
handling = subset(dat,dat$Trait=='Handling')
handling = droplevels(handling)

par(mar=c(5,5,4,1))
plot(log10(attack$Observed)~log10(attack$Predicted),xlab='Predicted',ylab='Observed',main = 'Attack rate',cex.lab=2)
abline(a = 0, b = 1,lwd=3)

par(mar=c(5,5,4,1))
plot(log10(handling$Observed)~log10(handling$Predicted),xlab='Predicted',ylab='Observed',main = 'Handling time',cex.lab=2)
abline(a = 0, b = 1,lwd=3)

par(mar=c(5,5,4,1))
plot(capture$Observed~capture$Predicted,xlab='Predicted',ylab='Observed',main = 'Capture probability',cex.lab=2)
abline(a = 0, b = 1,lwd=3)

par(mar=c(5,5,4,1))
plot(log10(speed$Observed)~log10(speed$Predicted),xlab='Predicted',ylab='Observed',main = 'Speed',cex.lab=2)
abline(a = 0, b = 1,lwd=3)

png('Figures.png',height = 480, width = 800)
par(mar=c(5,5,4,1),mfrow=c(2,2))
plot(log10(speed$Observed)~log10(speed$Predicted),xlab='Predicted',ylab='Observed',main = 'Speed',cex.lab=2)
abline(a = 0, b = 1,lwd=3)
plot(log10(attack$Observed)~log10(attack$Predicted),xlab='Predicted',ylab='Observed',main = 'Attack rate',cex.lab=2)
abline(a = 0, b = 1,lwd=3)
plot(capture$Observed~capture$Predicted,xlab='Predicted',ylab='Observed',main = 'Capture probability',cex.lab=2)
abline(a = 0, b = 1,lwd=3)
plot(log10(handling$Observed)~log10(handling$Predicted),xlab='Predicted',ylab='Observed',main = 'Handling time',cex.lab=2)
abline(a = 0, b = 1,lwd=3)
dev.off()

speedmod = log10(speed$Predicted) - log10(speed$Observed)
attackmod = log10(attack$Predicted) - log10(attack$Observed)
handlingmod = log10(handling$Predicted) - log10(handling$Observed)
capturemod = capture$Predicted - capture$Observed

mod1 = lm(speedmod~log10(speed$Predicted))
summary(mod1)

mod2 = lm(attackmod~log10(attack$Predicted))
summary(mod2)

mod3 = lm(handlingmod~log10(handling$Predicted))
summary(mod3)

mod4 = lm(capturemod~ capture$Predicted)
summary(mod4)


speedmod2 = speed$Predicted - speed$Observed
attackmod2 = attack$Predicted - attack$Observed
handlingmod2 = handling$Predicted - handling$Observed

mod26=lm(speedmod2~speed$Predicted)
summary(mod26)

mod27=lm(attackmod2~attack$Predicted)
summary(mod27)

mod28=lm(handlingmod2~handling$Predicted)
summary(mod28)

# RMSD
titi = (speed$Predicted - speed$Observed)^2
rmsd1 = sqrt(1/length(speedmod)*sum(titi))

titi = (attack$Predicted - attack$Observed)^2
rmsd2 = sqrt(1/length(speedmod)*sum(titi))

titi = (handling$Predicted - handling$Observed)^2
rmsd3 = sqrt(1/length(speedmod)*sum(titi))

titi = (capture$Predicted - capture$Observed)^2
rmsd4 = sqrt(1/length(speedmod)*sum(titi))
