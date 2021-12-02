# # Preparation # not to run
# dat = read.csv('DataFitting.csv')
# dat$Sizerange = round(log10(dat$Size))
# 
# speed = read.csv('Speed.csv')
# speed$Sizerange = round(log10(speed$Mass))
# 
# write.csv(dat,'DataFitting.csv',quote = F, row.names = F)
# write.csv(speed, 'Speed.csv', quote = F, row.names = F)

#### fitting ####
setwd("~/Master_Uottawa/Functional_Response/Manuscript/FEE_Paper/Manuscript/Review/Fitting")
dat = read.csv('DataFitting.csv',colClasses = c('numeric','numeric','numeric','factor','factor'))

speed = read.csv('Speed.csv',colClasses = c('numeric','numeric','numeric','factor'))

capture = subset(dat,dat$Trait=='Probability')
capture = droplevels(capture)
attack = subset(dat,dat$Trait=='Attack')
attack = droplevels(attack)
toto = which(attack$Predicted==0)
attack = attack[-toto,]
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

speed$speedmod = log10(speed$Predicted) - log10(speed$Observed)
attack$attackmod = log10(attack$Predicted) - log10(attack$Observed)
handling$handlingmod = log10(handling$Predicted) - log10(handling$Observed)
capture$capturemod = capture$Predicted - capture$Observed

mod1 = lm(speedmod~log10(Predicted)*LogSize,data=speed)
summary(mod1)

mod2 = lm(attackmod~log10(Predicted)*LogSize,data=attack)
summary(mod2)

mod3 = lm(handlingmod~log10(Predicted)*LogSize,data=handling)
summary(mod3)

mod4 = lm(capturemod~ capture$Predicted*LogSize,data=capture)
summary(mod4)

mod5 = lm(capturemod~ capture$Predicted,data=capture)
summary(mod5)


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

#### data for lmm ####
## Prepare data
setwd("~/Master_Uottawa/Functional_Response/Data/Li_2017")
li = read.csv('Lietal_oikos_2017_data.csv')
dat=subset(li,li$predation.strategy=='predator')
dat=droplevels(dat)
water=subset(dat,dat$ecosystem.type!='terrestrial')
water=droplevels(water)
water=water[-150,]
rm(li)

setwd("~/Master_Uottawa/Functional_Response/Data")
dat=read.csv('Portalier_et_al_2021_Predator_Prey_Interactions.csv')

attack = subset(dat,dat$predation.trait=='attack.rate')
attack = droplevels(attack)
handling = subset(dat,dat$predation.trait=='handling.time')
handling = droplevels(handling)
capture = subset(dat,dat$predation.trait=='capture.probability')
capture = droplevels(capture)

ref = water$original.publication.short
references = rep(0,length(ref))
originalvec = levels(as.factor(ref))
refvect = c('Anderson_et_al.1978','Bailey.1989','Barnhinsel_and_Kerfoot.2004','Bergstrom_and_Englund.2004','Buckel_and_Stoner.2000',
            'Butler_and_Burns.1993','Colton.1987','Cothran_and_Thorp.1985','DeBlois_and_Leggett.1991',
            'Fox_and_Murdoch.1978','Galarowicz_and_Wahl.2005','Gresens_et_al.1982','Houde_and_Schekter.1980',
            'Koski_and_Johnson.2002','Ljunggren_and_Sandstrom.2007','McKee_et_al.1997','Miller_et_al.1992',
            'Moss_and_Beauchamp.2007','Spitze.1985','Taylor_and_Collie.2003','Thompson.1975','Thompson.1978',
            'Vinyard_and_Menger.1980')

for (i in 1:length(ref)){
  index = which(originalvec==ref[i])
  references[i] = refvect[index]
}

names(attack)[9] = 'meta.analysis'
attack$references = references
attack = attack[,c(1,2,3,4,5,6,7,8,10,9)]

names(handling)[9] = 'meta.analysis'
handling$references = references
handling = handling[,c(1,2,3,4,5,6,7,8,10,9)]

capture$meta.analysis = NA

toto = rbind(attack, handling, capture)

setwd("~/Master_Uottawa/Functional_Response/Data")
write.csv(toto,'Portalier_et_al_2021_Predator_Prey_Interactions.csv',quote = F, row.names = F)

dat = read.csv('Portalier_et_al_2021_Predator_Prey_Interactions.csv')
datfit = read.csv('DataFitting.csv')
datfit$Study = dat$references
write.csv(datfit,'DataFitting.csv',quote = F, row.names = F)

#### lmm ####
# library(nlme)
library(lme4)

setwd("~/Master_Uottawa/Functional_Response/Manuscript/FEE_Paper/Manuscript/Review/Fitting")
dat = read.csv('DataFitting.csv',colClasses = c('numeric','numeric','numeric','factor','factor','factor'))
speed = read.csv('Speed.csv',colClasses = c('numeric','numeric','numeric','factor'))

speed5 = which(speed$LogSize==5)
speed$LogSize[speed5] = 4
speed = droplevels(speed)

capture = subset(dat,dat$Trait=='Probability')
capture = droplevels(capture)
attack = subset(dat,dat$Trait=='Attack')
attack = droplevels(attack)
toto = which(attack$Predicted==0)
attack = attack[-toto,]
handling = subset(dat,dat$Trait=='Handling')
handling = droplevels(handling)
rm(toto)

speed$log10pred_log10obs = log10(speed$Predicted) - log10(speed$Observed)
attack$log10pred_log10obs = log10(attack$Predicted) - log10(attack$Observed)
handling$log10pred_log10obs = log10(handling$Predicted) - log10(handling$Observed)
capture$pred_obs = capture$Predicted - capture$Observed

mod1 = lm(log10pred_log10obs~log10(Predicted)*LogSize,data=speed)
summary(mod1)
mod1co = mod1$coefficients
index1 = seq(1,20,by = 2)
index2 = seq(2,20, by = 2)
intercept1 = mod1co[index1]
slope1 = mod1co[index2]
colo = rainbow(10)

mod2 = lmer(log10pred_log10obs~log10(Predicted)*LogSize+(1|Study),data=attack)
mod22 = lmer(log10pred_log10obs~log10(Predicted)*LogSize+(1|Study),data=attack,REML = F)
mod23 = lmer(log10pred_log10obs~log10(Predicted)+(1|Study),data=attack,REML = F)
#pvals.fnc(mod2)
summary(mod2)
print(mod2, corr = T)
anova(mod22,mod23)

mod3 = lmer(log10pred_log10obs~log10(Predicted)*LogSize+(1|Study),data=handling)
summary(mod3)

mod5 = lm(pred_obs~ Predicted,data=capture)
summary(mod5)

#### clean code ####
library(lattice)
library(latticeExtra)
library(lme4)

setwd("~/Master_Uottawa/Functional_Response/Manuscript/FEE_Paper/Manuscript/Review/Fitting")
speed = read.csv('Speed.csv',colClasses = c('numeric','numeric','numeric','factor'))

speed5 = which(speed$LogSize==5)
speed$LogSize[speed5] = 4
speed5 = which(speed$LogSize==-5)
speed$LogSize[speed5] = -4
speed = droplevels(speed)

speed$log10pred_log10obs = log10(speed$Predicted) - log10(speed$Observed)

mod1 = lm(log10pred_log10obs~log10(Predicted)*LogSize,data=speed)
summary(mod1)

dat = read.csv('DataFitting.csv',colClasses = c('numeric','numeric','numeric','factor','factor','factor'))
capture = subset(dat,dat$Trait=='Probability')
capture = droplevels(capture)
attack = subset(dat,dat$Trait=='Attack')
attack = droplevels(attack)
toto = which(attack$Predicted==0)
attack = attack[-toto,]
handling = subset(dat,dat$Trait=='Handling')
handling = droplevels(handling)

attack$log10pred_log10obs = log10(attack$Predicted) - log10(attack$Observed)
handling$log10pred_log10obs = log10(handling$Predicted) - log10(handling$Observed)
capture$pred_obs = capture$Predicted - capture$Observed

# RMSD
titi = (speed$Predicted - speed$Observed)^2
n = length(titi)-1
rmsd1 = sqrt(1/n*sum(titi))

titi = (attack$Predicted - attack$Observed)^2
n = length(titi)-1
rmsd2 = sqrt(1/n*sum(titi))

titi = (handling$Predicted - handling$Observed)^2
n = length(titi)-1
rmsd3 = sqrt(1/n*sum(titi))

titi = (capture$Predicted - capture$Observed)^2
n = length(titi)-1
rmsd4 = sqrt(1/n*sum(titi))

# models
mod2 = lmer(log10pred_log10obs~log10(Predicted)*LogSize+(1|Study),data=attack)
summary(mod2)

mod2 = lm(log10pred_log10obs~log10(Predicted)*LogSize,data=attack)
summary(mod2)

mod3 = lmer(log10pred_log10obs~log10(Predicted)*LogSize+(1|Study),data=handling)
summary(mod3)

mod3 = lm(log10pred_log10obs~log10(Predicted)*LogSize,data=handling)
summary(mod3)

mod35 = lm(log10pred_log10obs~log10(Predicted)*LogSize,data=handling)
summary(mod35)

mod5 = lm(pred_obs~ Predicted,data=capture)
summary(mod5)

mod6 = lmer(pred_obs~ Predicted*LogSize+(1|Study),data=capture)
summary(mod6)

mod7 = lm(pred_obs~ Predicted*LogSize,data=capture)
summary(mod7)

mod8 = lmer(pred_obs~ Predicted+(1|Study),data=capture)
summary(mod8)

colo = rainbow(10)
mod1co = mod1$coefficients
slope1 = mod1co[seq(2,20, by = 2)]
inter1 = mod1co[seq(1,20, by = 2)]

par(mar=c(5,5,4,1),mfrow = c(2,2))
plot(log10(speed$Observed)~log10(speed$Predicted),xlab='Predicted',ylab='Observed',main = 'Speed',cex.lab=1.5,
     ylim = c(-1.2,1.6),xlim = c(-1.2,1.6))
abline(a = 0, b = 1,lwd=3)
for (i in 1:10){
  abline(a = 0, b = slope1[i], col = colo[i], lwd = 2.5)
}

plot(log10(speed$Observed)~log10(speed$Predicted),xlab='Predicted',ylab='Observed',main = 'Speed',cex.lab=1.5,
     ylim = c(-1.2,1.6),xlim = c(-1.2,1.6))
abline(a = 0, b = 1,lwd=3)
for (i in 1:10){
  abline(a = inter1[i], b = 1, col = colo[i], lwd = 2.5)
}

plot(log10(speed$Observed)~log10(speed$Predicted),xlab='Predicted',ylab='Observed',main = 'Speed',cex.lab=1.5,
     ylim = c(-1.2,1.6),xlim = c(-1.2,1.6))
abline(a = 0, b = 1,lwd=3)
for (i in 1:10){
  abline(a = inter1[i], b = slope1[i], col = colo[i], lwd = 2.5)
}

setwd("~/Master_Uottawa/Functional_Response/Manuscript/FEE_Paper/Manuscript/Review/Fitting")
png('Speedfit.png')
par(mar=c(5,5,4,1))
plot(log10(speed$Observed)~log10(speed$Predicted),xlab='Predicted',ylab='Observed',cex.lab=2.5,
     ylim = c(-1.2,1.6),xlim = c(-1.2,1.6))
abline(a = 0, b = 1,lwd=3)
dev.off()

png('Attackfit.png')
par(mar=c(5,5,4,1))
plot(log10(attack$Observed)~log10(attack$Predicted),xlab='Predicted',ylab='Observed',cex.lab=2.5,
     xlim = c(-12.5,-3), ylim = c(-12.5,-3))
abline(a = 0, b = 1,lwd=3)
dev.off()

png('Handlingfit.png')
par(mar=c(5,5,4,1))
plot(log10(handling$Observed)~log10(handling$Predicted),xlab='Predicted',ylab='Observed',cex.lab=2.5,
     xlim = c(-5,7), ylim = c(-5,7))
abline(a = 0, b = 1,lwd=3)
dev.off()

png('Capturefit.png')
par(mar=c(5,5,4,1))
plot(capture$Observed~capture$Predicted,xlab='Predicted',ylab='Observed',cex.lab=2.5,
     xlim = c(0,1))
abline(a = 0, b = 1,lwd=3)
dev.off()


## plots multi lines 
coefmod1 = mod1$coefficients
slope1n = coefmod1[c(2,seq(11,18))]
intercept1n = coefmod1[c(1,seq(3,10))]
slope1 = rep(0,length(slope1n))
intercept1 = rep(0,length(intercept1n))
for (i in 1:length(slope1n)){
  slope1[i] = slope1n[[i]]
  intercept1[i] = intercept1n[[i]]
}
rm(slope1n,intercept1n)

sl1 = slope1[1]
int1 = intercept1[1]
slope1 = sl1 - slope1 #sl1 - slope1
intercept1 = int1 - intercept1 #int1 - intercept1
intercept1[9] = intercept1[9]+0.5
intercept1[8] = intercept1[8]+0.5
intercept1[7] = intercept1[7]+0.25
intercept1[6] = intercept1[6]+0.25
intercept1[5] = intercept1[5]+0.25

#colo = rainbow(10)
#colo = heat.colors(10)
colo = c(palette.colors(),'grey30')
sizes = c(-1,-2,-3,-4,0,1,2,3,4)

par(mar=c(5,5,4,1))
plot(log10(speed$Observed)~log10(speed$Predicted),xlab='Predicted',ylab='Observed',cex.lab=1.5,
     ylim = c(-1.2,1.6),xlim = c(-1.2,1.6))
abline(a = 0, b = 1,lwd=3)
for (i in 2:length(sizes)){
  #abline(a = intercept1[i], b = slope1[i], col = colo[i], lwd = 2.5)
  dodo = speed[speed$LogSize==sizes[i],]
  points(log10(dodo$Observed)~log10(dodo$Predicted),col=colo[i])
  x0 = dodo$Predicted
  y0 = intercept1[i] + slope1[i]*log10(x0)
  lines(y0~log10(x0),col = colo[i],lwd=2.5)
}

#svg('OP_Speed.svg',width = 11)
png('OP_Speed.png',width = 800)
par(mar=c(5,5,4,1))
plot(log10(speed$Observed)~log10(speed$Predicted),xlab='Predicted',ylab='Observed',cex.lab=3,
     ylim = c(-1.2,1.6),xlim = c(-1.2,1.6))
abline(a = 0, b = 1,lwd=3.5)
for (i in 2:length(sizes)){
  #abline(a = intercept1[i], b = slope1[i], col = colo[i], lwd = 2.5)
  dodo = speed[speed$LogSize==sizes[i],]
  points(log10(dodo$Observed)~log10(dodo$Predicted),col=colo[i])
  x0 = dodo$Predicted
  y0 = intercept1[i] + slope1[i]*log10(x0)
  lines(y0~log10(x0),col = colo[i],lwd=3)
}
dev.off()


coefmod2 = mod2$coefficients
slope2n = coefmod2[c(2,seq(9,14))]
intercept2n = coefmod2[c(1,seq(3,8))]
slope2 = rep(0,length(slope2n))
intercept2 = rep(0,length(slope2n))
for (i in 1:length(slope2n)){
  slope2[i] = slope2n[[i]]
  intercept2[i] = intercept2n[[i]]
}
rm(slope2n,intercept2n)

sl2 = slope2[1]
int2 = intercept2[1]
slope2 = sl2 - slope2
intercept2 = int2 - intercept2
intercept2[2] = intercept2[2]-3
intercept2[3] = intercept2[3]-2
#intercept2[4] = intercept2[4]-1
intercept2[5] = intercept2[5]+2
intercept2[6] = intercept2[6]+4
intercept2[7] = intercept2[7]+5

#colo = rainbow(10)
#colo = heat.colors(10)
colo = palette.colors(7)
sizes = c(-1,-2,-3,-4,-5,-6,-7)

#svg('OP_Attack.svg',width = 11)
png('OP_Attack.png',width = 800)
par(mar=c(5,5,4,1))
plot(log10(attack$Observed)~log10(attack$Predicted),xlab='Predicted',ylab='Observed',cex.lab=3,
     xlim = c(-12.5,-3), ylim = c(-12.5,-3))
abline(a = 0, b = 1,lwd=3.5)
for (i in 2:length(slope2)){
  #abline(a = intercept1[i], b = slope1[i], col = colo[i], lwd = 2.5)
  dodo = attack[attack$LogSize==sizes[i],]
  points(log10(dodo$Observed)~log10(dodo$Predicted),col=colo[i])
  x0 = dodo$Predicted
  y0 = intercept2[i] + slope2[i]*log10(x0)
  lines(y0~log10(x0),col = colo[i],lwd=3)
}
dev.off()

coefmod2 = mod3$coefficients
slope2n = coefmod2[c(2,seq(9,14))]
intercept2n = coefmod2[c(1,seq(3,8))]
slope2 = rep(0,length(slope2n))
intercept2 = rep(0,length(slope2n))
for (i in 1:length(slope2n)){
  slope2[i] = slope2n[[i]]
  intercept2[i] = intercept2n[[i]]
}
rm(slope2n,intercept2n)

sl2 = slope2[1]
int2 = intercept2[1]
slope2 = sl2 - slope2
intercept2 = int2 - intercept2

#colo = rainbow(10)
#colo = heat.colors(10)
colo = palette.colors(7)
colo[1] = 'grey50'
sizes = c(-1,-2,-3,-4,-5,-6,-7)

png('OP_Handling.png',width = 800)
par(mar=c(5,5,4,1))
plot(log10(handling$Observed)~log10(handling$Predicted),xlab='Predicted',ylab='Observed',cex.lab=3,
     xlim = c(-5,7), ylim = c(-5,7))
abline(a = 0, b = 1,lwd=3.5)
for (i in 1:7){
  #abline(a = intercept1[i], b = slope1[i], col = colo[i], lwd = 2.5)
  dodo = handling[handling$LogSize==sizes[i],]
  points(log10(dodo$Observed)~log10(dodo$Predicted),col=colo[i])
  x0 = dodo$Predicted
  y0 = intercept2[i] + slope2[i]*log10(x0)
  lines(y0~log10(x0),col = colo[i],lwd=3)
}
dev.off()

