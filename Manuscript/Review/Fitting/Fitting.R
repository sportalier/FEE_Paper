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

#### fitting lattice ####
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

mod2 = lmer(log10pred_log10obs~log10(Predicted)*LogSize+(1|Study),data=attack)

mod2 = lm(log10pred_log10obs~log10(Predicted)*LogSize,data=attack)

mod3 = lmer(log10pred_log10obs~log10(Predicted)*LogSize+(1|Study),data=handling)

mod3 = lm(log10pred_log10obs~log10(Predicted)*LogSize,data=handling)

mod35 = lm(log10pred_log10obs~log10(Predicted)*LogSize,data=handling)

mod5 = lm(pred_obs~ Predicted,data=capture)


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

colo = c(palette.colors(),'grey30')
sizes = c(-1,-2,-3,-4,0,1,2,3,4)

speedlist = list()
for (i in 1:9){
  dodo = speed[speed$LogSize==sizes[i],]
  x0 = dodo$Predicted
  y0 = intercept1[i] + slope1[i]*log10(x0)
  speedlist[[i]] = data.frame(x0,y0,LogSize=dodo$LogSize)
}
speedlist = do.call('rbind',speedlist)

sizecex = 3
mypanelspeed = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(a = 0, b = 1, lwd=4, col='black')
  panel.text(-1,-1.2,'B',cex=2)
}
speed11 = xyplot(log10(Observed)~log10(Predicted), data = speed, groups = LogSize, pch=19, cex=1.2, col=colo,
                  xlab='Predicted',ylab='Observed', xlim = c(-1.3,1.6), ylim = c(-1.3,1.6),
                  panel = mypanelspeed,
                  par.settings=list(axis.text=list(cex=1.9),par.xlab.text=list(cex=sizecex),par.ylab.text=list(cex=sizecex),
                                    axis.components=list(top=list(tck=0),right=list(tck=0))),
                  scales=list(x=list(at=c(-1,-0.5,0,0.5,1,1.5),labels=c(-1,-0.5,0,0.5,1,1.5)),
                              y=list(at=c(-1,-0.5,0,0.5,1,1.5),labels=c(-1,-0.5,0,0.5,1,1.5))),
)
speed12 = xyplot(y0~log10(x0),data = speedlist, groups = LogSize, type='l',col = colo,lwd=4)
speedplot = speed11+as.layer(speed12)

png('OP_Speed_Lattice.png')
par(mar=c(5,5,4,1))
print(speedplot)
dev.off()

cairo_pdf('OP_Speed_Lattice.pdf')
par(mar=c(5,5,4,1))
print(speedplot)
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

colo = palette.colors(7)
sizes = c(-1,-2,-3,-4,-5,-6,-7)

attacklist = list()
for (i in 1:7){
  dodo = attack[attack$LogSize==sizes[i],]
  x0 = dodo$Predicted
  y0 = intercept2[i] + slope2[i]*log10(x0)
  attacklist[[i]] = data.frame(x0,y0,LogSize=dodo$LogSize)
}
attacklist = do.call('rbind',attacklist)

sizecex = 1.9
mypanelattack = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(a = 0, b = 1, lwd=3, col='black')
  panel.text(-11.25,-12,'D',cex=2)
}
attack11 = xyplot(log10(Observed)~log10(Predicted), data = attack, groups = LogSize, pch=19, cex=0.7, col=colo,
                 xlab='Predicted',ylab='Observed', xlim = c(-12.5,-2.7), ylim = c(-12.5,-2.7),
                 panel = mypanelattack,
                 par.settings=list(axis.text=list(cex=1.8),par.xlab.text=list(cex=sizecex),par.ylab.text=list(cex=sizecex),
                                   axis.components=list(top=list(tck=0),right=list(tck=0))),
                 scales=list(x=list(at=c(-12,-10,-8,-6,-4),labels=c(-12,-10,-8,-6,-4)),
                             y=list(at=c(-12,-10,-8,-6,-4),labels=c(-12,-10,-8,-6,-4))),
)
attack12 = xyplot(y0~log10(x0),data = attacklist, groups = LogSize, type='l',col = colo,lwd=3)
attackplot = attack11+as.layer(attack12)

# png('OP_Attack_lattice.png')
# par(mar=c(5,5,4,1))
# print(attackplot)
# dev.off()
# 
# cairo_pdf('OP_Attack_lattice.pdf')
# par(mar=c(5,5,4,1))
# print(attackplot)
# dev.off()

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

handlist = list()
for (i in 1:7){
  dodo = handling[handling$LogSize==sizes[i],]
  x0 = dodo$Predicted
  y0 = intercept2[i] + slope2[i]*log10(x0)
  handlist[[i]] = data.frame(x0,y0,LogSize=dodo$LogSize)
}
handlist = do.call('rbind',handlist)

colo = palette.colors(7)
colo[1] = 'grey50'
sizes = c(-1,-2,-3,-4,-5,-6,-7)

sizecex = 1.9
mypanelhand = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(a = 0, b = 1, lwd=4, col='black')
  panel.text(-3.4,-4.3,'F',cex=2)
}
phand11 = xyplot(log10(Observed)~log10(Predicted), data = handling, groups = LogSize, pch=19, cex=0.7, col=colo,
                xlab='Predicted',ylab='Observed', xlim = c(-5,7), ylim = c(-5,7),
                panel = mypanelhand,
                par.settings=list(axis.text=list(cex=1.8),par.xlab.text=list(cex=sizecex),par.ylab.text=list(cex=sizecex),
                                  axis.components=list(top=list(tck=0),right=list(tck=0))),
                scales=list(x=list(at=c(-4,-2,0,2,4,6),labels=c(-4,-2,0,2,4,6)),
                            y=list(at=c(-4,-2,0,2,4,6),labels=c(-4,-2,0,2,4,6))),
)
phand12 = xyplot(y0~log10(x0),data = handlist, groups = LogSize, type='l',col = colo,lwd=3)
phand = phand11+as.layer(phand12)

# png('OP_Handling_Lattice.png')
# par(mar=c(5,5,4,1))
# print(phand)
# dev.off()
# 
# cairo_pdf('OP_Handling_Lattice.pdf')
# par(mar=c(5,5,4,1))
# print(phand)
# dev.off()

## capture
sizecex = 1.9
mypanelcap = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(a = 0, b = 1, lwd=4, col='black')
  panel.text(0.15,0.05,'E',cex=2)
}
pcap = xyplot(Observed~Predicted, data = capture, pch=19, cex=0.7, col='black',
                xlab='Predicted',ylab='Observed', xlim = c(-0.02,1.05), ylim = c(-0.02,1.05),
                panel = mypanelcap,
                par.settings=list(axis.text=list(cex=1.8),par.xlab.text=list(cex=sizecex),par.ylab.text=list(cex=sizecex),
                                  axis.components=list(top=list(tck=0),right=list(tck=0))),
                scales=list(x=list(at=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1)),
                            y=list(at=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1))),
                )

# png('OP_Capture_Lattice.png')
# par(mar=c(5,5,4,1))
# print(pcap)
# dev.off()
# 
# cairo_pdf('OP_Capture_Lattice.pdf')
# par(mar=c(5,5,4,1))
# print(pcap)
# dev.off()


# cairo_pdf('Total_Fit_Lattice.pdf')
# par(mar=c(5,5,4,1))
# print(attackplot,position=c(0,0,0.98,0.98),split=c(1,1,2,2),more=T)
# print(pcap,position=c(0.5,0,1.5,0.98),split=c(1,1,2,2),more=T)
# print(phand,position=c(0.0,0,1,0.98),split=c(1,2,2,2),more=F)
# dev.off()

#### total plots ####
sizecex = 1.9
setwd("~/Master_Uottawa/Functional_Response/Data/Li_2017")

aquatic = read.csv('Data_Aquatic.csv')
paramaqua = read.table('Attack_Rate_Param_Aquatic_Li.txt',h=T)
paramaqua$Handling = paramaqua$Ingestion+paramaqua$Digestion
paramaqua$Attack = paramaqua$Beta*paramaqua$Pc
paramaqua$PredatorMass = aquatic$PredatorMass
paramaqua = paramaqua[-150,]
aquatic = aquatic[-150,]

mypanel1=function(x,y,...){
  panel.xyplot(x,y, ...)
  panel.text(-8.5,-11.9,labels='A',cex=2)
}
pp4=xyplot(log10(attack.rate)~log10(PredatorMass),data=aquatic,col='black',ylim=c(-13,-1),xlim=c(-9,0),pch=19,cex=0.9,
           xlab=expression(paste("Predator Mass")), ylab=expression(paste("log"[10]*" Attack rate (m"^"3"*".s"^"-1"*")")),
           panel=mypanel1,
           par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=sizecex),par.ylab.text=list(cex=sizecex),
                             axis.components=list(top=list(tck=0),right=list(tck=0))),
           scales=list(x=list(at=c(-9,-6,-3,0),labels=c(expression(paste(mu*'g')),'mg','g','kg')),
                       y=list(at=c(-12,-10,-8,-6,-4,-2),labels=c('-12','-10','-8','-6','-4','-2'))),
           key=list(corner=c(0.03,0.95),padding.text=1,between=1.5,rep=T,
                    text=list(c('Observed','Predicted'),cex=1.3),
                    points=list(pch=c(19,17),col=c('black','red'),cex=1.3)
           )
)
pp4_2=xyplot(log10(Attack)~log10(PredatorMass),data=paramaqua,ylim=c(-13,-1.5),xlim=c(-9,0),pch=17,cex=0.9,col='red')
p4 = pp4+as.layer(pp4_2)

setwd("~/Master_Uottawa/Functional_Response/Data/Li_2017")

aquatic = read.csv('Data_Aquatic.csv')
paramaqua = read.table('Attack_Rate_Param_Aquatic_Li.txt',h=T)
paramaqua$Handling = paramaqua$Ingestion+paramaqua$Digestion
paramaqua$Attack = paramaqua$Beta*paramaqua$Pc
paramaqua$PredatorMass = aquatic$PredatorMass
paramaqua = paramaqua[-150,]
aquatic = aquatic[-150,]

setwd("~/Master_Uottawa/Functional_Response/Data/Vucic_Pestic_2010")
vucic = read.csv('Total_Vucic.csv')

mypanel1=function(x,y,...){
  panel.xyplot(x,y, ...)
  panel.text(-8.6,-3.8,labels='C',cex=2)
}
pp6=xyplot(log10(handling.time)~log10(PredatorMass),data=aquatic,col='black',ylim=c(-5,8.5),xlim=c(-9,0),pch=19,cex=0.9,
           xlab=expression(paste("Predator Mass")), ylab=expression(paste("log"[10]*" Handling time (s)")),
           panel=mypanel1,
           par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=sizecex),par.ylab.text=list(cex=sizecex),
                             axis.components=list(top=list(tck=0),right=list(tck=0))),
           scales=list(x=list(at=c(-9,-6,-3,0),labels=c(expression(paste(mu*'g')),'mg','g','kg')),
                       y=list(at=c(-4,-2,0,2,4,6,8),labels=c('-4','-2','0','2','4','6','8')))
)
pp6_2=xyplot(log10(Handling)~log10(PredatorMass),data=paramaqua,ylim=c(-5,8.5),xlim=c(-9,0),pch=17,cex=0.9,col='red',panel=mypanel1)
p6 = pp6+as.layer(pp6_2)


setwd("~/Master_Uottawa/Functional_Response/Matlab")
dat=read.csv('Data_Aquatic1.csv')
capture=read.table('Capture_Prob_Aquatic_Matlab.txt')

capt1=subset(dat,dat$published_data.standardisedtraitname=='Successful Attack Probability')
capt1=droplevels(capt1)
capt2=subset(dat,dat$published_data.standardisedtraitname=='Unsuccessful Attack Probability')
capt2=droplevels(capt2)
capt3=subset(dat,dat$published_data.standardisedtraitname=='Unsuccessful Attack Probability of Trial')
capt3=droplevels(capt3)

rea1=capt1$published_data.standardisedtraitvalue
rea2=1-capt2$published_data.standardisedtraitvalue
rea3=1-capt3$published_data.standardisedtraitvalue
Prob = c(rea1,rea2,rea3)
Size = c(capt1$published_data.interactor1massvaluesi,capt2$published_data.interactor1massvaluesi,capt3$published_data.interactor1massvaluesi)

mat = capture[135:210,135:210]
Predicted = diag(as.matrix(mat))
ResultsAqua = data.frame(Size,Prob,Predicted)
rm(rea1,rea2,rea3,Prob,Size,Predicted)

#sizecex = 1.5
mypanel1=function(x,y,...){
  panel.xyplot(x,y, ...)
  panel.text(-2.9,0.05,labels='B',cex=2)
}
pp2 = xyplot(Prob~log10(Size),data=ResultsAqua,col='black',ylim=c(-0.05,1.2),xlim=c(-3,-1),pch=19,cex=0.9,
             xlab=expression(paste("Predator Mass")), ylab=expression(paste("Capture probability")),
             panel=mypanel1,
             par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=sizecex),par.ylab.text=list(cex=sizecex),
                               axis.components=list(top=list(tck=0),right=list(tck=0))),
             scales=list(x=list(at=c(-3,-2,-1),labels=c(expression(paste('g')),'dag','hg')),
                         y=list(at=c(0,0.2,0.4,0.6,0.8,1),labels=c('0','0.2','0.4','0.6','0.8','1')))
)
pp2_21=xyplot(Predicted~log10(Size),data=ResultsAqua,ylim=c(0,1),xlim=c(-3,-1),pch=17,cex=0.9,col='red')
p2 = pp2+as.layer(pp2_21)


setwd("~/Master_Uottawa/Functional_Response/Plots")
svg('New_Plots_4.svg',width=13,height=16)
par(mar=c(5,6,4,3))
print(p4,position=c(0,0,1.18,0.98),split=c(1,1,2,3),more=T)
print(attackplot,position=c(0.2,0,1,1),split=c(2,1,2,3),more=T)
print(p2,position=c(0,0,1.18,0.98),split=c(1,2,2,3),more=T)
print(pcap,position=c(0.2,0,1,1),split=c(2,2,2,3),more=T)
print(p6,position=c(0,0,1.18,0.98),split=c(1,3,2,3),more=T)
print(phand,position=c(0.2,0,1,1),split=c(2,3,2,3),more=F)
dev.off()

# setwd("~/Master_Uottawa/Functional_Response/Plots")
# svg('New_Plots_4.svg',width=8,height=12)
# par(mar=c(5,6,4,3))
# print(p4,position=c(0,0,0.98,0.98),split=c(1,1,2,2),more=T)
# print(p2,position=c(0.5,0,1.5,0.98),split=c(1,1,2,2),more=T)
# print(p6,position=c(0.0,0,1,0.98),split=c(1,2,2,2),more=F)
# dev.off()


setwd("~/Master_Uottawa/Functional_Response/Matlab")

run = read.csv("RunningBrose.csv")
water = read.csv('SwimmingBrose.csv')
runmatlab = read.table('RunBroseMatlab.txt',h=T)
watermatlab = read.table('SwimBroseMatlab.txt',h=T)
fly = read.csv('FlyingBrose.csv')
flymatlab = read.table('FlyBroseMatlab.txt',h=T)

waterSpeed = water$max_speed_km.h.1 /3600 * 1000
watermass = water$body_mass_kg
water2 = data.frame(watermass,waterSpeed)

runSpeed = run$max_speed_km.h.1 /3600 * 1000
runmass = run$body_mass_kg
run2 = data.frame(runmass,runSpeed)

flySpeed = fly$max_speed_km.h.1 /3600 * 1000
flymass = fly$body_mass_kg
fly2 = data.frame(flymass,flySpeed)


mypanel1=function(x,y,...){
  panel.xyplot(x,y, ...)
  panel.text(-5.6,-1.8,labels='A',cex=2)
}

pp1=xyplot(log10(MaxSpeed)~log10(Mass),data=watermatlab,col='red',ylim=c(-2,2),xlim=c(-6,6.5),pch=17,cex=1.2,
           xlab=expression(paste("Body Mass")), ylab=expression(paste('log'[10]*" Speed (m.s"^'-1'*')')),
           panel=mypanel1,
           par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=3),par.ylab.text=list(cex=3),
                             axis.components=list(top=list(tck=0),right=list(tck=0))),
           scales=list(x=list(at=c(-6,-3,0,3,6),labels=c('mg','g','kg','Mg','Gg')),
                       y=list(at=c(-2,-1,0,1,2),labels=c('-2','-1','0','1','2'))),
           key=list(corner=c(0.05,0.95),padding.text=1,between=1.5,rep=T,
                    text=list(c('Observed','Predicted'),cex=1.4),
                    points=list(pch=c(19,17),col=c('black','red'),cex=1.5)
           )
)
pp1_2=xyplot(log10(waterSpeed)~log10(watermass),data=water2,ylim=c(-2,2),xlim=c(-6,6.5),pch=19,cex=1.2,col='black',panel=mypanel1)

p1 = pp1+as.layer(pp1_2)

setwd("~/Master_Uottawa/Functional_Response/Plots")
svg('New_Speed_Plots_Total.svg',width=15,height=7)
par(mar=c(5,6,4,3))
print(p1,position=c(0,0,1.15,0.98),split=c(1,1,2,1),more=T)
print(speedplot,position=c(0.16,0,1,1),split=c(2,1,2,1),more=F)
dev.off()

