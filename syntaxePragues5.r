require(graphics)
require(foreign)
require(psych)
require(vcd)
require(gmodels)
library(relimp, pos=4)
library(vegan)
library(nFactors)
library(cluster)
library(car)
library(foreign)
library(rgrs)
library(prettyR)
library(FactoMineR)
library(GPArotation)
library(psych)
library(ade4)
library(network)
library(sna)
library(statnet)
library(gmodels)
library(xtable)

#require(foreign)
#require(nnet)
#require(ggplot2)
#require(reshape2)
#library(Rage)
library(fmsb)


setwd("/home/widmer/conferences/2015Prague/paper/R")

load(file = '/home/widmer/conferences/2015Prague/paper/R/nvague1to3GiacomoOx.RData')

giac<-nvague1to3GiacomoOx

sink("giacvarNew.txt")
label(giac)
sink()

summary( glm( giac$w3f1Q1_8 == 'vrai' ~ giac$clusters, binomial))
summary( glm( giac$w3h1Q1_8 == 'vrai' ~ giac$clusters, binomial))

summary( glm( giac$w3f1Q21 == 'plusieurs fois par semaine' ~ giac$clusters, binomial))
summary( glm( giac$w3h1Q21 == 'plusieurs fois par semaine' ~ giac$clusters, binomial))

summary( glm( giac$w3f1Q22 == 'plusieurs fois par semaine' ~ giac$clusters, binomial))
summary( glm( giac$w3h1Q22 == 'plusieurs fois par semaine' ~ giac$clusters, binomial))

summary( glm( giac$w3f1Q23 == 'Une famille unie, soudee, affectueuse' ~ giac$clusters, binomial))
summary( glm( giac$w3h1Q23 == 'Une famille unie, soudee, affectueuse' ~ giac$clusters, binomial))

giacHommes <- giac[ which(giac$G..RSEX.y=="Homme"), ] #sous échantillon vague 1
giacFemmes <- giac[ which(giac$G..RSEX.y=="Femme"), ] #sous échantillon vague 2

dim(giacHommes)
dim(giacFemmes)
jana$Q2<-as.numeric(jana$idmen)


colnames (giacHommes) <- paste ("H", colnames(giacHommes), sep="_")
colnames (giacFemmes) <- paste ("F", colnames(giacFemmes), sep="_")

giacHommes$Q2<-giacHommes$H_Q2
giacFemmes$Q2<-giacFemmes$F_Q2

label(giacHommes)
label(giacFemmes)


#satisfaction conjugale, marginallement significatif

giac$Q13dic<-1
giac$Q13dic[giac$Q13=="vraiment bien"] <-0 
giac$Q13dic[giac$Q13=="assez bien"] <-0
round(tapply(giac$Q13dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q13dic ~ giac$Ward5multichannel))

#GIACOMO


#autonomie-fusion
giac$Q1_1


pairwise.t.test(giac$Q13dic,giac$Ward5multichannel, p.adj = "none")


freq(giac$Q1_5)

summary(aov(giac$Q1_5 ~ giac$Ward5multichannel))

pairwise.t.test(giac$H_Q13dic,giac$Ward5multichannel, p.adj = "none")


library(gmodels)
require(vcd)


freq(giac$clusters)

giac$w3f1Q1_1d[giac$w3f1Q1_1=="vrai"] <-1
giac$w3f1Q1_1d[giac$w3f1Q1_1=="plutot vrai"] <-2
giac$w3f1Q1_1d[giac$w3f1Q1_1=="plutot faux"] <-3
giac$w3f1Q1_1d[giac$w3f1Q1_1=="faux"] <-4

giac$w3h1Q1_1d[giac$w3h1Q1_1=="vrai"] <-1
giac$w3h1Q1_1d[giac$w3h1Q1_1=="plutot vrai"] <-2
giac$w3h1Q1_1d[giac$w3h1Q1_1=="plutot faux"] <-3
giac$w3h1Q1_1d[giac$w3h1Q1_1=="faux"] <-4


giac$w3f1Q1_2d[giac$w3f1Q1_2=="vrai"] <-1
giac$w3f1Q1_2d[giac$w3f1Q1_2=="plutot vrai"] <-2
giac$w3f1Q1_2d[giac$w3f1Q1_2=="plutot faux"] <-3
giac$w3f1Q1_2d[giac$w3f1Q1_2=="faux"] <-4

giac$w3h1Q1_2d[giac$w3h1Q1_2=="vrai"] <-1
giac$w3h1Q1_2d[giac$w3h1Q1_2=="plutot vrai"] <-2
giac$w3h1Q1_2d[giac$w3h1Q1_2=="plutot faux"] <-3
giac$w3h1Q1_2d[giac$w3h1Q1_2=="faux"] <-4

giac$w3h1Q1_3d[giac$w3h1Q1_3=="vrai"] <-1
giac$w3h1Q1_3d[giac$w3h1Q1_3=="plutot vrai"] <-2
giac$w3h1Q1_3d[giac$w3h1Q1_3=="plutot faux"] <-3
giac$w3h1Q1_3d[giac$w3h1Q1_3=="faux"] <-4

giac$w3f1Q1_3d[giac$w3f1Q1_3=="vrai"] <-1
giac$w3f1Q1_3d[giac$w3f1Q1_3=="plutot vrai"] <-2
giac$w3f1Q1_3d[giac$w3f1Q1_3=="plutot faux"] <-3
giac$w3f1Q1_3d[giac$w3f1Q1_3=="faux"] <-4

giac$w3h1Q1_4d[giac$w3h1Q1_4=="vrai"] <-1
giac$w3h1Q1_4d[giac$w3h1Q1_4=="plutot vrai"] <-2
giac$w3h1Q1_4d[giac$w3h1Q1_4=="plutot faux"] <-3
giac$w3h1Q1_4d[giac$w3h1Q1_4=="faux"] <-4

giac$w3f1Q1_4d[giac$w3f1Q1_4=="vrai"] <-1
giac$w3f1Q1_4d[giac$w3f1Q1_4=="plutot vrai"] <-2
giac$w3f1Q1_4d[giac$w3f1Q1_4=="plutot faux"] <-3
giac$w3f1Q1_4d[giac$w3f1Q1_4=="faux"] <-4

giac$w3h1Q1_5d[giac$w3h1Q1_5=="vrai"] <-4
giac$w3h1Q1_5d[giac$w3h1Q1_5=="plutot vrai"] <-3
giac$w3h1Q1_5d[giac$w3h1Q1_5=="plutot faux"] <-2
giac$w3h1Q1_5d[giac$w3h1Q1_5=="faux"] <-1

giac$w3f1Q1_5d[giac$w3f1Q1_5=="vrai"] <-4
giac$w3f1Q1_5d[giac$w3f1Q1_5=="plutot vrai"] <-3
giac$w3f1Q1_5d[giac$w3f1Q1_5=="plutot faux"] <-2
giac$w3f1Q1_5d[giac$w3f1Q1_5=="faux"] <-1

giac$w3h1Q1_6d[giac$w3h1Q1_6=="vrai"] <-1
giac$w3h1Q1_6d[giac$w3h1Q1_6=="plutot vrai"] <-2
giac$w3h1Q1_6d[giac$w3h1Q1_6=="plutot faux"] <-3
giac$w3h1Q1_6d[giac$w3h1Q1_6=="faux"] <-4

giac$w3f1Q1_6d[giac$w3f1Q1_6=="vrai"] <-1
giac$w3f1Q1_6d[giac$w3f1Q1_6=="plutot vrai"] <-2
giac$w3f1Q1_6d[giac$w3f1Q1_6=="plutot faux"] <-3
giac$w3f1Q1_6d[giac$w3f1Q1_6=="faux"] <-4

giac$w3h1Q1_8d[giac$w3h1Q1_8=="vrai"] <-4
giac$w3h1Q1_8d[giac$w3h1Q1_8=="plutot vrai"] <-3
giac$w3h1Q1_8d[giac$w3h1Q1_8=="plutot faux"] <-2
giac$w3h1Q1_8d[giac$w3h1Q1_8=="faux"] <-1

giac$w3f1Q1_8d[giac$w3f1Q1_8=="vrai"] <-4
giac$w3f1Q1_8d[giac$w3f1Q1_8=="plutot vrai"] <-3
giac$w3f1Q1_8d[giac$w3f1Q1_8=="plutot faux"] <-2
giac$w3f1Q1_8d[giac$w3f1Q1_8=="faux"] <-1


giac$w3h1Q1_9d[giac$w3h1Q1_9=="vrai"] <-1
giac$w3h1Q1_9d[giac$w3h1Q1_9=="plutot vrai"] <-2
giac$w3h1Q1_9d[giac$w3h1Q1_9=="plutot faux"] <-3
giac$w3h1Q1_9d[giac$w3h1Q1_9=="faux"] <-4

giac$w3f1Q1_9d[giac$w3f1Q1_9=="vrai"] <-1
giac$w3f1Q1_9d[giac$w3f1Q1_9=="plutot vrai"] <-2
giac$w3f1Q1_9d[giac$w3f1Q1_9=="plutot faux"] <-3
giac$w3f1Q1_9d[giac$w3f1Q1_9=="faux"] <-4

giac$q1Fauton<-giac$w3f1Q1_1d+
  giac$w3f1Q1_2d+
  giac$w3f1Q1_3d+
  giac$w3f1Q1_4d+
  giac$w3f1Q1_5d+
  giac$w3f1Q1_6d+
  giac$w3f1Q1_8d+
  giac$w3f1Q1_9d

giac$q1Hauton<-giac$w3h1Q1_1d+
  giac$w3h1Q1_2d+
  giac$w3h1Q1_3d+
  giac$w3h1Q1_4d+
  giac$w3h1Q1_5d+
  giac$w3h1Q1_6d+
  giac$w3h1Q1_8d+
  giac$w3h1Q1_9d


giac$q1FautonShort<-giac$w3f1Q1_1d+
   giac$w3f1Q1_3d+
   giac$w3f1Q1_5d+
  giac$w3f1Q1_6d+
  giac$w3f1Q1_8d
 

giac$q1HautonShort<-giac$w3h1Q1_1d+
  giac$w3h1Q1_3d+
  giac$w3h1Q1_5d+
  giac$w3h1Q1_6d+
  giac$w3h1Q1_8d



round(tapply(giac$q1Hauton, giac$clusters,mean,na.rm=TRUE),digits=2)
summary(aov(giac$q1Hauton ~ giac$clusters))
pairwise.t.test(giac$q1Hauton,giac$clusters, p.adj = "none")


round(tapply(giac$q1Fauton, giac$clusters,mean,na.rm=TRUE),digits=2)
summary(aov(giac$q1Fauton ~ giac$clusters))
pairwise.t.test(giac$q1Fauton,giac$clusters, p.adj = "none")



summary( lm( giac$q1FautonShort ~ giac$clusters))
summary( lm( giac$q1HautonShort ~ giac$clusters))

recnum = function(x, lim = c(1:4)) as.numeric(as.character(factor(x, levels = levels(x), labels = c(lim) )))
recnum(giac$w3f1Q1_9)
table(giac$)
table(giac$w3f1Q1_9, recnum(x = giac$w3f1Q1_9))


#OUVERTURE-FERMETURE 


table(giac$w3f1Q2_1, recnum(giac$w3f1Q2_1, lim = c(4:1)) )
table(giac$w3f1Q2_4, recnum(giac$w3f1Q2_4, lim = c(1:4)) )

### Femme

# q1
q2_1f = recnum(giac$w3f1Q2_1, lim = c(4:1))
# q2
q2_2f = recnum(giac$w3f1Q2_2, lim = c(4:1))
# q3
q2_3f = recnum(giac$w3f1Q2_3, lim = c(1:4))
# q4
q2_4f = recnum(giac$w3f1Q2_4, lim = c(1:4))
# q5
q2_5f = recnum(giac$w3f1Q2_5, lim = c(4:1))
# q6
q2_6f = recnum(giac$w3f1Q2_6, lim = c(4:1))



### Homme

# q1
q2_1h = recnum(giac$w3h1Q2_1, lim = c(4:1))
# q2
q2_2h = recnum(giac$w3h1Q2_2, lim = c(4:1))
# q3
q2_3h = recnum(giac$w3h1Q2_3, lim = c(1:4))
# q4
q2_4h = recnum(giac$w3h1Q2_4, lim = c(1:4))
# q5
q2_5h = recnum(giac$w3h1Q2_5, lim = c(4:1))
# q6
q2_6h = recnum(giac$w3h1Q2_6, lim = c(4:1))

q2F = q2_1f + q2_2f + q2_3f + q2_4f + q2_5f + q2_6f
q2H = q2_1h + q2_2h + q2_3h + q2_4h + q2_5h + q2_6h


summary( lm( q2F ~ giac$clusters))
summary( lm( q2H ~ giac$clusters))

#GENRE





q2FS = q2_2f + q2_4f + q2_5f
q2HS = q2_2h + q2_4h + q2_5h
summary( lm( q2FS ~ giac$clusters))
summary( lm( q2HS ~ giac$clusters))
summary( lm( q2_1f ~ giac$clusters))
summary( lm( q2_2f ~ giac$clusters))
summary( lm( q2_3f ~ giac$clusters))
summary( lm( q2_4f ~ giac$clusters))
summary( lm( q2_5f ~ giac$clusters))
summary( lm( q2_6f ~ giac$clusters))

summary( lm( q2_1h ~ giac$clusters))
summary( lm( q2_2h ~ giac$clusters))
summary( lm( q2_3h ~ giac$clusters))
summary( lm( q2_4h ~ giac$clusters))
summary( lm( q2_5h ~ giac$clusters))
summary( lm( q2_6h ~ giac$clusters))




#scores q1 (fusion-autonomie) femmmes
CrossTable(giac$w3f1Q1_1,giac$clusters)
mytable4 <- xtabs(~giac$w3f1Q1_1+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) 

CrossTable(giac$w3f1Q1_2,giac$clusters)
mytable4 <- xtabs(~giac$w3f1Q1_2+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) 

CrossTable(giac$w3f1Q1_3,giac$clusters)
mytable4 <- xtabs(~giac$w3f1Q1_3+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) 


CrossTable(giac$w3f1Q1_4,giac$clusters)
mytable4 <- xtabs(~giac$w3f1Q1_4+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) 

CrossTable(giac$w3f1Q1_5,giac$clusters)
mytable4 <- xtabs(~giac$w3f1Q1_5+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) #sig

CrossTable(giac$w3f1Q1_6,giac$clusters)
mytable4 <- xtabs(~giac$w3f1Q1_6+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) #sig

CrossTable(giac$w3f1Q1_7,giac$clusters)
mytable4 <- xtabs(~giac$w3f1Q1_6+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) 

CrossTable(giac$w3f1Q1_8,giac$clusters)
mytable4 <- xtabs(~giac$w3f1Q1_8+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) 

CrossTable(giac$w3f1Q1_9,giac$clusters)
mytable4 <- xtabs(~giac$w3f1Q1_9+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) 


#scores q1 (fusion-autonomie) hommes

CrossTable(giac$w3h1Q1_1,giac$clusters)
mytable4 <- xtabs(~giac$w3h1Q1_1+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) 

CrossTable(giac$w3h1Q1_2,giac$clusters)
mytable4 <- xtabs(~giac$w3h1Q1_2+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) 

CrossTable(giac$w3h1Q1_3,giac$clusters)
mytable4 <- xtabs(~giac$w3h1Q1_3+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) 



CrossTable(giac$w3h1Q1_4,giac$clusters)
mytable4 <- xtabs(~giac$w3h1Q1_4+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) 

CrossTable(giac$w3h1Q1_5,giac$clusters)
mytable4 <- xtabs(~giac$w3h1Q1_5+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) #sig

CrossTable(giac$w3h1Q1_6,giac$clusters)
mytable4 <- xtabs(~giac$w3h1Q1_6+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) #sig

CrossTable(giac$w3h1Q1_7,giac$clusters)
mytable4 <- xtabs(~giac$w3h1Q1_6+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) 

CrossTable(giac$w3h1Q1_8,giac$clusters)
mytable4 <- xtabs(~giac$w3h1Q1_8+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) 

CrossTable(giac$w3h1Q1_9,giac$clusters)
mytable4 <- xtabs(~giac$w3h1Q1_9+giac$clusters,exclude=NULL,na.action=na.pass)
assocstats(mytable4) 









pour les hommes
freq(giac$H_Q13)
giac$H_Q13dic<-1
giac$H_Q13dic[giac$H_Q13=="vraiment bien"] <-0
giac$H_Q13dic[giac$H_Q13=="assez bien"] <-0
round(tapply(giac$H_Q13dic, giac$H_Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$H_Q13dic ~ giac$H_Ward5multichannel))

pairwise.t.test(giac$H_Q13dic,giac$H_Ward5multichannel, p.adj = "none")

# pensee de divorce

giac$Q14dic<-1
giac$Q14dic[giac$Q14=="Non, jamais"] <-0 
round(tapply(giac$Q14dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q14dic ~ giac$Ward5multichannel))


pairwise.t.test(giac$Q14dic,giac$Ward5multichannel, p.adj = "none")

freq(giac$F_Q14)
giac$F_Q14dic<-1
giac$F_Q14dic[giac$F_Q14=="Non, jamais"] <-0
round(tapply(giac$F_Q14dic, giac$F_Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$F_Q14dic ~ giac$F_Ward5multichannel))

pairwise.t.test(giac$F_Q14dic,giac$F_Ward5multichannel, p.adj = "none")

pour les hommes
freq(giac$H_Q13)
giac$H_Q13dic<-1
giac$H_Q13dic[giac$H_Q13=="Non, jamais"] <-0
round(tapply(giac$H_Q13dic, giac$H_Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$H_Q13dic ~ giac$H_Ward5multichannel))

pairwise.t.test(giac$H_Q13dic,giac$H_Ward5multichannel, p.adj = "none")

freq(giac$Q5_1)

summary(aov(giac$Q1_5 ~ giac$H_Ward5multichannel))

pairwise.t.test(giac$H_Q13dic,giac$H_Ward5multichannel, p.adj = "none")




#communicationbnon sig

freq(giac$Q10_1)
giac$Q10_1dic<-1
giac$Q10_1dic[giac$Q10_1=="Non, pas du tout"] <-0
round(tapply(giac$Q10_1dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q10_1dic ~ giac$Ward5multichannel))

pairwise.t.test(giac$Q10_1dic,giac$Ward5multichannel, p.adj = "none")

freq(giac$Q10_14)

#msententes relations sexuelles super significatif

freq(giac$Q10_2)
giac$Q10_2dic<-1
giac$Q10_2dic[giac$Q10_2=="Non, pas du tout"] <-0
giac$Q10_2dic[giac$Q10_2=="Plut?t non"] <-0
round(tapply(giac$Q10_2dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q10_2dic ~ giac$Ward5multichannel))

pairwise.t.test(giac$Q10_2dic,giac$Ward5multichannel, p.adj = "none")


labels(tot)

freq(giac$F_Q10_2)
giac$F_Q10_2dic<-1
giac$F_Q10_2dic[giac$F_Q10_2=="Non, pas du tout"] <-0
giac$F_Q10_2dic[giac$F_Q10_2=="Plut?t non"] <-0
round(tapply(giac$F_Q10_2dic, giac$F_Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$F_Q10_2dic ~ giac$F_Ward5multichannel))

pairwise.t.test(giac$F_Q10_2dic,giac$F_Ward5multichannel, p.adj = "none")

Surtout pour les hommes

freq(giac$H_Q10_2)
giac$H_Q10_2dic<-1
giac$H_Q10_2dic[giac$H_Q10_2=="Non, pas du tout"] <-0
giac$H_Q10_2dic[giac$H_Q10_2=="Plut?t non"] <-0
round(tapply(giac$H_Q10_2dic, giac$H_Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$H_Q10_2dic ~ giac$H_Ward5multichannel))

pairwise.t.test(giac$H_Q10_2dic,giac$H_Ward5multichannel, p.adj = "none")


#deception sentimentale, marginally significant

freq(giac$Q10_3)
giac$Q10_3dic<-1
giac$Q10_3dic[giac$Q10_3=="Non, pas du tout"] <-0
giac$Q10_3dic[giac$Q10_3=="Plut?t non"] <-0
round(tapply(giac$Q10_3dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q10_3dic ~ giac$Ward5multichannel))

pairwise.t.test(giac$Q10_3dic,giac$Ward5multichannel, p.adj = "none")


freq(giac$F_Q10_3)
giac$F_Q10_3dic<-1
giac$F_Q10_3dic[giac$F_Q10_3=="Non, pas du tout"] <-0
giac$F_Q10_3dic[giac$F_Q10_3=="Plut?t non"] <-0
round(tapply(giac$F_Q10_3dic, giac$F_Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$F_Q10_3dic ~ giac$F_Ward5multichannel))

pairwise.t.test(giac$F_Q10_3dic,giac$F_Ward5multichannel, p.adj = "none")

freq(giac$H_Q10_3)
giac$H_Q10_3dic<-1
giac$H_Q10_3dic[giac$H_Q10_3=="Non, pas du tout"] <-0
giac$H_Q10_3dic[giac$H_Q10_3=="Plut?t non"] <-0
round(tapply(giac$H_Q10_3dic, giac$H_Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$H_Q10_3dic ~ giac$H_Ward5multichannel))

pairwise.t.test(giac$H_Q10_3dic,giac$H_Ward5multichannel, p.adj = "none")




#amour, marginally significant

freq(giac$Q10_4)
giac$Q10_4dic<-1
giac$Q10_4dic[giac$Q10_4=="Non, pas du tout"] <-0
round(tapply(giac$Q10_4dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q10_4dic ~ giac$Ward5multichannel))

pairwise.t.test(giac$Q10_4dic,giac$Ward5multichannel, p.adj = "none")



#personnalité, non sig
freq(giac$Q10_5)
giac$Q10_5dic<-1
giac$Q10_5dic[giac$Q10_5=="Non, pas du tout"] <-0
giac$Q10_5dic[giac$Q10_5=="Plut?t non"] <-0
round(tapply(giac$Q10_5dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q10_5dic ~ giac$Ward5multichannel))

pairwise.t.test(giac$Q10_5dic,giac$Ward5multichannel, p.adj = "none")



#infedility, marginally significant

freq(giac$Q10_6)
giac$Q10_6dic<-1
giac$Q10_6dic[giac$Q10_6=="Non, pas du tout"] <-0
round(tapply(giac$Q10_6dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q10_6dic ~ giac$Ward5multichannel))

pairwise.t.test(giac$Q10_6dic,giac$Ward5multichannel, p.adj = "none")





#absence, non sig
freq(giac$Q10_7)
giac$Q10_7dic<-1
giac$Q10_7dic[giac$Q10_7=="Non, pas du tout"] <-0
round(tapply(giac$Q10_7dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q10_7dic ~ giac$Ward5multichannel))

pairwise.t.test(giac$Q10_7dic,giac$Ward5multichannel, p.adj = "none")




#taches dom sig
freq(giac$Q10_8)
giac$Q10_8dic<-1
giac$Q10_8dic[giac$Q10_8=="Non, pas du tout"] <-0
giac$Q10_8dic[giac$Q10_8=="Plut?t non"] <-0
round(tapply(giac$Q10_8dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q10_8dic ~ giac$Ward5multichannel))

pairwise.t.test(giac$Q10_8dic,giac$Ward5multichannel, p.adj = "none")

# prof
freq(giac$Q10_9)
giac$Q10_9dic<-1
giac$Q10_9dic[giac$Q10_9=="Non, pas du tout"] <-0 
giac$Q10_9dic[giac$Q10_9=="Plut?t non"] <-0
freq(giac$Q10_9dic)
round(tapply(giac$Q10_9dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q10_9dic ~ giac$Ward5multichannel))

pairwise.t.test(giac$Q10_9dic,giac$Ward5multichannel, p.adj = "none")


#temps sig
freq(giac$Q10_10)
giac$Q10_10dic<-1
giac$Q10_10dic[giac$Q10_10=="Non, pas du tout"] <-0
round(tapply(giac$Q10_10dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q10_10dic ~ giac$Ward5multichannel))

pairwise.t.test(giac$Q10_10dic,giac$Ward5multichannel, p.adj = "none")

#temps sig
freq(giac$Q10_11)
giac$Q10_11dic<-1
giac$Q10_11dic[giac$Q10_11=="Non, pas du tout"] <-0
giac$Q10_9dic[giac$Q10_11=="Plut?t non"] <-0
round(tapply(giac$Q10_11dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q10_11dic ~ giac$Ward5multichannel))

pairwise.t.test(giac$Q10_11dic,giac$Ward5multichannel, p.adj = "none")



#loisirs
freq(giac$Q10_12)
giac$Q10_12dic<-1
giac$Q10_12dic[giac$Q10_12=="Non, pas du tout"] <-0 
round(tapply(giac$Q10_12dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q10_12dic ~ giac$Ward5multichannel))

pairwise.t.test(giac$Q10_12dic,giac$Ward5multichannel, p.adj = "none")


# violence physique sexuelle, marginallement sig
freq(giac$Q10_13)
giac$Q10_13dic<-1
giac$Q10_13dic[giac$Q10_13=="Non, pas du tout"] <-0
round(tapply(giac$Q10_13dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q10_13dic ~ giac$Ward5multichannel))

pairwise.t.test(giac$Q10_13dic,giac$Ward5multichannel, p.adj = "none")


#enfqnts, non sig
freq(giac$Q10_14)
giac$Q10_14dic<-1
giac$Q10_14dic[giac$Q10_14=="Non, pas du tout"] <-1
giac$Q10_14dic[giac$Q10_14=="Plut?t non"] <-1
round(tapply(giac$Q10_14dic, giac$Ward5multichannel,mean,na.rm=TRUE),digits=2)
summary(aov(giac$Q10_14dic ~ giac$Ward5multichannel))

pairwise.t.test(giac$Q10_14dic,giac$Ward5multichannel, p.adj = "none")



#AVEC LABELS


#communicationbnon sig

giac$Q10_l1dic<-"comm-non"
giac$Q10_l1dic[giac$Q10_1=="Non, pas du tout"] <-"comm-oui"



#msententes relations sexuelles super significatif

giac$Q10_l2dic<-"sex-oui"
giac$Q10_l2dic[giac$Q10_2=="Non, pas du tout"] <-"sex-non"
giac$Q10_l2dic[giac$Q10_2=="Plut?t non"] <-"sex-non"


#deception sentimentale, marginally significant

giac$Q10_l3dic<-"sent-oui"
giac$Q10_l3dic[giac$Q10_3=="Non, pas du tout"] <-"sent-non"
giac$Q10_l3dic[giac$Q10_3=="Plut?t non"] <-"sent-non"


#amour, marginally significant

giac$Q10_l4dic<-"desamour-oui"
giac$Q10_l4dic[giac$Q10_4=="Non, pas du tout"] <-"desamour-non"



#personnalité, non sig
giac$Q10_l5dic<-"perso-oui"
giac$Q10_l5dic[giac$Q10_5=="Non, pas du tout"] <-"perso-non"
giac$Q10_l5dic[giac$Q10_5=="Plut?t non"] <-"perso-non"



#infedility, marginally significant

giac$Q10_l6dic<-"infid-oui"
giac$Q10_l6dic[giac$Q10_6=="Non, pas du tout"] <-"infid-non"


#absence, non sig
giac$Q10_l7dic<-"abs-oui"
giac$Q10_l7dic[giac$Q10_7=="Non, pas du tout"] <-"abs-non"


#taches dom sig
giac$Q10_l8dic<-"dom-oui"
giac$Q10_l8dic[giac$Q10_8=="Non, pas du tout"] <-"dom-non"
giac$Q10_l8dic[giac$Q10_8=="Plut?t non"] <-"dom-non"

# prof
giac$Q10_l9dic<-"prof-oui"
giac$Q10_l9dic[giac$Q10_9=="Non, pas du tout"] <-"prof-non" 
giac$Q10_l9dic[giac$Q10_9=="Plut?t non"] <-"prof-oui"

#temps sig
giac$Q10_l10dic<-"temps-oui"
giac$Q10_l10dic[giac$Q10_10=="Non, pas du tout"] <-"temps-non"

#temps sig
giac$Q10_l11dic<-"argent-oui"
giac$Q10_l11dic[giac$Q10_11=="Non, pas du tout"] <-"argent-non"
giac$Q10_l11dic[giac$Q10_11=="Plut?t non"] <-"argent-non"


#loisirs
giac$Q10_l12dic<-"loisirs-oui"
giac$Q10_l12dic[giac$Q10_12=="Non, pas du tout"] <-"loisirs-non" 


# violence physique sexuelle, marginallement sig
giac$Q10_l13dic<-"viol-oui"
giac$Q10_l13dic[giac$Q10_13=="Non, pas du tout"] <-"viol-non"


#enfqnts, non sig

giac$Q10_l14dic<-"enfants-oui"
giac$Q10_l14dic[giac$Q10_14=="Non, pas du tout"] <-"Enfants-non"
giac$Q10_l14dic[giac$Q10_14=="Plut?t non"] <-"Enfants-non"












#MCA

library(FactoMineR)
library(PCAmixdata) # package pour faire des rotations

#avec tous: ambi, configuration, sexe, partner, enfant, fratrie, revenu et santé fonctionnelle
total1<- c("Ward5multichannel", "G..RSEX.y","Q10_l14dic") #variables sélectionnées pour l'analyse des correspondances
total1<- c("Ward5multichannel", "G..RSEX.y","Q10_l1dic","Q10_l2dic","Q10_l3dic","Q10_l4dic","Q10_l5dic","Q10_l6dic","Q10_l7dic","Q10_l8dic","Q10_l9dic","Q10_l10dic", "Q10_l11dic","Q10_l12dic","Q10_l13dic","Q10_l14dic") #variables sélectionnées pour l'analyse des correspondances

netExTotal1 <- giac[total1]
netExTotal1<-na.omit(netExTotal1) #enleve les cs avec missing values












#avec rotation


library(calibrate)

mca1<-PCAmix(,netExTotal1,ndim=3, graph=TRUE)  #rotation
mca1$categ.coord
mca1$eig
mca1$res.categ

plot(mca1$categ.coord[,1],mca1$categ.coord[,2],choice="ind", main="MCA scores",label=TRUE)
textxy(mca1$categ.coord[,1],mca1$categ.coord[,2],labels(mca1$categ.coord[,1],mca1$categ.coord[,2]))




rot<-PCArot(mca1,2,graph=TRUE)
plot(rot$categ.coord[,1],rot$categ.coord[,2],choice="ind", main="Rotated scores",label=TRUE)
textxy(rot$categ.coord[,1],rot$categ.coord[,2],labels(rot$categ.coord[,1],rot$categ.coord[,2]),cx=0.75)
plot(rot,choice="var",main="Squared loadings after rotation")
plot(rot,choice="categ",main="Categories after rotation")
plot(rot,choice="cor",main="Correlation circle after rotation")

include("scatterplot3d")

resul<-as.matrix(rot$categ.coord)

s3d<-scatterplot3d(resul[,1],resul[,3],resul[,2], highlight.3d = TRUE, angle = 120,type="h")
s3d.coords <- s3d$xyz.convert(resul[,1],resul[,3],resul[,2]) # convert 3D coords to 2D projection
text(s3d.coords$x, s3d.coords$y,labels=labels(resul[,1]),cex=.8, pos=4)






