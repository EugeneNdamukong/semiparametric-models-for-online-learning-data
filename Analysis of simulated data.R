#Loading libraries 
library(mgcv)
library(lme4)
install.packages("AICcmodavg")
library(AICcmodavg)
library(itsadug)
data.seq=read.table(file.choose(),header = TRUE)
View(data.seq)
names(data.seq)


#####################################################################################"
########################### CASE ONE ################################################

#Data for modelling
data.seq.ses2=data.frame(data.seq.ses,
                          abil.w.lin=data.seq.ses$Trueability.w.lin+rnorm(nrow(data.seq.ses),sd=0.05),
                          abil.w.nlin=data.seq.ses$Trueability.w.nlin+rnorm(nrow(data.seq.ses),sd=0.005))

######## Ability versus within time linear relationship ###########
# Fitting GAM
gam.lin.ab=bam(abil.w.lin~Session + s(Student,bs='re')+s(Wtime,by=Session,bs='cr',k=50)+s(Wtime,Student,bs='re'),
               family=gaussian(link = "identity"), data=data.seq.ses2, method = "REML")
summary(gam.lin.ab)
gam.vcomp(gam.lin.ab)
confint(gam.lin.ab,parm = "Session",type = "confidence")
par(mfrow=c(2,2))
gam.check(gam.lin.ab)


#Confidence intervals
coef.ses.lin=coef(gam.lin.ab)
Var.ses.lin <- vcov(gam.lin.ab, unconditional = TRUE)
se.ses.lin <- sqrt(diag(Var.ses.lin))
ses.lin.vec=c("(Intercept)","Session1","Session2","Session3","Session4")
for(i in ses.lin.vec){
  i <- which(names(coef.ses.lin) == i)
  print(coef.ses.lin[i] + (c(-1,1) * (2 * se.ses.lin[i])))
}


#Fitting GLMM
glm.lin.ab=lmer(Trueability.w.lin~Session+Wtime:Session+(1+Wtime|Student),
                 data = data.seq.ses2)
summary(glm.lin.ab)


########## Ability versus within time non-linear relationship ##############

#Fitting GAM
gam.nlin.ab=bam(abil.w.nlin~Session+s(Student,bs='re')+s(Wtime,by=Session,bs='cr',k=18)+
                  s(Wtime,Student,bs='re'),family=gaussian(link = "identity"), data=data.seq.ses2, method = "REML")
summary(gam.nlin.ab)
gam.vcomp(gam.nlin.ab)
gam.check(gam.nlin.ab)

#Fitting GLM
glm.nlin.ab=lmer(abil.w.nlin~Session+Wtime:Session+(1+Wtime|Student),
                data = data.seq.ses2)
summary(glm.nlin.ab)


AIC(gam.lin.ab,glm.lin.ab)


#Preparing data for plots of fitted
library(doBy)
library(ggplot2)

fit.lin.data.rm=data.frame(data.seq.ses2,fit.gam=fitted(gam.lin.ab))
fit.lin.data.rm=orderBy(~Wtime,fit.lin.data.rm)

fit.nlin.data.rm=data.frame(data.seq.ses2,fit.gam=fitted(gam.nlin.ab))
fit.nlin.data.rm=orderBy(~Wtime,fit.nlin.data.rm)


fit.glm.lin160=data.frame(data.seq.ses2,fit.gam=fitted(glm.lin.ab))
fit.glm.lin160=orderBy(~Wtime,fit.glm.lin160)

fit.glm.nlin160=data.frame(data.seq.ses2,fit.gam=fitted(glm.nlin.ab))
fit.glm.nlin160=orderBy(~Wtime,fit.glm.nlin160)

par(mfrow=c(1,2))
plot(fit.nlin.data.rm$fit.gam~fit.nlin.data.rm$Wtime)
plot(fit.glm.nlin160$fit.gam~fit.nlin.data.rm$Wtime)

#True ability trajectory
ggplot(data.seq.ses2, aes(x=Wtime, y=Trueability.w.nlin)) + 
  geom_point(aes(group=data.seq.ses2$Student),colour=data.seq.ses2$Session)+
  labs(x="Within-time",y="Ability")

#Plot of fitted on the expected ability vs wtime
#Linear plots per session estimated by GAM
p.gam_lin=ggplot(data=fit.lin.data.rm,aes(x=Wtime,y=Trueability.w.lin))+
  geom_point(data=fit.lin.data.rm)+theme(legend.position="none")+
  geom_line(data = fit.lin.data.rm,aes(x=Wtime,y=fit.gam,group=Student),colour="red")+
  labs(x="Within-time",y="Ability")+ggtitle("Estimated trend by GAM")

#Linear plots per session estimated by GLMM
p_simlin.rm160=ggplot(data=fit.glm.lin160,aes(x=Wtime,y=Trueability.w.lin))+
  geom_point(data=fit.glm.lin160)+theme(legend.position="none")+
  geom_line(data = fit.glm.lin160,aes(x=Wtime,y=fit.gam,group=Student),colour="red")+
  labs(x="Within-time",y="Ability")+ggtitle("Estimated trend by GLMM")

#Linear plots of 3 learners in session 1 estimated by GAM
fit.lin.data.ses=subset(fit.lin.data.rm,Session==1)
p.gam_lin.stud=ggplot(data=fit.lin.data.ses[fit.lin.data.ses$Student==6 |fit.lin.data.ses$Student==7|fit.lin.data.ses$Student==8, ],aes(x=Wtime,y=Trueability.w.lin))+
  geom_point(data=fit.lin.data.ses[fit.lin.data.ses$Student==6 |fit.lin.data.ses$Student==7|fit.lin.data.ses$Student==8, ])+theme(legend.position="none")+
  geom_line(data = fit.lin.data.ses[fit.lin.data.ses$Student==6 |fit.lin.data.ses$Student==7|fit.lin.data.ses$Student==8, ],aes(x=Wtime,y=fit.gam,group=Student),colour="red")+
  labs(x="Within-time",y="Ability")+ggtitle("Estimated trend by GAM for learner 6,7 and 8 in Session 1")

#Linear plots of 3 learners in session 1 estimated by GLMM
fit.glm.lin.ses=subset(fit.glm.lin160,Session==1)
p_simlin.stud=ggplot(data=fit.glm.lin.ses[fit.glm.lin.ses$Student==6 |fit.glm.lin.ses$Student==7|fit.glm.lin.ses$Student==8, ],aes(x=Wtime,y=Trueability.w.lin))+
  geom_point(data=fit.glm.lin.ses[fit.glm.lin.ses$Student==6 |fit.glm.lin.ses$Student==7|fit.glm.lin.ses$Student==8, ])+theme(legend.position="none")+
  geom_line(data = fit.glm.lin.ses[fit.glm.lin.ses$Student==6 |fit.glm.lin.ses$Student==7|fit.glm.lin.ses$Student==8, ],aes(x=Wtime,y=fit.gam,group=Student),colour="red")+
  labs(x="Within-time",y="Ability")+ggtitle("Estimated trend by GLMM for learner 6,7 and 8 in Session 1")



#Non-linear plots per sessions estimated by GAM
p.gam_nlin=ggplot(data=fit.nlin.data.rm,aes(x=Wtime,y=Trueability.w.nlin))+
  geom_point(data=fit.nlin.data.rm)+theme(legend.position="none")+
  geom_line(data = fit.nlin.data.rm,aes(x=Wtime,y=fit.gam,group=Student),colour="red")+
  labs(x="Within-time",y="Ability")+ggtitle("Estimated trend by GAM")

#Non-linear plots per sessions estimated by GLMM
p.glm_nlin=ggplot(data=fit.glm.nlin160,aes(x=Wtime,y=Trueability.w.nlin))+
  geom_point(data=fit.glm.nlin160)+theme(legend.position="none")+
  geom_line(data = fit.glm.nlin160,aes(x=Wtime,y=fit.gam,group=Student),colour="red")+
  labs(x="Within-time",y="Ability")+ggtitle("Estimated trend by GLMM")


#Non-linear plots of 3 learners in session 4 estimated by GAM
fit.nlin.data.ses=subset(fit.nlin.data.rm,Session==4)
p.gam_nlin.stud=ggplot(data=fit.nlin.data.ses[fit.nlin.data.ses$Student==6 |fit.nlin.data.ses$Student==7|fit.nlin.data.ses$Student==8, ],aes(x=Wtime,y=Trueability.w.nlin))+
  geom_point(data=fit.nlin.data.ses[fit.nlin.data.ses$Student==6 |fit.nlin.data.ses$Student==7|fit.nlin.data.ses$Student==8, ])+theme(legend.position="none")+
  geom_line(data = fit.nlin.data.ses[fit.nlin.data.ses$Student==6 |fit.nlin.data.ses$Student==7|fit.nlin.data.ses$Student==8, ],aes(x=Wtime,y=fit.gam,group=Student),colour="red")+
  labs(x="Within-time",y="Ability")+ggtitle("Estimated trend by GAM for learner 6,7 and 8 in Session 1")


#Non-linear plots of 3 learners in session 4 estimated by GLMM
fit.nlin.data.ses.stud=subset(fit.glm.nlin160,Session==4) 
p.glm_nlin.stud=ggplot(data=fit.nlin.data.ses.stud[fit.nlin.data.ses.stud$Student==6 |fit.nlin.data.ses.stud$Student==7|fit.nlin.data.ses.stud$Student==8, ],aes(x=Wtime,y=Trueability.w.nlin))+
  geom_point(data=fit.nlin.data.ses.stud[fit.nlin.data.ses.stud$Student==6 |fit.nlin.data.ses.stud$Student==7|fit.nlin.data.ses.stud$Student==8, ])+theme(legend.position="none")+
  geom_line(data = fit.nlin.data.ses.stud[fit.nlin.data.ses.stud$Student==6 |fit.nlin.data.ses.stud$Student==7|fit.nlin.data.ses.stud$Student==8, ],aes(x=Wtime,y=fit.gam,group=Student),colour="red")+
  labs(x="Within-time",y="Ability")+ggtitle("Estimated trend by GLMM for learner 6,7 and 8 in Session 1")


#Putting plots on grid
library(gridExtra)
grid.arrange(p_simlin.rm160,p.gam_lin,p_simlin.stud,p.gam_lin.stud, nrow = 2,ncol=2)
grid.arrange(p.glm_nlin,p.gam_nlin,p.glm_nlin.stud,p.gam_nlin.stud, nrow = 2,ncol=2)
plot(fitted(gam.lin.ab)~data.seq$wtime,col=data.seq$student)


#Evaluation of models using MSE
#MSE of linear relationship estimated by GAM
mse_gam160.lin=matrix(data=NA,nrow=1,ncol = 4,dimnames = list(c("GAM"),c("session 1","session 2","session 3","session 4")))
for(i in 1:4){
  mse_gam160.lin[1,i]=mean((data.seq.ses2$abil.w.lin[data.seq.ses2$Session==i]-
                              fitted(gam.lin.ab)[data.seq.ses2$Session==i])^2)
}


#MSE of linear relationship estimated by GLMM
mse_glm160.lin=matrix(data=NA,nrow=1,ncol = 4,dimnames = list(c("GLMM"),c("session 1","session 2","session 3","session 4")))
for(i in 1:4){
  mse_glm160.lin[1,i]=mean((data.seq.ses2$abil.w.lin[data.seq.ses2$Session==i]-
                              fitted(glm.lin.ab)[data.seq.ses2$Session==i])^2)
}

#MSE of non-linear relationship estimated by GAM
mse_gam160.nlin=matrix(data=NA,nrow=1,ncol = 4,dimnames = list(c("GAM"),c("session 1","session 2","session 3","session 4")))
for(i in 1:4){
  mse_gam160.nlin[1,i]=mean((data.seq.ses2$abil.w.nlin[data.seq.ses2$Session==i]-
                              fitted(gam.nlin.ab)[data.seq.ses2$Session==i])^2)
}

#MSE of non-linear relationship estimated by GLMM
mse_glm160.nlin=matrix(data=NA,nrow=1,ncol = 4,dimnames = list(c("GLMM"),c("session 1","session 2","session 3","session 4")))
for(i in 1:4){
  mse_glm160.nlin[1,i]=mean((data.seq.ses2$abil.w.nlin[data.seq.ses2$Session==i]-
                               fitted(glm.nlin.ab)[data.seq.ses2$Session==i])^2)
}



