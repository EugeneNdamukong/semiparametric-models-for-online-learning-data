
library(lme4)
library(mgcv)
library(ggplot2)
############# Intercept + Slope #############

#Simulation parameters: Intercept Female=0.5 ;Intercept Male= -2.5; 
#slope Female=0.18; slope male=0.05; var(student intercept)=0.5; 
#var(student slope)=.0008

#Data to be model after adding random noise
data.seq.rand2=data.frame(data.seq.rand,
                          abil.bw.lin=data.seq.rand$trueability.bw.lin+rnorm(nrow(data.seq2),sd=0.05),
                          abil.bw.nlin=data.seq.rand$trueability.bw.nlin+rnorm(nrow(data.seq2),sd=0.05),
                          abil.w.lin=data.seq.rand$trueability.w.lin+rnorm(nrow(data.seq2),sd=0.05),
                          abil.w.nlin=data.seq.rand$trueability.w.nlin+rnorm(nrow(data.seq2),sd=0.05))



###############################################################################################
################################ CASE TWO #####################################################

#Ability versus within time with GLMM
glm.ab.bw=glmer(abil.bw.lin~ + wtime+btime+(1+wtime+btime|student),
             data = data.seq.rand2,family = gaussian(link = "identity"))
summary(glm.ab.bw)


#Ability versus within time using GAM 

gam.lin.ab.bw=bam(abil.bw.lin~s(student,bs='re')+s(wtime,bs='cr')+
                    s(wtime,student,bs='re')+ s(btime,bs='cr')+s(btime,student,bs='re'),
                  family=gaussian(link = "identity"),data=data.seq.rand2, method = "REML")
summary(gam.lin.ab.bw)
confint(gam.lin.ab.bw)
gam.vcomp(gam.lin.ab.bw)
par(mfrow=c(2,2))
gam.check(gam.lin.ab.bw)

#Confidence intervals
coef.bw=coef(gam.lin.ab.bw)
Var.bw <- vcov(gam.lin.ab.bw, unconditional = TRUE)
se.bw <- sqrt(diag(Var.bw))
i <- which(names(coef.bw) == "(Intercept)")
coef.bw[i] + (c(-1,1) * (2 * se.bw[i]))


### Preparing data for fitting ###
library(doBy)
#For GAM
fitted.ab=fitted(gam.lin.ab.bw)
fit.data=data.frame(data.seq.rand2,fit.gam=fitted.ab)
fit.data=orderBy(~-wtime,fit.data)

#For GLMM
fitted.ab.glmm=fitted(glm.ab.bw)
fit.data_lin160=data.frame(data.seq.rand2,fit.gam=fitted.ab.glmm)
fit.data_lin160=orderBy(~-wtime,fit.data_lin160)


#Simulated linear within time trend 
ggplot(data.seq.rand, aes(x=wtime, y=trueability.w.lin)) + geom_point(colour=data.seq.rand$session)+
  labs(x="Within-time",y="Ability")

#Linear plots estimated by GAM
p_rm=ggplot(data=fit.data,aes(x=wtime,y=trueability.bw.lin))+
  geom_point(data=fit.data)+theme(legend.position="none")+
  geom_line(data = fit.data,aes(x=wtime,y=fit.gam,group=fit.data$student),colour="red")+
  labs(x="Within-time",y="Ability")+ggtitle("Estimated by GAM")

#Linear plots of 3 learners in session 1 estimated by GAM
fit.data.stud=subset(fit.data,session==1)
p_rm.stud=ggplot(data=fit.data.stud[fit.data.stud$student==6|fit.data.stud$student==7|fit.data.stud$student==8,],aes(x=wtime,y=trueability.bw.lin))+
  geom_point(data=fit.data.stud[fit.data.stud$student==6|fit.data.stud$student==7|fit.data.stud$student==8,])+theme(legend.position="none")+
  geom_line(data = fit.data.stud[fit.data.stud$student==6|fit.data.stud$student==7|fit.data.stud$student==8,],aes(x=wtime,y=fit.gam,group=fit.data.stud$student[fit.data.stud$student==6|fit.data.stud$student==7|fit.data.stud$student==8]),colour="red")+
  labs(x="Within-time",y="Ability")+ggtitle("Estimated by GAM by learner 6, 8, 7 in session 1")



#Linear plots estimated by GLMM
p_lin160=ggplot(data=fit.data_lin160,aes(x=wtime,y=trueability.bw.lin))+
  geom_point(data=fit.data_lin160)+theme(legend.position="none")+
  geom_line(data = fit.data_lin160,aes(x=wtime,y=fit.gam,group=fit.data_lin160$student),colour="red")+
  labs(x="Within-time",y="Ability")+ggtitle("Estimated by GLMM")

#Linear plots of 3 learners in session 1 estimated by GLMM
fit.data_lin.stud=subset(fit.data_lin160,session==1)
p_lin160.stud=ggplot(data=fit.data_lin.stud[fit.data_lin.stud$student==6|fit.data_lin.stud$student==7|fit.data_lin.stud$student==8,],aes(x=wtime,y=trueability.bw.lin))+
  geom_point(data=fit.data_lin.stud[fit.data_lin.stud$student==6|fit.data_lin.stud$student==7|fit.data_lin.stud$student==8,])+theme(legend.position="none")+
  geom_line(data = fit.data_lin.stud[fit.data_lin.stud$student==6|fit.data_lin.stud$student==7|fit.data_lin.stud$student==8,],aes(x=wtime,y=fit.gam,group=fit.data_lin.stud$student[fit.data_lin.stud$student==6|fit.data_lin.stud$student==7|fit.data_lin.stud$student==8]),colour="red")+
  labs(x="Within-time",y="Ability")+ggtitle("Estimated by GLMM by learner 6,8,7 in session 1")


#Putting plots on grid
install.packages("gridExtra")
library(gridExtra)
grid.arrange(p_rm,p_lin160,p_rm.stud,p_lin160.stud, nrow = 2,ncol=2)


### MSE of ability ###
#GAM
mse_gam160=matrix(data=NA,nrow=1,ncol = 4,dimnames = list(c("females"),c("session 1","session 2","session 3","session 4")))
for(i in 1:4){
  mse_gam160[1,i]=mean((data.seq.rand2$trueability.bw.lin[data.seq.rand2$session==i]-
                          fitted.ab[data.seq.rand2$session==i])^2)
}


#GLM
mse_glm160=matrix(data=NA,nrow=1,ncol = 4,dimnames = list(c("females"),c("session 1","session 2","session 3","session 4")))
for(i in 1:4){
  mse_glm160[1,i]=mean((data.seq.rand2$trueability.bw.lin[data.seq.rand2$session==i]-
                          fitted.ab.glmm[data.seq.rand2$session==i])^2)
}










