####################################################################################################################
###################### ANALYSIS OF THE REAL-LIFE DATA ############################################

#Importing data
math.data=read.csv(file.choose(),header = T)
names(math.data)

#Some data manipulation
Gender=ifelse(math.data$gender=="Man","Male",
              ifelse(math.data$gender=="Vrouw","Female","none"))
Gender=as.factor(Gender)

#Oefeningen     Kennisvragen   Inzichtsvragen
ItemType=ifelse(math.data$itemtype=="Oefeningen","Exercises",
                ifelse(math.data$itemtype=="Kennisvragen","Knowledge_questions",
                       ifelse(math.data$itemtype=="Inzichtsvragen","Insight_questions","none")))
ItemType=as.factor(ItemType)

math.data=data.frame(math.data,Gender=Gender, ItemType=ItemType)

math.data$iduser=as.factor(math.data$iduser)
math.data$iditem=as.factor(math.data$iditem)
math.data$ses=as.factor(math.data$ses)

#Some basic descriptive statistics 
sum_mat=matrix(data=NA,nrow=7,ncol = 5)
for(i in 1:7){
  sum_mat[i,1]=i
  sum_mat[i,2]=min(math.data$wtime[math.data$ses==i])
  sum_mat[i,3]=median(math.data$wtime[math.data$ses==i])
  sum_mat[i,4]=mean(math.data$wtime[math.data$ses==i])
  sum_mat[i,5]=max(math.data$wtime[math.data$ses==i])
}

#Scatter plot
library(GGally)
ggpairs(math.data[,c("grade","ses","wtime",
                    "btime","item")],columnLabels = c("Response","Session","Within-time","Between-time","Item"))
#Eliminating sessions 5, 6 and 7 because of less observations
table(as.factor(math.data$ses))
math.sub=subset(math.data,ses==1|ses==2|ses==3|ses==4)
math.sub2=subset(math.data,ses==1|ses==2)
table(as.factor(math.sub$ses))
nrow(math)

#GAM vs GLM
library(mgcv)
library(lme4)
#Intercept model
math.gam.int=gam(grade~ 1+s(iditem,bs="re")+s(iduser,bs="re"),family = binomial(link = "logit"),data=math.data,method = "P-REML")
summary(math.gam.int)
gam.vcomp(math.gam.int)
BIC(math.gam.int)

math.glm.int= glmer(grade~(1|iduser)+(1|iditem),data=math.sub,family = binomial(link = "logit") )
summary(math.glm.int)
BIC(math.glm.int, math.gam.int)



##### CASE ONE : WITHIN AND SESSIONS #####

#GAM with cubic regression spline
math.gam.s=bam(grade~ ses+ s(iduser,by=ses,bs="re") +s(wtime,by=ses,bs="cr")+s(wtime,iduser,by=ses,bs="re")+
                 s(iditem,bs="re"),family = binomial(link = "logit"),
               data = math.sub,method = "REML")
summary(math.gam.s)
gam.vcomp(math.gam.s)
par(mfrow=c(2,2))
gam.check(math.gam.s)


#GAM with thin plate regression spline
math.gam.s2=bam(grade~ ses+ s(iduser,by=ses,bs="re") +s(wtime,by=ses,bs="tp")+s(wtime,iduser,by=ses,bs="re")+
                  s(iditem,bs="re"),family = binomial(link = "logit"),
                data = math.sub,method = "REML")
summary(math.gam.s2)
gam.vcomp(math.gam.s2)
par(mfrow=c(2,2))
gam.check(math.gam.s2)


#GLMM 
math.glm.s=glmer(grade~ ses+ wtime:ses+(ses+ses:wtime|iduser) +(1|iditem),
                 data=math.sub, family = binomial(link = "logit"))
summary(math.glm.s)


AIC(math.gam.s,math.gam.s2,math.glm.s)
BIC(math.gam.s,math.gam.s2,math.glm.s)


#Plotting of best spline
install.packages("itsadug")
library(itsadug)
plot.gam(math.gam.s,select=5,ylab = "Ability in first session",
         xlab="Within-session time",scale = 0,shade = TRUE,col ="blue")
plot.gam(math.gam.s,select=6,scale = 0,ylab = "Ability in second session",
         xlab="Within-session time",shade = TRUE,col="blue")
plot.gam(math.gam.s,select=7,scale = 0,ylab = "Ability in third session",
         xlab="Within-session time",shade = TRUE,col="blue")
plot.gam(math.gam.s,select=8,scale = 0,ylab = "Ability in fourth session",
         xlab = "Within-session time",shade = TRUE,col="blue")

par(mfrow=c(3,4))
ses_vec=c("1","2","3","4")
for(j in user_vec){
  for(i in ses_vec){
    plot_smooth(math.gam.s2, view = "wtime",cond=list(iduser=j,ses=i),rm.ranef=FALSE,col = "blue", 
                xlab = paste("within-session ", i," time"),ylab=paste("Ability of learner ",j))
    
  }
}


library(voxel)
plotGAM(math.gam.s,smooth.cov = "wtime")




#### CASE TWO : WITHIN AND BETWEEN TIME TREND #########

#GAM estimating within time trend using cubic regression spline
math.gam.wt=gam(grade~ 1 + s(iduser,bs="re") +s(wtime,bs="cr")+s(wtime,iduser,bs="re")+s(iditem,bs="re"),family = binomial(link = "logit"),
                data = math.sub,method = "REML")
summary(math.gam.wt)
gam.vcomp(math.gam.wt)
BIC(math.gam.wt)

#GAM estimating within time trend using thin-plate regression spline
math.gam.wt2=bam(grade~ 1 + s(iduser,bs="re") +s(wtime,bs="tp")+s(wtime,iduser,bs="re")+s(iditem,bs="re"),family = binomial(link = "logit"),
                 data = math.sub,method = "REML")
summary(math.gam.wt2)
gam.vcomp(math.gam.wt2)

#GAM estimating within and between time trend using cubic regression spline
math.gam.wb=bam(grade~ 1 + s(iduser,bs="re") +s(wtime,bs="cr")+s(wtime,iduser,bs="re")+s(btime,bs="cr")+s(btime,iduser,bs="re")
                 +s(iditem,bs="re"),family = binomial(link = "logit"),
                data = math.sub,method = "REML")
summary(math.gam.wb)
gam.vcomp(math.gam.wb)
par(mfrow=c(2,2))
gam.check(math.gam.wb)
inv.logit(0.2584)


#GAM estimating within and between time trend using thin plate regression spline
math.gam.wb2=bam(grade~ 1 + s(iduser,bs="re") +s(wtime,bs="tp")+s(wtime,iduser,bs="re")+s(btime,bs="tp")+s(btime,iduser,bs="re")
                 +s(iditem,bs="re"),family = binomial(link = "logit"),
                 data = math.sub,method = "REML")
summary(math.gam.wb)
gam.vcomp(math.gam.wb)


#GLMM estimating within time and between time trend
math.glm.wb=glmer(grade~ 1 + wtime + btime+(1+btime+wtime|iduser) +(1|iditem),data=math.sub, family = binomial(link = "logit"))
summary(math.glm.wb)

#math.glm.wt    6.0000 3850.502
#math.gam.wt2 245.4765 5207.146
AIC(math.glm.wb,math.gam.wb,math.gam.wb2)


#Plotting best splines 
par(mfrow=c(1,2))
plot.gam(math.gam.wb,select=2,scale = 0,ylab = "Ability ",
         xlab="Within-session time",shade = TRUE,col ="blue")
plot.gam(math.gam.wb,select=4,scale = 0,ylab = "Ability",
         xlab="Between-session time",shade = TRUE,col ="blue")

user_vec=c("6","7","8")
par(mfrow=c(2,3))
for(i in user_vec){
  plot_smooth(math.gam.wb, view = "btime",cond=list(iduser=i),rm.ranef=FALSE,col = "blue", 
              xlab = "between-session time",ylab=paste("Ability of learner ",i))
  
}




#### EVALUATION OF MODELS ####
#MCR for case 1
gam.s.fit=fitted(math.gam.s2)
glm.s.fit=fitted(math.glm.s)

pred.gam.s=ifelse(gam.s.fit>=0.5,1,0)
pred.glm.s=ifelse(glm.s.fit>=0.5,1,0)

mcr.gam.vec2=vector(mode="numeric",length = 4)
mcr.glm.vec2=vector(mode="numeric",length = 4)

tab.gam.mat.s.all=table(pred.gam.s,math.sub$grade)
tab.glm.mat.s.all=table(pred.glm.s,math.sub$grade)

1-sum(diag(tab.gam.mat.s.all))/sum(tab.gam.mat.s.all)
1-sum(diag(tab.glm.mat.s.all))/sum(tab.glm.mat.s.all)

for(i in 1:4){
  tab.gam.mat.s=table(pred.gam.s[math.sub$ses==i],math.sub$grade[math.sub$ses==i])
  tab.glm.mat.s=table(pred.glm.s[math.sub$ses==i],math.sub$grade[math.sub$ses==i])
  
  mcr.gam.vec2[i]=1-sum(diag(tab.gam.mat.s))/sum(tab.gam.mat.s)
  mcr.glm.vec2[i]=1-sum(diag(tab.glm.mat.s))/sum(tab.glm.mat.s)
  
}



#MCR for case 2
gam.wt.fit=fitted(math.gam.wb)
glm.wt.fit=fitted(math.glm.wb)

pred.gam.wt=ifelse(gam.wt.fit>=0.5,1,0)
pred.glm.wt=ifelse(glm.wt.fit>=0.5,1,0)

tab.gam.mat=table(pred.gam.wt,math.sub$grade[math.sub$ses==4])
tab.gam.cr.mat=table(pred.gam.cr.wt,math.sub$grade)
tab.glm.mat=table(pred.glm.wt,math.sub$grade[math.sub$ses==4])

mcr.gam.vec=vector(mode = "numeric",length = 4)
mcr.glm.vec=vector(mode = "numeric",length = 4)

for(i in 1:4){
  tab.gam.mat=table(pred.gam.wt[math.sub$ses==i],math.sub$grade[math.sub$ses==i])
  tab.glm.mat=table(pred.glm.wt[math.sub$ses==i],math.sub$grade[math.sub$ses==i])
  
  mcr.gam.vec[i]=1-sum(diag(tab.gam.mat))/sum(tab.gam.mat)
  mcr.glm.vec[i]=1-sum(diag(tab.glm.mat))/sum(tab.glm.mat)
  
}

tab.gam.mat.a=table(pred.gam.wt,math.sub$grade)
tab.glm.mat.a=table(pred.glm.wt,math.sub$grade)

1-sum(diag(tab.gam.mat.s))/sum(tab.gam.mat.s)
1-sum(diag(tab.glm.mat.s))/sum(tab.glm.mat.s)



#Extracting the difficulties from GAM
idx <-grep("iditem", names(coef(math.gam.wt2)))
diff.rr.gam<-coef(math.gam.wt2)[idx]
attributes(diff.rr.gam)<-NULL
which.min(diff.rr.gam)
diff.rr.gam[30]
which.max(diff.rr.gam)
diff.rr.gam[32]




