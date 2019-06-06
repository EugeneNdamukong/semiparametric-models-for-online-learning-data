######################################################################################################################
###################################### SIMULATION OF CASE ONE ########################################################
library(boot)
install.packages("mvtnorm")
library(mvtnorm)
install.packages("matrixStats")
library(matrixStats)
install.packages("lme4")
library(lme4)
install.packages("ltm")
library(ltm)
install.packages("R2OpenBUGS")
library(R2OpenBUGS)
install.packages("R2jags")
library(R2jags) 
install.packages("ggplot2")
library(ggplot2)

N.student<-50
N.item<-160
beta.item<- rnorm(N.item,0,2) 
save(beta.item, file="beta.item.RData")
N.session <- 4
item.session <- 40 # num of items solved in each session
N.answer <- N.session*item.session 
gender<- sample(c("M","F"), N.student, replace=T)
#session=sample(c("ses1","ses2","ses3","ses4"), N.item, replace=T)
session=rep(c("ses1","ses2","ses3","ses4"),c(item.session,item.session,item.session,item.session))
table(session)
session=sort(session)
save(gender, file="gender.RData")  
individual.item.time.par<-0.05


# Random effect param 
mean.w<-rep(0,5)
sigma.w<-(diag(c(0.5, 0.0008, 0.008, 0.0007, 0.0005)))


# Initial ability 
female.const<- 0.5  # 'a00'
male.const<- -2.5 #-0.2   # 'a01'  
gender.constant.effect<- ifelse(gender=="M", male.const, female.const)

ses1.const=-0.06
ses2.const=0.35
ses3.const=0.45
ses4.const=0.60
ses.constant.effect= ifelse(session=="ses1", ses1.const, 
                            ifelse(session=="ses2",ses2.const,
                                   ifelse(session=="ses3",ses3.const,ses4.const)))

# Within-session linear slope  
female.slope.wtime<- 0.12 #0.01  # 'a10'
male.slope.wtime<- 0.05 #0.003   # 'a11' 
gender.wtime.effect<-ifelse(gender=="M",male.slope.wtime,female.slope.wtime)

ses1.slope.wtime=0.08
ses2.slope.wtime=0.20
ses3.slope.wtime=0.40
ses4.slope.wtime=0.60
ses.wtime.effect= ifelse(session=="ses1", ses1.slope.wtime, 
                         ifelse(session=="ses2",ses2.slope.wtime,
                                ifelse(session=="ses3",ses3.slope.wtime,ses4.slope.wtime)))


# Within-session non-linear slope  
female.nl.slope.wtime<- 0.20 #0.01  # 'a10'
male.nl.slope.wtime<- 0.09 #0.003   # 'a11' 
gender.wtime.nl.effect<-ifelse(gender=="M",male.nl.slope.wtime, female.nl.slope.wtime)

ses1.nl.slope.wtime=0.1
ses2.nl.slope.wtime=0.3
ses3.nl.slope.wtime=0.5
ses4.nl.slope.wtime=0.7
ses.wtime.nl.effect= ifelse(session=="ses1", ses1.nl.slope.wtime, 
                            ifelse(session=="ses2",ses2.nl.slope.wtime,
                                   ifelse(session=="ses3",ses3.nl.slope.wtime, ses4.nl.slope.wtime)))


# Random effects 
w.student<-rmvnorm(N.student, mean=mean.w, sigma=sigma.w) 


# time stamp  
session.start.time <- rpois(N.item, 30) 
btime <- rep(0, N.item) 
prevSession.wtime <- rep(0, N.student) # time passed until the end of last session



# "between.session.time" : time spent between sessions
# "individual.item.time" : time spent to solve the item
# "item.time" : accumulated time spent within the current session
# "wtime" : accumulated time spent within sessions
# "btime" : accumulated time spent between sessions

#Creation of dummy matrix
session.numb.items=as.vector(table(session))
ses1.dum=rep(c(1,0,0,0),c(session.numb.items[1],session.numb.items[2],session.numb.items[3],session.numb.items[4]))
ses2.dum=rep(c(0,1,0,0),c(session.numb.items[1],session.numb.items[2],session.numb.items[3],session.numb.items[4]))
ses3.dum=rep(c(0,0,1,0),c(session.numb.items[1],session.numb.items[2],session.numb.items[3],session.numb.items[4]))
ses4.dum=rep(c(0,0,0,1),c(session.numb.items[1],session.numb.items[2],session.numb.items[3],session.numb.items[4]))

ses.dum.mat=cbind(ses1.dum,ses2.dum,ses3.dum,ses4.dum)
dim(ses.dum.mat)

data.tmp<-matrix(NA, nrow=1, ncol=4)  
colnames(data.tmp)<- c("Student", "Session", "Item.no",
                        "Wtime")
#data.tmp<-as.data.frame(data.tmp)
ses.abil.l.vec=c(NA)
ses.abil.nl.vec=c(NA)

data<- NULL
for(j in 1:N.student){
  
  #items.answered <- sample.int(N.item, item.session) # randomly choose items within each session  
    
  individual.item.time <- rep(individual.item.time.par,N.item) #rnorm(item.session, 0.24) 
  item.time <- cumsum(individual.item.time)
  wtime <- item.time
   
    
  #Linear equation  
  true.ability.l <-  (rep(ses.constant.effect[1:item.session],4)+ses.constant.effect*ses.dum.mat[,2]+ 
                        ses.constant.effect*ses.dum.mat[,3]+ses.constant.effect*ses.dum.mat[,4] + 
                        w.student[j,1])+(ses.wtime.effect*ses.dum.mat[,1] + ses.wtime.effect*ses.dum.mat[,2] +
                                                                                     ses.wtime.effect*ses.dum.mat[,3]+ 
                                                                                        ses.wtime.effect*ses.dum.mat[,4] +w.student[j,2])*wtime
    
  ses.abil.l.vec=append(ses.abil.l.vec,true.ability.l)  
  
  #Non-linear equation 
  true.ability.nl <-  (rep(ses.constant.effect[1:item.session],4)+ses.constant.effect*ses.dum.mat[,2]+ ses.constant.effect*ses.dum.mat[,3]+ses.constant.effect*ses.dum.mat[,4] + w.student[j,1])+(ses.wtime.effect*ses.dum.mat[,1] + ses.wtime.effect*ses.dum.mat[,2] +
                                                                                      ses.wtime.effect*ses.dum.mat[,3]+ 
                                                                                         ses.wtime.effect*ses.dum.mat[,4] + 
                                                                                            w.student[j,2])*wtime + (ses.wtime.effect*ses.dum.mat[,1]+
                                                                                                                       ses.wtime.effect*ses.dum.mat[,2] +
                                                                                                                       ses.wtime.effect*ses.dum.mat[,3]+ 
                                                                                                                          ses.wtime.effect*ses.dum.mat[,4] + 
                                                                                                                             w.student[j,4])*sin(wtime)    
      
   
  ses.abil.nl.vec=append(ses.abil.nl.vec,true.ability.nl)  
    
  #Columns of data matrix
  Student<- rep(j,N.item)
  Session<- rep(c(1,2,3,4),c(item.session,item.session,item.session,item.session))
  Item.no<- 1:N.item
  Wtime<- wtime
  
  
  data.tmp=rbind(data.tmp,cbind(Student, Session, Item.no,
                                Wtime))
}
  
  
data <- rbind(data,data.tmp)
  
#Creating of data structure 
data.seq.ses <- as.data.frame(na.omit(data))
data.seq.ses=data.frame(data.seq.ses,Trueability.w.lin=na.omit(ses.abil.l.vec), 
                        Trueability.w.nlin=na.omit(ses.abil.nl.vec))

#Converting categorical variables into factors
data.seq.ses$Student=as.factor(data.seq.ses$Student)
data.seq.ses$Session=as.factor(data.seq.ses$Session)
data.seq.ses$Item.no=as.factor(data.seq.ses$Item.no)


#Check the simulated relationship graphically 
ggplot(data.seq.ses, aes(x=Wtime, y=Trueability.w.nlin)) + 
  geom_point(colour=data.seq.ses$Session)+labs(x="Within-session time",y="Ability",fill="Sessions")+
  theme(legend.position="top")

ggplot(data.seq.ses, aes(x=Wtime, y=Trueability.w.lin)) + 
  geom_point(colour=data.seq.ses$Session)+labs(x="Within-session time",y="Ability",fill="Sessions")+
  theme(legend.position="top")






  