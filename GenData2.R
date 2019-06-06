######################################################################################################################
###################################### SIMULATION OF CASE TWO ########################################################
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
session=rep(c("ses1","ses2","ses3","ses4"),c(40,40,40,40))
table(session)
session=sort(session)
save(gender, file="gender.RData")  
individual.item.time.par<-0.05


# Random effect param 
mean.w<-rep(0,5)
sigma.w<-(diag(c(0.5, 0.0008, 0.008, 0.0007, 0.0008)))


# Random effects 
w.student<-rmvnorm(N.student, mean=mean.w, sigma=sigma.w) 


# time stamp  
session.start.time <- rpois(N.student, 30) 
btime <- rep(0, N.student) 
prevSession.wtime <- rep(0, N.student) # time passed until the end of last session



# "between.session.time" : time spent between sessions
# "individual.item.time" : time spent to solve the item
# "item.time" : accumulated time spent within the current session
# "wtime" : accumulated time spent within sessions
# "btime" : accumulated time spent between sessions



data.tmp<-matrix(NA, nrow=N.student*item.session, ncol=16)  
colnames(data.tmp)<- c("student", "gender", "session", "item.no",
                       "session.start.time", "between.session.time", "individual.item.time", 
                       "actual.time", "btime", "wtime","trueability.bw.lin",
                       "trueability.bw.nlin", "truedifficulty", "item",
                       "response.bw.lin","response.bw.nlin")
data.tmp<-as.data.frame(data.tmp)

data<- NULL
for(j in 1:N.session){
  for(i in 1:N.student){
    items.answered <- sample.int(N.item, item.session) # randomly choose items within each session  
    
    individual.item.time <- rep(individual.item.time.par,item.session) #rnorm(item.session, 0.24) 
    item.time <- cumsum(individual.item.time)    
    wtime <-prevSession.wtime[i] + item.time
    
    
    
    between.session.time <- ifelse(j==1,0,rpois(1,24))/24
    btime[i] <- btime[i] + between.session.time 
    session.start.time[i] <- session.start.time[i] + between.session.time  
    
    
    #sigma.w<-(diag(c(0.5, 0.0008, 0.0005, 0.0001, 0.0003)))
    
 
    #Linear equation  
    true.ability.bw.l <-  (0.15 + w.student[i,1])+
      (0.06 + w.student[i,2])*wtime + 
      (0.003 + w.student[i,4])*btime[i] 
   
    #Non-linear equation
    true.ability.bw.nl <-  (0.18 + w.student[i,1])+
      (0.06 + w.student[i,2])*wtime + 
      (0.08 + w.student[i,3])*sin(wtime)+
      ( 0.003+ w.student[i,4])*btime[i] + 
      (0.008 + w.student[i,5])*sin(btime[i])
    
    
    #Success probabilities
    prob.correct.bw.l <- inv.logit(true.ability.bw.l - beta.item[items.answered])
    prob.correct.bw.nl <- inv.logit(true.ability.bw.nl - beta.item[items.answered])
    
    #Binary responses
    response.bw.l <- rbinom(item.session,1,prob.correct.bw.l)
    response.bw.nl <- rbinom(item.session,1,prob.correct.bw.nl)
    
    
    data.tmp[((i-1)*item.session+1):(i*item.session),1]<- rep(i,item.session)
    data.tmp[((i-1)*item.session+1):(i*item.session),2]<- rep(gender[i],item.session)
    data.tmp[((i-1)*item.session+1):(i*item.session),3]<- rep(j,item.session)
    data.tmp[((i-1)*item.session+1):(i*item.session),4]<- 1:item.session
    data.tmp[((i-1)*item.session+1):(i*item.session),5]<- rep(session.start.time[i],item.session)
    data.tmp[((i-1)*item.session+1):(i*item.session),6]<- rep(between.session.time,item.session)
    data.tmp[((i-1)*item.session+1):(i*item.session),7]<- individual.item.time
    data.tmp[((i-1)*item.session+1):(i*item.session),8]<- session.start.time[i]+item.time
    data.tmp[((i-1)*item.session+1):(i*item.session),9]<- rep(btime[i],item.session)
    data.tmp[((i-1)*item.session+1):(i*item.session),10]<- wtime
    data.tmp[((i-1)*item.session+1):(i*item.session),11]<- true.ability.bw.l
    data.tmp[((i-1)*item.session+1):(i*item.session),12]<- true.ability.bw.nl
    data.tmp[((i-1)*item.session+1):(i*item.session),13]<- beta.item[items.answered]
    data.tmp[((i-1)*item.session+1):(i*item.session),14]<- items.answered
    data.tmp[((i-1)*item.session+1):(i*item.session),15]<- response.bw.l
    data.tmp[((i-1)*item.session+1):(i*item.session),16]<- response.bw.nl
    
    
    session.start.time[i] <- session.start.time[i]+sum(individual.item.time)
    prevSession.wtime[i] <- prevSession.wtime[i]+sum(individual.item.time)
    }
  data <- rbind(data,data.tmp)
  }

data.seq.rand <- data[order(data$actual.time),] 
#Converting categorical variables into factors
data.seq.rand$student=as.factor(data.seq.rand$student)
data.seq.rand$session=as.factor(data.seq.rand$session)
data.seq.rand$item=as.factor(data.seq.rand$item)
data.seq.rand$gender=as.factor(data.seq.rand$gender)
data.seq.rand$item.no=as.factor(data.seq.rand$item.no)

View(orderBy(~session,data.seq.rand))


#Description of simmulated data
for(i in 1:4){
  print(mean(data.seq.rand$trueability.bw.nlin[data.seq.rand$session==i]))
}

for(i in 1:4){
  print(range(data.seq.rand$btime[data.seq.rand$session==i]))
}

#Ranges of between session time
b1=c(0.625000, 1.416667)
b2=c(1.333333, 2.791667)
b3=c(2.416667, 3.708333)
mat.b=rbind(b1,b2,b3)
#Mean abilities between sessions
for(i in 1:3){
  print(mean(data.seq.rand$trueability.bw.lin[data.seq.rand$btime>=mat.b[i,1]&data.seq.rand$btime<=mat.b[i,2]]))
  
}

#Graphical inspection of simulated relationship
ggplot(data.seq.rand, aes(x=wtime, y=trueability.bw.lin)) + 
  geom_point(colour=data.seq.rand$session)+labs(x="Within-time",y="Ability",fill="Sessions")+
  theme(legend.position="top")


