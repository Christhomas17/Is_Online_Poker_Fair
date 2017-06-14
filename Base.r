#Working directory changed to remove name of file
setwd("C:/Users/-------")

#OLD
a<-Sys.time()
data<-read.csv("PokerRingSlim.csv",header=T)
data<-data[,-c(1)]
list<-which(duplicated(data$Round_ID))
data2<-data[-list,]
write.csv(data2,"pokerRingSlimUnique.csv",row.names=F)
b<-Sys.time()
b-a
rm(b,a,list)


data<-read.csv("PokerRingSlimUnique.csv",header=T)
noblank<-data[-which(data$TableCards==""),]
write.csv(noblank,"noblanks.csv",row.names=F)
table<-as.data.frame(noblank$TableCards)
colnames(table)<-c("TableCards")

write.csv(table,"TableCards.csv",row.names=F)
write.table(table,"Table.txt",row.names=F)
####END OLD####

###START ANALYSIS###
cards<-read.csv("SepTableCards.csv",header=T)
flop<-cards[,-c(1,5,6)]
adjust<-(flop-1)%%13
write.csv(adjust,"AdjustedFlop.csv",row.names=F)

pair<-function(x,y,z){if(x==y||x==z||y==z){return(T)}else{return(F)}}

##Test small sample to make sure working correctly
test<-head(adjust,100)
test1<-data.frame(apply(test, 1,function(x) pair(x[1],x[2],x[3])))
colnames(test1)<-c("IsPair")
comb<-cbind(test,test1)
comb2<-cbind(head(flop,100),comb)
p<-which(test1==T);np<-which(test1==F)
P.pair<-length(p)/(length(p)+length(np))
P.pair ## =0.15
##End, back to full analysis

ispair<-data.frame(apply(adjust,1,function(x) pair(x[1],x[2],x[3])))
colnames(ispair)<-c("IsPair")
p<-which(ispair==T);np<-which(ispair==F)
p.pair<-length(p)/(length(p)+length(np))
p.pair

##Function to partition data and cound pairs/non-pairs
part<-function(x,y){
  temp.p<-length(which(ispair[x:y,1]==T))
  temp.np<-length(which(ispair[x:y,1]==F))
  df<-data.frame(x1<-temp.p,x2<-temp.np)
  colnames(df)<-c("Pair","NoPair")
  return(df)
}

n<-10000
m<-floor(length(ispair$IsPair)/n)
for(i in 1:m){
  min<-(i-1)*n+1
  max<-i*n
  partition<-part(min,max)
  if(i==1){
	samp<-partition}
  else{
	samp<-rbind(samp,partition)}
}

prop<-data.frame(apply(samp,1,function(x) (x[1]/(x[1]+x[2]))))


plot(prop[,1],xlab="Sample",ylab="Prop of Pair",main="Scatter Plot of Proportion of Pairs on the Flop(10,000 per sample)")

###Test using patrons sample size
n<-3000
m<-floor(length(ispair$IsPair)/n)
for(i in 1:m){
  min<-(i-1)*n+1
  max<-i*n
  partition<-part(min,max)
  if(i==1){
	samp<-partition}
  else{
	samp<-rbind(samp,partition)}
}

prop<-data.frame(apply(samp,1,function(x) (x[1]/(x[1]+x[2]))))
test<-t.test(prop[,1],mu=0.1718,conf.level=.95)
test

plot(prop[,1],xlab="Sample",ylab="Prop of Pair",main="Scatter Plot of Proportion of Pairs on the Flop(3,000 per sample)")


##Random samples of 3,000
s<-3000
n<-1000
for(i in 1:n){
  x<-sample(ispair[,1],s)
  a<-length(which(x==T))
  b<-length(which(x==F))
  partition<-data.frame(x1<-a,x2<-b)
  colnames(dt)<-c("Pair","NoPar")
  if(i==1){
	samp<-partition}
  else{
	samp<-rbind(samp,partition)}
}

prop<-data.frame(apply(samp,1,function(x) (x[1]/(x[1]+x[2]))))
test<-t.test(prop[,1],mu=0.1718,conf.level=.95)
test
plot(prop[,1],xlab="Sample",ylab="Prop of Pair",main="Scatter Plot of Proportion of Pairs on the Flop(3,000 per random sample)")


##Random samples of 10,000
s<-10000
n<-1000
for(i in 1:n){
  x<-sample(ispair[,1],s)
  a<-length(which(x==T))
  b<-length(which(x==F))
  partition<-data.frame(x1<-a,x2<-b)
  colnames(dt)<-c("Pair","NoPar")
  if(i==1){
	samp<-partition}
  else{
	samp<-rbind(samp,partition)}
}

prop<-data.frame(apply(samp,1,function(x) (x[1]/(x[1]+x[2]))))
test<-t.test(prop[,1],mu=0.1718,conf.level=.95)
test
plot(prop[,1],xlab="Sample",ylab="Prop of Pair",main="Scatter Plot of Proportion of Pairs on the Flop(10,000 per random sample)")



