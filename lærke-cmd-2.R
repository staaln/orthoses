setwd("/Users/stalenygard/lærke/data-2/MedGas")


set.seed(42);

nm <- list.files(path="/Users/stalenygard/lærke/data-2/MedGas")[1:51]
p<-length(nm)/3

library(xlsx)
y<-NULL
yy<-y
time<-NULL
print(length(nm))
for (i in 1:(length(nm))){
	print(i)
	data<-read.xlsx(nm[i],sheetIndex=1,startRow=1)
	v<-as.vector(as.matrix(data[,2:10],ncol=1))
	print(length(v))
	y<-c(y,v)
	print(length(v))
	yy<-cbind(yy,v)
}




patient<-rep(1:17,each=1001*3*9)
treat<-rep(rep(c(2,1,3),each=1001*9),17)
y<-as.numeric(y)
time<-rep(seq(0,100,0.1),9*17*3)
#time<-as.factor(time)
library(nlme)
#ix<-which(is.na(dat$y)==TRUE)
#dat<-dat[-ix,]
#ix<-1:300000
dat<-data.frame(cbind(y,treat,patient,time))
dat$treat<-as.factor(dat$treat)
dat$patient<-as.factor(dat$patient)
#dat$time<-as.factor(dat$time)
#dat<-dat[ix,]
res.mixed.m<-lme(y~ns(time)+treat,random= ~ 1|patient,data=dat)
coef(summary(res.mixed.m))

i<-1


B<-10000
m.afo.bb.medgas<-rep(NA,B)
for (b in 1:B){
  s<-sample(1:p,p,replace=TRUE)
  m.afo.bb<-rep(NA,p)
  print(b)
  for (i in s){
    #print(i)
    ix.afo<-which(patient==i & treat==2)
    ix.bb<-which(patient==i & treat==1)
    m.afo<-mean(y[ix.afo],na.rm=TRUE)
    m.bb<-mean(y[ix.bb],na.rm=TRUE)
    m.afo.bb[i]<-m.afo-m.bb
  }
  m.afo.bb.medgas[b]<-mean(m.afo.bb,na.rm=TRUE)
}
hist(m.afo.bb.medgas,100)
quantile(m.afo.bb.medgas,c(0.025,0.5,0.975),na.rm=TRUE)/mean(y[which(treat==1)])

#B<-500
m.trafo.bb.medgas<-rep(NA,B)
for (b in 1:B){
  s<-sample(1:p,p,replace=TRUE)
  m.trafo.bb<-rep(NA,p)
  print(b)
  for (i in s){
    #print(i)
    ix.trafo<-which(patient==i & treat==3)
    ix.bb<-which(patient==i & treat==1)
    m.trafo<-mean(y[ix.trafo],na.rm=TRUE)
    m.bb<-mean(y[ix.bb],na.rm=TRUE)
    m.trafo.bb[i]<-m.trafo-m.bb
  }
  m.trafo.bb.medgas[b]<-mean(m.trafo.bb,na.rm=TRUE)
}
hist(m.trafo.bb.medgas,100)
quantile(m.trafo.bb.medgas,c(0.025,0.5,0.975),na.rm=TRUE)
quantile(m.trafo.bb.medgas,c(0.025,0.5,0.975),na.rm=TRUE)/mean(y[which(treat==1)])

length(which(m.afo.bb.medgas>0))/B
length(which(m.trafo.bb.medgas>0))/B

test<-intervals(res.mixed.m)
format(test$fixed[3,]/mean(y)*100,digits=2)
format(test$fixed[4,]/mean(y)*100,digits=3)

#B<-1000
m.trafo.afo.medgas<-rep(NA,B)
for (b in 1:B){
  s<-sample(1:p,p,replace=TRUE)
  m.trafo.afo<-rep(NA,p)
  print(b)
  for (i in s){
    #print(i)
    ix.trafo<-which(patient==i & treat==3)
    ix.afo<-which(patient==i & treat==2)
    m.trafo<-mean(y[ix.trafo],na.rm=TRUE)
    m.afo<-mean(y[ix.afo],na.rm=TRUE)
    m.trafo.afo[i]<-m.trafo-m.afo
  }
  m.trafo.afo.medgas[b]<-mean(m.trafo.afo,na.rm=TRUE)
}
hist(m.trafo.afo.medgas,100)
quantile(m.trafo.afo.medgas,c(0.025,0.5,0.975),na.rm=TRUE)/mean(y[which(treat==2)])




A<-rbind(yy[,1:3],yy[,4:6],yy[,7:9],yy[,10:12],yy[,13:15],yy[,16:18],yy[,19:21],yy[,22:24],yy[,25:27],yy[,28:30],yy[,31:33],yy[,34:36],yy[,37:39],yy[,40:42],yy[,43:45],yy[,46:48],yy[,49:51])
for (i in 1:3) A[,i]<-as.numeric(A[,i])
#plot(as.numeric(A[,1])-as.numeric(A[,3]))
tid<-rep(seq(0,100,0.1),9*p)
patient<-rep(1:p,each=1001*9)
#plot(tid,as.numeric(A[,1])-as.numeric(A[,3]),pch=".",col="red")
#points(tid,as.numeric(A[,2])-as.numeric(A[,3]),pch=".",col="green")

#pdf("diff-curves-3.pdf")
par(mfrow=c(1,1))
n<-100
W1<-matrix(NA,1001,n)
for (k in 1:n){
	print(k)
	a<-sample(1:p,p,replace=TRUE)
	w<-NULL
	for(i in 1:length(a)){
		ix<-which(patient==a[i])
		w<-rbind(w,A[ix,])
	}
	tid<-rep(seq(0,100,0.1),9*p)

	for (i in 1:1001){
		#print(i)
		ix<-which(tid==(i-1)/10)
		W1[i,k]<-mean(w[ix,1]-w[ix,2])
	}
	#points(seq(0,100,0.1),w1,pch=".",col="red")
}
ww1m.u<-apply(W1,1,function(x) quantile(x,0.975,na.rm=TRUE))
ww1m.l<-apply(W1,1,function(x) quantile(x,0.025,na.rm=TRUE))
ww1m.m<-apply(W1,1,function(x) quantile(x,0.5,na.rm=TRUE))

n<-100
W2<-matrix(NA,1001,n)
for (k in 1:n){
	print(k)
	a<-sample(1:p,p,replace=TRUE)
	w<-NULL
	for(i in 1:length(a)){
		ix<-which(patient==a[i])
		w<-rbind(w,A[ix,])
	}
	tid<-rep(seq(0,100,0.1),9*p)

	for (i in 1:1001){
		#print(i)
		ix<-which(tid==(i-1)/10)
		W2[i,k]<-mean(w[ix,3]-w[ix,2])
	}
	#points(seq(0,100,0.1),w1,pch=".",col="red")
}
ww2m.u<-apply(W2,1,function(x) quantile(x,0.975,na.rm=TRUE))
ww2m.l<-apply(W2,1,function(x) quantile(x,0.025,na.rm=TRUE))
ww2m.m<-apply(W2,1,function(x) quantile(x,0.5,na.rm=TRUE))

n<-100
W3<-matrix(NA,1001,n)
for (k in 1:n){
	print(k)
	a<-sample(1:p,p,replace=TRUE)
	w<-NULL
	for(i in 1:length(a)){
		ix<-which(patient==a[i])
		w<-rbind(w,A[ix,])
	}
	tid<-rep(seq(0,100,0.1),9*p)

	for (i in 1:1001){
		#print(i)
		ix<-which(tid==(i-1)/10)
		W3[i,k]<-mean(w[ix,3]-w[ix,1])
	}
	#points(seq(0,100,0.1),w1,pch=".",col="red")
}
ww3m.u<-apply(W3,1,function(x) quantile(x,0.975,na.rm=TRUE))
ww3m.l<-apply(W3,1,function(x) quantile(x,0.025,na.rm=TRUE))
ww3m.m<-apply(W3,1,function(x) quantile(x,0.5,na.rm=TRUE))



nm <- list.files(path="/Users/stalenygard/lærke/data-2/TibAnt")[1:51]
setwd("/Users/stalenygard/lærke/data-2/TibAnt")

y<-NULL
yy<-y
time<-NULL
print(length(nm))
for (i in 1:(length(nm))){
	print(i)
	data<-read.xlsx(nm[i],sheetIndex=1,startRow=1)
	v<-as.vector(as.matrix(data[,2:10],ncol=1))
	print(length(v))
	y<-c(y,v)
	yy<-cbind(yy,v)
}

patient<-rep(1:17,each=1001*3*9)
treat<-rep(rep(c(2,1,3),each=1001*9),17)
y<-as.numeric(y)
time<-rep(seq(0,100,0.1),9*17*3)
#time<-as.factor(time)
library(nlme)
#ix<-which(is.na(dat$y)==TRUE)
#dat<-dat[-ix,]
#ix<-1:300000
dat<-data.frame(cbind(y,treat,patient,time))
dat$treat<-as.factor(dat$treat)
dat$patient<-as.factor(dat$patient)
#dat$time<-as.factor(dat$time)
#dat<-dat[ix,]
res.mixed.t<-lme(y~ns(time)+treat,random= ~ 1|patient,data=dat)
coef(summary(res.mixed.t))


A<-rbind(yy[,1:3],yy[,4:6],yy[,7:9],yy[,10:12],yy[,13:15],yy[,16:18],yy[,19:21],yy[,22:24],yy[,25:27],yy[,28:30],yy[,31:33],yy[,34:36],yy[,37:39],yy[,40:42],yy[,43:45],yy[,46:48],yy[,49:51])
for (i in 1:3) A[,i]<-as.numeric(A[,i])
tid<-rep(seq(0,100,0.1),9*p)
patient<-rep(1:p,each=1001*9)

#B<-10000
m.afo.bb.tibant<-rep(NA,B)
for (b in 1:B){
  s<-sample(1:p,p,replace=TRUE)
  m.afo.bb<-rep(NA,p)
  print(b)
  for (i in s){
    #print(i)
    ix.afo<-which(patient==i & treat==2)
    ix.bb<-which(patient==i & treat==1)
    m.afo<-mean(y[ix.afo],na.rm=TRUE)
    m.bb<-mean(y[ix.bb],na.rm=TRUE)
    m.afo.bb[i]<-m.afo-m.bb
  }
  m.afo.bb.tibant[b]<-mean(m.afo.bb,na.rm=TRUE)
}
hist(m.afo.bb.tibant,100)
quantile(m.afo.bb.tibant,c(0.025,0.5,0.975),na.rm=TRUE)/mean(y[which(treat==1)])

#B<-1000
m.trafo.bb.tibant<-rep(NA,B)
for (b in 1:B){
  s<-sample(1:p,p,replace=TRUE)
  m.trafo.bb<-rep(NA,p)
  print(b)
  for (i in s){
    #print(i)
    ix.trafo<-which(patient==i & treat==3)
    ix.bb<-which(patient==i & treat==1)
    m.trafo<-mean(y[ix.trafo],na.rm=TRUE)
    m.bb<-mean(y[ix.bb],na.rm=TRUE)
    m.trafo.bb[i]<-m.trafo-m.bb
  }
  m.trafo.bb.tibant[b]<-mean(m.trafo.bb,na.rm=TRUE)
}
hist(m.trafo.bb.tibant,100)
quantile(m.trafo.bb.tibant,c(0.025,0.5,0.975),na.rm=TRUE)
quantile(m.trafo.bb.tibant,c(0.025,0.5,0.975),na.rm=TRUE)/mean(y[which(treat==1)])

length(which(m.afo.bb.tibant>0))/B
length(which(m.trafo.bb.tibant>0))/B

#B<-1000
m.trafo.afo.tibant<-rep(NA,B)
for (b in 1:B){
  s<-sample(1:p,p,replace=TRUE)
  m.trafo.afo<-rep(NA,p)
  print(b)
  for (i in s){
    #print(i)
    ix.trafo<-which(patient==i & treat==3)
    ix.afo<-which(patient==i & treat==2)
    m.trafo<-mean(y[ix.trafo],na.rm=TRUE)
    m.afo<-mean(y[ix.afo],na.rm=TRUE)
    m.trafo.afo[i]<-m.trafo-m.afo
  }
  m.trafo.afo.tibant[b]<-mean(m.trafo.afo,na.rm=TRUE)
}
hist(m.trafo.afo.tibant,100)
quantile(m.trafo.afo.tibant,c(0.025,0.5,0.975),na.rm=TRUE)/mean(y[which(treat==2)])




test<-intervals(res.mixed.t)
format(test$fixed[3,]/mean(y)*100,digits=3)
format(test$fixed[4,]/mean(y)*100,digits=3)

#pdf("diff-curves-3.pdf")
par(mfrow=c(1,1))
n<-100
W1<-matrix(NA,1001,n)
for (k in 1:n){
	print(k)
	a<-sample(1:p,p,replace=TRUE)
	w<-NULL
	for(i in 1:length(a)){
		ix<-which(patient==a[i])
		w<-rbind(w,A[ix,])
	}
	tid<-rep(seq(0,100,0.1),9*p)

	for (i in 1:1001){
		#print(i)
		ix<-which(tid==(i-1)/10)
		W1[i,k]<-mean(w[ix,1]-w[ix,2])
	}
	#points(seq(0,100,0.1),w1,pch=".",col="red")
}
ww1t.u<-apply(W1,1,function(x) quantile(x,0.975,na.rm=TRUE))
ww1t.l<-apply(W1,1,function(x) quantile(x,0.025,na.rm=TRUE))
ww1t.m<-apply(W1,1,function(x) quantile(x,0.5,na.rm=TRUE))

n<-100
W2<-matrix(NA,1001,n)
for (k in 1:n){
	print(k)
	a<-sample(1:p,p,replace=TRUE)
	w<-NULL
	for(i in 1:length(a)){
		ix<-which(patient==a[i])
		w<-rbind(w,A[ix,])
	}
	tid<-rep(seq(0,100,0.1),9*p)

	for (i in 1:1001){
		#print(i)
		ix<-which(tid==(i-1)/10)
		W2[i,k]<-mean(w[ix,3]-w[ix,2])
	}
	#points(seq(0,100,0.1),w1,pch=".",col="red")
}
ww2t.u<-apply(W2,1,function(x) quantile(x,0.975,na.rm=TRUE))
ww2t.l<-apply(W2,1,function(x) quantile(x,0.025,na.rm=TRUE))
ww2t.m<-apply(W2,1,function(x) quantile(x,0.5,na.rm=TRUE))

n<-100
W3<-matrix(NA,1001,n)
for (k in 1:n){
	print(k)
	a<-sample(1:p,p,replace=TRUE)
	w<-NULL
	for(i in 1:length(a)){
		ix<-which(patient==a[i])
		w<-rbind(w,A[ix,])
	}
	tid<-rep(seq(0,100,0.1),9*p)

	for (i in 1:1001){
		#print(i)
		ix<-which(tid==(i-1)/10)
		W3[i,k]<-mean(w[ix,3]-w[ix,1])
	}
	#points(seq(0,100,0.1),w1,pch=".",col="red")
}
ww3t.u<-apply(W3,1,function(x) quantile(x,0.975,na.rm=TRUE))
ww3t.l<-apply(W3,1,function(x) quantile(x,0.025,na.rm=TRUE))
ww3t.m<-apply(W3,1,function(x) quantile(x,0.5,na.rm=TRUE))


setwd("/Users/stalenygard/lærke/")

dens<-30
pdf("diff-curves-afo-barefoot.pdf")
plot(seq(0,100,0.1),ww1t.m,pch=".",ylim=c(-0.0004,0.0004),col="red",main="AFO vs barefoot",ylab="Difference to barefoot (V)",xlab="% gait cycle")
#lines(ww.u,col="red")
#lines(ww.l,col="red")
ixx<-which(is.na(ww1t.u) & is.na(ww1t.l))
polygon(x=c((seq(0,100,0.1))[-ixx],rev(seq(0,100,0.1))[-ixx]),y=c(ww1t.u[-ixx],rev(ww1t.l[-ixx])),col="red",density=dens,angle=45)
abline(h=0)
points(seq(0,100,0.1),ww1m.m,pch=".",ylim=c(-0.0004,0.0004),col="blue")
#lines(ww.u,col="red")
#lines(ww.l,col="red")
ixx<-which(is.na(ww1m.u) & is.na(ww1m.l))
polygon(x=c((seq(0,100,0.1))[-ixx],rev(seq(0,100,0.1))[-ixx]),y=c(ww1m.u[-ixx],rev(ww1m.l[-ixx])),col="blue",density=dens,angle=135)
legend("topright",c("TibAnt","MedGas"),lty=1,col=c("red","blue"))
graphics.off()

pdf("diff-curves-trafo-barefoot.pdf")
plot(seq(0,100,0.1),ww2t.m,pch=".",ylim=c(-0.0004,0.0004),col="red",main="TRAFO vs barefoot",ylab="Difference to barefoot (V)",xlab="% gait cycle")
#lines(ww.u,col="red")
#lines(ww.l,col="red")
ixx<-which(is.na(ww2t.u) & is.na(ww2t.l))
polygon(x=c((seq(0,100,0.1))[-ixx],rev(seq(0,100,0.1))[-ixx]),y=c(ww2t.u[-ixx],rev(ww2t.l[-ixx])),col="red",density=dens,angle=45)
abline(h=0)
points(seq(0,100,0.1),ww2m.m,pch=".",ylim=c(-0.0004,0.0004),col="blue")
#lines(ww.u,col="red")
#lines(ww.l,col="red")
ixx<-which(is.na(ww2m.u) & is.na(ww2m.l))
polygon(x=c((seq(0,100,0.1))[-ixx],rev(seq(0,100,0.1))[-ixx]),y=c(ww2m.u[-ixx],rev(ww2m.l[-ixx])),col="blue",density=dens,angle=135)
legend("topright",c("TibAnt","MedGas"),lty=1,col=c("red","blue"))
graphics.off()

postscript("diff-curves-trafo-afo.eps")
plot(seq(0,100,0.1),ww3t.m,pch=".",ylim=c(-0.0004,0.0004),col="red",main="TRAFO vs AFO",ylab="Difference to AFO (V)",xlab="% gait cycle")
#lines(ww.u,col="red")
#lines(ww.l,col="red")
ixx<-which(is.na(ww3t.u) & is.na(ww3t.l))
polygon(x=c((seq(0,100,0.1))[-ixx],rev(seq(0,100,0.1))[-ixx]),y=c(ww3t.u[-ixx],rev(ww3t.l[-ixx])),col="red",density=dens,angle=45)
abline(h=0)
points(seq(0,100,0.1),ww3m.m,pch=".",ylim=c(-0.0004,0.0004),col="blue")
#lines(ww.u,col="red")
#lines(ww.l,col="red")
ixx<-which(is.na(ww3m.u) & is.na(ww3m.l))
polygon(x=c((seq(0,100,0.1))[-ixx],rev(seq(0,100,0.1))[-ixx]),y=c(ww3m.u[-ixx],rev(ww3m.l[-ixx])),col="blue",density=dens,angle=135)
legend("topright",c("TibAnt","MedGas"),lty=1,col=c("red","blue"))
graphics.off()

write.table(cbind(seq(0,100,0.1),ww1t.l,ww1t.m,ww1t.u),file="AFO-bb-TibAnt.txt",sep="\t",row.names=FALSE)
write.table(cbind(seq(0,100,0.1),ww2t.l,ww2t.m,ww2t.u),file="trAFO-bb-TibAnt.txt",sep="\t",row.names=FALSE)
write.table(cbind(seq(0,100,0.1),ww3t.l,ww3t.m,ww3t.u),file="trAFO-AFO-TibAnt.txt",sep="\t",row.names=FALSE)
write.table(cbind(seq(0,100,0.1),ww1m.l,ww1m.m,ww1m.u),file="AFO-bb-MedGas.txt",sep="\t",row.names=FALSE)
write.table(cbind(seq(0,100,0.1),ww2m.l,ww2m.m,ww2m.u),file="trAFO-bb-MedGas.txt",sep="\t",row.names=FALSE)
write.table(cbind(seq(0,100,0.1),ww3m.l,ww3m.m,ww3m.u),file="trAFO-AFO-MedGas.txt",sep="\t",row.names=FALSE)

dens<-30

pdf("diff-curves-2.pdf",width=20,height=7)
par(mfrow=c(1,3))
plot(seq(0,100,0.1),ww1t.m,pch=".",ylim=c(-0.0004,0.0002),col="red",main="AFOf vs barefoot",ylab="Difference to barefoot (V)",xlab="% gait cycle")
#lines(ww.u,col="red")
#lines(ww.l,col="red")
ixx<-which(is.na(ww1t.u) & is.na(ww1t.l))
polygon(x=c((seq(0,100,0.1))[-ixx],rev(seq(0,100,0.1))[-ixx]),y=c(ww1t.u[-ixx],rev(ww1t.l[-ixx])),col="red",density=dens,angle=45)
abline(h=0)
points(seq(0,100,0.1),ww1m.m,pch=".",ylim=c(-0.0004,0.0004),col="blue")
#lines(ww.u,col="red")
#lines(ww.l,col="red")
ixx<-which(is.na(ww1m.u) & is.na(ww1m.l))
polygon(x=c((seq(0,100,0.1))[-ixx],rev(seq(0,100,0.1))[-ixx]),y=c(ww1m.u[-ixx],rev(ww1m.l[-ixx])),col="blue",density=dens,angle=135)
legend("topright",c("TibAnt","MedGas"),lty=1,col=c("red","blue"))

plot(seq(0,100,0.1),ww2t.m,pch=".",ylim=c(-0.0004,0.0002),col="red",main="AFOc vs barefoot",ylab="Difference to barefoot (V)",xlab="% gait cycle")
#lines(ww.u,col="red")
#lines(ww.l,col="red")
ixx<-which(is.na(ww2t.u) & is.na(ww2t.l))
polygon(x=c((seq(0,100,0.1))[-ixx],rev(seq(0,100,0.1))[-ixx]),y=c(ww2t.u[-ixx],rev(ww2t.l[-ixx])),col="red",density=dens,angle=45)
abline(h=0)
points(seq(0,100,0.1),ww2m.m,pch=".",ylim=c(-0.0004,0.0004),col="blue")
#lines(ww.u,col="red")
#lines(ww.l,col="red")
ixx<-which(is.na(ww2m.u) & is.na(ww2m.l))
polygon(x=c((seq(0,100,0.1))[-ixx],rev(seq(0,100,0.1))[-ixx]),y=c(ww2m.u[-ixx],rev(ww2m.l[-ixx])),col="blue",density=dens,angle=135)
legend("topright",c("TibAnt","MedGas"),lty=1,col=c("red","blue"))

plot(seq(0,100,0.1),ww3t.m,pch=".",ylim=c(-0.0004,0.0002),col="red",main="AFOc vs AFOf",ylab="Difference to AFO (V)",xlab="% gait cycle")
#lines(ww.u,col="red")
#lines(ww.l,col="red")
ixx<-which(is.na(ww3t.u) & is.na(ww3t.l))
polygon(x=c((seq(0,100,0.1))[-ixx],rev(seq(0,100,0.1))[-ixx]),y=c(ww3t.u[-ixx],rev(ww3t.l[-ixx])),col="red",density=dens,angle=45)
abline(h=0)
points(seq(0,100,0.1),ww3m.m,pch=".",ylim=c(-0.0004,0.0004),col="blue")
#lines(ww.u,col="red")
#lines(ww.l,col="red")
ixx<-which(is.na(ww3m.u) & is.na(ww3m.l))
polygon(x=c((seq(0,100,0.1))[-ixx],rev(seq(0,100,0.1))[-ixx]),y=c(ww3m.u[-ixx],rev(ww3m.l[-ixx])),col="blue",density=dens,angle=135)
legend("topright",c("TibAnt","MedGas"),lty=1,col=c("red","blue"))
graphics.off()