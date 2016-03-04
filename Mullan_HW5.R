#1. Create These Vectors

a <- c(1:20)
b <- c(20:1)
c <- c(a[1:20],b[2:20])

#d
tmp <- c(4,6,3)

e <-rep(tmp,10)
f <- rep(tmp, length.out=31)
g <- rep(tmp,each=10)

#2 create a vector based on x
x2 <- seq(3,6,by=0.1)
xvector= exp(x2)*cos(x2)

#3 create 2 vectors
x3 <- seq(3,36,by=3)
y3 <- seq(1,34,by=3)
vector3a <-(0.1^x3)*(0.1^y3)
z3 <- c(1:25)
vector3b <- (2^z3)/z3

#4 create 2 different sums
i <- c(1:100)
sum = 0
vector4a <- i^3 + 4*i^2
sum(vector4a)
i2 <- c(1:25)
vector4b <- ((2^i2)/i2)+((3^i2)/(i2^2))
sum(vector4b)

#5 create 2 different vectors with strings
i5 <- c(1:30)
char <- as.character(i5)
label = "label"
labelvec <- rep(label,30)
vector5a <-paste(labelvec,char)

fn = "fn"
fnvec <- rep(fn,30)
vector5b <- paste(fn,char,sep="")

#6 create 2 vectors from the given code
set.seed(50)
xVec <- sample(0:999,250,replace=T)
yVec <- sample(0:999,250,replace=T)

vector6a <- NULL
for (n in 1:249) vector6a <- c(vector6a,(yVec[n+1]-xVec[n])) 

vector6b <- NULL
for (n in 1:249) vector6b <- c(vector6b,(sin(yVec[n]))/(cos(xVec[n+1])))

vector6c <- NULL
for (n in 3:250) vector6c <- c(vector6c,(xVec[n-2] + 2*xVec[n-1] - xVec[n]))

vector6d <- NULL
for (n in 1:249) vector6d <- c(vector6d,(exp(-xVec[n+1]))/(xVec[n]+10))

#7 Use xVec and yVec to learn some built in functions

vector7a <- NULL
for (n in 1:250) {if (yVec[n]>600) vector7a <- c(vector7a,yVec[n])}

vector7b <- NULL
for (n in 1:250) {if (yVec[n]>600) vector7b <- c(vector7b,n)}

vector7c <- NULL
for (n in vector7b) vector7c <- c(vector7c,xVec[n])

xbar=mean(xVec)
vector7d <- NULL
for (n in 1:250) vector7d <- c(vector7d,sqrt(abs(xVec[n]-xbar)))

counte=0
for (n in 1:250) {if ((max(yVec)-yVec[n])<200) counte=counte+1}

countf=0
for (n in 1:250) {if (xVec[n] %%2 == 0) countf=countf+1}


yorder <- rank(yVec)
xorder <- NULL
for (n in yorder) xorder <- c(xorder,xVec[n])
xorder


i7 <- seq(1,250, by=3)
vector7h <- NULL
for (n in i7) vector7h <- c(vector7h,yVec[n])


#8 use cumprod
x8 <-seq(2,38,by=2)
y8<-seq(3,39,by=2)
ratio<-x8/y8
product <- cumprod(ratio)
answer = 1 + sum(product)
print(answer)
