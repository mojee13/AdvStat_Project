n_people_tot <- 50
pbday <- rep(0, n_people_tot)
for (k in 2:n_people_tot) {
  n_tests = 1E5; cb <- 0
  for (i in 1:n_tests) {
    bdays <- sample(1:365 , k ,
                    replace=TRUE)
    if (length(bdays) > length(unique(bdays ))) {
      cb = cb + 1
    }
  }
  pbday[k] <- cb/n_tests
  message(paste("k:", k, "pb(",k,"):",pbday[k]))
}
pfunc <- function(f, b) function(a) f(a,b)
p50_index <- Position(pfunc(`>`, 0.5) , pbday)
message(paste("First element with prob >0.5:", p50_index))




xc <- seq(-2,2,1)
pc <- pnorm(xc,0,1)
qc <- qnorm(pc)
xx=seq(-3,3,0.01)
xx
plot(xx,dnorm(xx,0,0.1))
hist(rnorm(1000,4,1))
p1 <- ggplot ( mpg , aes ( xx , dnorm(xx,0,0.1) ))
p1
yy=dnorm(xx,0,0.1)
df=data.frame(xx,yy)
ggplot(xx,yy)
ggplot(data=df, aes(x = xx, y = yy))+geom_line()


p1 <- hist(rnorm(500,4))                     # centered at 4
p2 <- hist(rnorm(500,6))                     # centered at 6
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,10))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,10), add=T)  # second





cust <- 4
p <- 4/15
x <- 0:4
ap <- dbinom(x,cust ,p)
barplot(ap,names=x, col="navy", xlab="x", ylab="f(x)", density=40,main = sprintf("Binomial distr. Customers=%d, p=%.2f",cust ,p),cex.lab=1.5, cex.axis=1.25, cex.main=1.25, cex.sub=1.5)
cat(paste(c("P(2|np) = ", ap[3],"\n")))



n <- 20
r <- 10
p <- seq(0, 1, length.out = 201)
p.post <- dbinom(x=r, size=n, prob=p)
plot(p, p.post ,
     xaxs="i", yaxs="i", col="navy",
     type="l", lty=1, lwd = 3,
     ylim=c(0,0.2),
     xlab="p",
     ylab=expression(paste(Pˆsymbol("*"),
                           "(p | r,n,M)")))





alpha.prior <- 10; beta.prior <- 10
Nsamp <- 200
delta.p <- 1/Nsamp
p <- seq(from=1/(2*Nsamp),
         by=1/Nsamp ,
         length.out=Nsamp)


p.like <- dbinom(x=r, size=n, prob=p)
p.like1 <- p.like /( delta.p*sum(p.like ))
plot(p,p.like1)
par(mfrow=c(1,2))

f <- function(x,a=30,b=40){dgamma(x,a,b)}
x=seq(0,1,0.001)
plot(x,f(x))
k <-0; integral <-1; interval <- 0.95
while (integral > 0.95){k <- k + 0.01; integral <- integrate(f, k,1-k)$value}



p     <- c(1:30)
 n <- 75
 prob <- 0.15
 binom <- dbinom(p,n,prob)
 plot(p, binom, xlab="number of times", col="navy",lwd = 2.6,type="l",ylab="Density" )
 title(main=paste("former prob (15%. not the new method!) pdf of the number of times the new method fails"), line=0.7, cex.main=1.2)
 
 
 beta.func  <- function(exp.x, std.x){return((((1-exp.x)*exp.x)/((std.x)**2) - 1) / (1+ exp.x/(1-exp.x)))}
 
 
 alpha.func <- function(exp.x, std.x){return((exp.x/(1-exp.x))*(((1-exp.x)*exp.x)/((std.x)**2) - 1) / (1+ exp.x/(1-exp.x)))}
 
 
 data <- c(514,536,345,440,427,443,386,418,364,
           483,506,385,410,561,275,306,294,402,
           350,343,480,334,324,414,296)

p=seq(200,600,0.1)
a=1231.5
lam=3.82
plot(p,dgamma(p,a,lam),type = "l")
title(main="prior")
plot()



# Utils
n.sample <- 4000
mu <- seq(300,500,length.out=n.sample)
delta.mu <- mu[2]-mu[1]

N <- length(data)
y.bar <- mean(data)

sigma.sq <- (80**2)
s.sq <- (80**2)
m <- 325

mean=mean(data)
v=var(data)
lambda.post=beta.func




mu.post <- (1/s.sq) / (N/sigma.sq + 1/s.sq) * m + (N/sigma.sq) / (N/sigma.sq + 1/s.sq) * y.bar
sd.sq.post <- (sigma.sq * s.sq) / (sigma.sq + N * s.sq)


plot(mu,dnorm(mu,mu.post,sqrt(sd.sq.post)),type = "l")
abline(v=q1)
abline(v=q2)
q1=qnorm(0.05,mu.post,sqrt(sd.sq.post))
q2=qnorm(0.95,mu.post,sqrt(sd.sq.post))




