# NLS practice

#set a seed value
set.seed(23)

#Generate x as 100 integers using seq function
x<-seq(0,100,1)

#Generate y as a*e^(bx)+c
y<-runif(1,0,20)*exp(runif(1,0.005,0.075)*x)+runif(101,0,5)

# What does our data look like? 
plot(x,y)

### Linear model

lin_mod=lm(y~x)

#Plotting the model
plot(x,y)
abline(lin_mod)

### Nonlinear model
nonlin_mod=nls(y~a*exp(b*x),start=list(a=13,b=0.1)) #a is the starting value and b is the exponential start

#This new plot can be made by using the lines() function
plot(x,y)
lines(x,predict(nonlin_mod),col= "red")

#Error calculation
error <- lin_mod$residuals  
lm_error <- sqrt(mean(error^2))   #5.960544

error2=y-predict(nonlin_mod)
nlm_error <- sqrt(mean(error2^2)) #1.527064

# Making the nonlinear model
nonlin_mod

######################################

y3 = y2 + 25

#visually estimate some starting parameter values
plot(x2,y3)
#from this graph set approximate starting values
a_start <- 40 #param a is the y value when x=0
b_start <- .01 #b is the decay rate
#model
m <- gnls(y3 ~ a*exp(-b*x2), start = list(a = a_start, b = b_start))
#get some estimation of goodness of fit
cor(y3, predict(m))
#plot the fit
lines(x2,predict(m),col="red",lty=2,lwd=3)


# with autocorrelation
m_cor <- gnls(y3 ~ a*exp(-b*x2), 
              start = list(a = a_start, b = b_start), 
              correlation = corAR1(form = ~1))
cor(y3, predict(m_cor))

plot(x2,y3)
lines(x2, predict(m_cor), col = "red", lty = 2, lwd = 3)
