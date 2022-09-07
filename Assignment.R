#=============
# Import data
#=============
file_name = "Real_Estate.csv"
Path = "/Users/xingxing/Desktop/STAT5002/5002Assignment/"
data = read.csv(paste(Path,file_name, sep=''))

summary(data)

#=============
# Summary table
#=============

# Separate data columns
summary(data)

sd(data$Price)
sd(data$Bedrooms)
sd(data$Size)
sd(data$Distance)

t.test(data$Price,conf.level=0.95)
t.test(data$Bedrooms,conf.level=0.95)
t.test(data$Size,conf.level=0.95)
t.test(data$Distance,conf.level=0.95)

#================
# t test suburb2
#================
# Normality test
test_price = data$Price[data$Suburbs == 2]
t.test(test_price, mu = 420.000, alternative = "greater")

1 - pt(5.3670, 42)
qqnorm(test_price, col = "blue", pch = 19)
qqline(test_price)
shapiro.test(test_price)

#=========
# Q3 
#=========
data$new_Size = data$Size[data$Size < 200 ] = 'Small'

data2=within(data,{Size_Group=ifelse(data$Size < 200,"Small","Large")})
table(data2$Size_Group)

small_price = data$Price[data2$new_Size=='Small']
large_price = data$Price[data2$new_Size=='Large']


par(mfrow = c(1,2))
boxplot(small_price, xlim=, xlab = 'Small house Price')
boxplot(large_price, xlab = 'Large house Price')


t.test(small_price, conf.level=0.95)
t.test(large_price, conf.level=0.95)



#==================
# Regression model
#==================
y = data$Price
x = data$Size

hist(x)
hist(y)

# check outliers
par(mfrow = c(1,2))
boxplot(y)
hist(y)

# scatter plot
plot(y~x, main = 'Size VS Price', xlab = 'Size', ylab = 'Price')

# regression model
fit = lm(y~x)
abline(fit, col = 'red', lwd = 2)

summary(fit)

# coeficient 
b0 = fit$coefficients[[1]];b0
b1 = fit$coefficients[[2]];b1

sum(lm.influence(fit)$h > 2 * 2/53)
q = qqnorm(fit$res)
abline(lm(y2 ~ x2, data = q))


n = length(x);n
sig.hat = sqrt(sum(fit$res^2) /(n-2)) 
Sxx = var(x)*(n-1)
t = b1/(sig.hat / sqrt(Sxx))
p = 2*(1-pt(t, n-2));p

























