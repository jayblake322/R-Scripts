x=c(1,2,3,4,5,6,7,8,9,0)

y=c(13,28,43,35,96,84,101,110,108,13)

lm.out <- lm(y ~ x)
newx = seq(min(x),max(x),by = 0.05)
conf_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)

plot(x, y, xlab="x", ylab="y", main="Regression")
abline(lm.out, col="lightblue")
matlines(newx, conf_interval[,2:3], col = "blue", lty=2)