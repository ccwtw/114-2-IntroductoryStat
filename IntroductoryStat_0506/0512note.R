

qbinom(0.75, 10, 0.2)

set.seed(0512)
rbinom(10, 10, 0.2)

set.seed(20250512)
rbinom(n = 8, size = 10, prob = 0.2)

dnorm(x = 0, mean = 0, sd = 1)
pnorm(q = 1.62, mean = 0, sd = 1)
sqrt(2)^2

set.seed(20250512)
u <- rbinom(10000, 20, 0.5)
x <- seq(0, 20, length = 10000)
y <- dnorm(x, 10, sqrt(5))
hist(u, breaks = seq(-0.5, 20.5, by = 1), prob = T, ylim = c(0, 0.2))
lines(x, y, col = 'red')


set.seed(1)
x <- rnorm(30, mean = 2, sd = 1.36)
z <- (mean(x) - 2) / (1.36 / sqrt(30))
2 * (1 - pnorm(abs(z)))
qnorm(0.975)

set.seed(20250512)
errors <- replicate(10000, {
  x <- rnorm(30, mean = 2, sd = 1.36)
  z <- (mean(x) - 2) / (1.36/ sqrt(30))
  as.integer(abs(z) > qnorm(0.975))
})
mean(errors)


mu0 <- 100; mu1 <- 105; sigma <- 10; n <- 25; alpha <- 0.05    

# 臨界值：z_alpha（右尾）
z_alpha <- qnorm(1 - alpha)
crit_value <- mu0 + z_alpha * (sigma / sqrt(n))  # 對應 x 的拒絕門檻

# 建立 x 軸
x <- seq(85, 120, length = 1000)

# H0 和 H1 的常態分布
f0 <- dnorm(x, mean = mu0, sd = sigma / sqrt(n))
f1 <- dnorm(x, mean = mu1, sd = sigma / sqrt(n))

# 畫圖
plot(x, f0, type = "l", lwd = 2, col = "red", ylim = c(0, max(f0, f1)),
     ylab = "Density", xlab = "Sample Mean", main = "Type II Error (β) Illustration")
lines(x, f1, col = "blue", lwd = 2)

x_beta <- x[x < crit_value]
polygon(c(x_beta, rev(x_beta)),
        c(dnorm(x_beta, mean = mu1, sd = sigma / sqrt(n)), rep(0, length(x_beta))),
        col = rgb(0, 0, 1, 0.3), border = NA)

abline(v = crit_value, lty = 2)
legend("topright", legend = c("H0: N(100)", "H1: N(105)", "Type II error (β)"),
       col = c("red", "blue", rgb(0, 0, 1, 0.3)), lty = c(1,1,NA), pch = c(NA, NA, 15))

successes <- rbinom(1, size = 100, prob = 0.62)
prop.test(successes, 100, p = 0.62)


aov_df <- data.frame(value = c(rnorm(20, mean = 30, sd = 2), 
                             rnorm(20, mean = 35, sd = 2),
                             rnorm(20, mean = 38, sd = 2)),
                   group = factor(rep(c("A", "B", "C"), each = 20)))
aov(value ~ group, data = aov_df)
summary(aov(value ~ group, data = aov_df))

rbinom(1, 50, 0.6)

set.seed(20250512)
rolls <- sample(1:6, size = 90, replace = T, prob = c(1,1,1,1,1,2))

chisq_result <- chisq.test(table(rolls), p = rep(1/6, 6))
ggchisqtest(chisq_result)
