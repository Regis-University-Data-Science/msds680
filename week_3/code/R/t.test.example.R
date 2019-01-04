# df, degrees of freedom, is n - 1; n is number of samples
curve(dt(x, df = 3), -4, 4, col = 'red')
curve(dt(x, df = 10), add = T)
curve(dt(x, df = 300), add = TRUE)
curve(dt(x, df = 3000), add = TRUE)
curve(dt(x, df = 300000), add = TRUE)
curve(dt(x, df = 3000000), add = TRUE)

# conversions (signups) from A version of website
# 0 is no sign up, 1 is sign up
# 40% success rate with a
a.results <- rbinom(100, 1, p=0.4)

# 41% success rate with version B
b.results <- rbinom(100, 1, p=0.41)
# p-value is large (t-value is small), so effect doesn't look significant
t.test(a.results, b.results, alternative = 'two.sided')


# what if we have 500k visitors?
# 40% success rate with a
a.results <- rbinom(500000, 1, p=0.4)
# 41% success rate with version B
b.results <- rbinom(500000, 1, p=0.41)

# suddenly the 1% difference looks significant, even though it may not really be
t.test(a.results, b.results, alternative = 'two.sided')
