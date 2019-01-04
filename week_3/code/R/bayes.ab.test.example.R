# let's just play around for a bit a check out different settings of the beta distribution
curve(dbeta(x, 2, 2), 0, 1)  # 50-50 shot with 2 tests

curve(dbeta(x, 1, 1), 0, 1)  # 0 tests -- blank prior

# as we increase number of trials, the distribution sharpens up
curve(dbeta(x, 11, 11), 0, 1)
curve(dbeta(x, 21, 21), 0, 1)
curve(dbeta(x, 201, 201), 0, 1)

# what about a lot of successes and few failures?
curve(dbeta(x, 21, 2), 0, 1)

# lots of failures, few successes
curve(dbeta(x, 2, 21), 0, 1)

curve(dbeta(x, 3, 7), 0, 1)
# prove the mean is 30%
x <- seq(0, 1000) / 1000
weighted.mean(x, dbeta(x, 3, 7))
# incorporating a prior
curve(dbeta(x, 3, 7) * dbeta(x, 37, 115), 0, 1)
curve(dbeta(x, 3 + 36, 7 + 114), add = T, col = 'red')


# normalizing the curves to check if it is indeed the same
mult.beta <- dbeta(x, 3, 7) * dbeta(x, 37, 115) / sum(dbeta(x, 3, 7) * dbeta(x, 37, 115))
add.beta <- dbeta(x, 3 + 36, 7 + 114) / sum(dbeta(x, 3 + 36, 7 + 114))

# can't do line plots in base R...have to use lattice or ggplot2
library(ggplot2)
# have to make a data table or frame to use ggplot...
library(data.table)
df <- data.frame(x = x, mult.beta = mult.beta, add.beta = add.beta)
dt <- as.data.table(df)
ggplot(data = dt, aes(x = x, y = mult.beta)) + geom_line(color = 'blue') + geom_line(data = dt, aes(x = x, y = add.beta, color = 'red'))
# yep, multiplying two beta distributions or adding the alphas and betas gets to the same result
dt[, diff:=mult.beta - add.beta]
sum(dt$diff)  # basically 0

# Say we have a webpage where we try to get people to buy a subscription.
# If they subscribe, it is a successful conversion.
# "Conversion rate" is the % of people that subscribe
# out of the total that view the page.

# set our prior -- estimating a 30% conversion rate
# alpha is successes + 1, beta is failures + 1
# so 30% conversion (with a weak prior, not a sharp distribution) is
# alpha = 3 + 1; beta = 7 + 1
alpha.prior <- 4
beta.prior <- 8

curve(dbeta(x, alpha.prior, beta.prior), 0, 1)
# version A of the webpage gets 50 out of 450 people to sign up
# B has 37 out 425 that signed up
# so the conversion rates are
a.conv.rate <- 50/450
print(a.conv.rate)
b.conv.rate <- 37/425
print(b.conv.rate)
# randomly sample 100K times to compare the two distributions

n.trials <- 100000
a.samples <- rbeta(n.trials, 50 + alpha.prior, 450 + beta.prior)
b.samples <- rbeta(n.trials, 37 + alpha.prior, 425 + beta.prior)
a.b.ratio <- a.samples / b.samples
hist(a.b.ratio)
print(mean(a.b.ratio))
# cumulative disribution function --
# there is a small chance that a is actually worse than b
plot(ecdf(a.b.ratio))
