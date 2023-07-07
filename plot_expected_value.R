get_survival_num <- function(x, a) {
  y <- 1/2 * (1 + (1 - exp(-1 * a * (2 * x - 1))) / (1 + exp(-1 * a * (2 * x - 1))) * (1 + exp(-1 * a)) / (1 - exp(-1 * a))) * (1 / x)
  return (y)
}

x <- seq(0.01, 1, by=0.01)

max_a <- 40
m <- max_a / 2
s <- max_a / 6
expected_value_each_size <- rep(0, times=length(x))
for (a in 1:max_a) {
  if (a == 1) {
    probability = pnorm(a, mean=m, sd=s)
  } else if (a == max_a) {
    probability = 1 - pnorm(a, mean=m, sd=s)
  } else {
    probability = dnorm(a, mean=m, sd=s)
  }
  (expected_value_each_size <- expected_value_each_size
    + get_survival_num(x, a) * probability)
}

plot(
  x,
  expected_value_each_size,
  type='l',
  xaxt='n',
  yaxt='n',
  xlab='',
  ylab=''
)

axis(side=1, at=seq(0, 1.1, by=0.1))
axis(side=2, at=seq(0, 2, by=0.1))
title(
  paste('expected value (max_a=', max_a, ')'),
  xlab='egg size',
  ylab='expected number of children'
)

print(max(expected_value_each_size))
print(which.max(expected_value_each_size) * 0.01)
