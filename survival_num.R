get_survival_num <- function(x, a) {
  y <- 1/2 * (1 + (1 - exp(-1 * a * (2 * x - 1))) / (1 + exp(-1 * a * (2 * x - 1))) * (1 + exp(-1 * a)) / (1 - exp(-1 * a))) * (1 / x)
  return (y)
}

x <- seq(0.01, 1, by=0.01)
max_a <- 40
for (a in 1:max_a) {
  plot(
    x,
    get_survival_num(x, a), 
    type='l',
    xaxt='n',
    yaxt='n', 
    xlab='',
    ylab=''
  )
  par(new=T)
}

axis(side=1, at=seq(0, 1.1, by=0.1))
axis(side=2, at=seq(0, 2, by=0.1))
title(
  paste('number of surviving children (max_a=', max_a, ')'),
  xlab='egg size',
  ylab='number of surviving children'
)
