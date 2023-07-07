get_survival_num <- function(x, a) {
  y <- 1/2 * (1 + (1 - exp(-1 * a * (2 * x - 1))) / (1 + exp(-1 * a * (2 * x - 1))) * (1 + exp(-1 * a)) / (1 - exp(-1 * a))) * (1 / x)
  return (y)
}

x <- seq(0.01, 1, by=0.01)
df <- data.frame(matrix(ncol=3))[0, ]
colnames(df) <- c("max_a", "max_value", "egg_size")
for (max_a in 10:50) {
  m <- max_a / 2
  s <- max_a / 6
  expected_value_each_size <- rep(0, times=length(x))
  for (a in 1:max_a) {
    if (a == 1) {
      probability <- pnorm(a, mean=m, sd=s)
    } else if (a == max_a) {
      probability <- 1 - pnorm(a, mean=m, sd=s)
    } else {
      probability <- dnorm(a, mean=m, sd=s)
    }
    (expected_value_each_size <- expected_value_each_size
      + get_survival_num(x, a) * probability)
  }
  cur_data <- data.frame(
    max_a=max_a,
    max_value=max(expected_value_each_size),
    egg_size=which.max(expected_value_each_size) * 0.01
  )
  df <- rbind(df, cur_data)
}

write.csv(df, "./output/expected_value_list.csv")

plot(c(10:50), df$max_value, xlab="max_a", ylab="max expected value")
title("max_a and max expected value")
plot(c(10:50), df$egg_size, xlab="max_a", ylab="best egg size")
title("max_a and best egg size")