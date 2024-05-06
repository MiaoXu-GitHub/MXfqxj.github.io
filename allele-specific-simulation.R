boot_out <- replicate(10000, {
  t.test(rnorm(10000, 0.1, 1.5),
         rnorm(10000, 0, 1.5))$p.value
})

set.seed(1985)

n <- 1000
a <- 0.7
AA <- rgamma(1, 5, 0.6)
aa <- rgamma(1, 5, 0.5)

count_AA <- rpois(n, AA)
count_aa <- rpois(n, aa)

samp <- sample(n, a * n)
count_total <- c(count_AA[samp], count_aa[-samp])

plot(density(count_total))


## making full dataframe of sampled counts
df <- data.frame(allele = sort(rep(c("AA", "aa"), 1000)), count = c(count_aa, count_AA))

## density comparison of underlying count
ggplot(df, aes(x = count, group = allele, fill = allele)) + geom_density()

## density plot of 70% mixture of allele counts
qplot(count_total, geom = "density", fill = I("blue"), alpha = 0.3)

## all densities in one plot
qplot(count_total, geom = "density", fill = I("black"), alpha = 0.3) +
  geom_density(mapping = aes(x = count, group = allele, fill = allele), data = df)
