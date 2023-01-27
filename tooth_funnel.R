# https://www.gov.uk/government/publications/hospital-tooth-extractions-of-0-to-19-year-olds

library(data.table)
library(RColorBrewer)
library(devEMF)

vpt <- function(X, N, ..., FUN = poisson.test) {
  f <- function (x, n) c(x/n, FUN(x, n, ...)$conf.int[1:2])
  r <- mapply(f, x = X, n = N, SIMPLIFY = T)
  r <- t(r)
  `colnames<-`(r, c('rate', 'lower', 'upper'))
}


setwd("H:/bradford_imd")

d <- fread("inpatient_tooth_extractions.csv")
setnames(d, "LA Code", 'lad2019')

d[, pop_18 := as.numeric(pop_18)]
d[, count_18_19 := as.numeric(count_18_19)]
d[, count_20_21 := as.numeric(count_20_21)]

d[, ct := count_18_19 + count_19_20]
d[, p := pop_18 + pop_19]
d[, r := ct / p * 100000]

# add IMD

i <- fread("imd_lad_2019.csv")
d <- i[d, on = 'lad2019']
d[, imd10 := findInterval(imd_rank, quantile(imd_rank, probs = 0:9/10))]

cols <- brewer.pal(3, 'Set1')
d[, cols := 'grey60']
d$cols[d$imd10 %in% 1:3] <- cols[1]
d$cols[d$imd10 %in% 8:10] <- cols[2]

# national value

n <- d[, .(ct = sum(ct, na.rm = T), p = sum(p, na.rm = T))]
n[, rt := ct / p * 100000]

# check social gradient

sg <- d[, .(sum(ct, na.rm = T) / sum(p, na.rm = T) * 100000), imd10][order(V1)]
sg <- sg[order(imd10)]
barplot(sg$V1, ylim = c(0, 600))
abline(h = n$rt)

# confidence limits

pop <- 1:700 * 1000
ct <- pop * n$rt / 100000
ct <- round(ct, 0)
pop <- ct / n$rt * 100000

lim1 <- vpt(X = ct, N = pop / 100000, conf.level = 0.95)
lim2 <- vpt(X = ct, N = pop / 100000, conf.level = 0.99)

plot(1, type = 'n', xlim = c(0, 400000), ylim = c(0, 1400))
lines(pop, lim1[,2])
lines(pop, lim1[,3])
lines(pop, lim2[,2], lty = 3)
lines(pop, lim2[,3], lty = 3)
points(d$p, d$r, pch = 4, col = d$cols, cex = 0.7)
abline(h = n$rt)
with(d[`LA Name` == 'Bradford'], points(p, r, pch = 19, cex = 1.5))


# -----
# d3mft
# -----

d3 <- fread("d3mft.csv")
d3 <- d3[examined > 0]
d3[, pr := any_d3mft / examined]

n <- d3[, .(any_d3mft = sum(any_d3mft), examined = sum(examined))]
n[, pr := any_d3mft / examined]

# add IMD

d3 <- i[d3, on = 'lad2019']
d3[, imd10 := findInterval(imd_rank, quantile(imd_rank, probs = 0:9/10, na.rm = T))]
d3[, cols := 'grey60']
d3$cols[d3$imd10 %in% 1:3] <- cols[1]
d3$cols[d3$imd10 %in% 8:10] <- cols[2]


# confidence limits

pop <- 1:200 * 10
ct <- pop * n$pr
ct <- round(ct, 0)
pop <- ct / n$pr

lim1 <- vpt(X = ct, N = pop, conf.level = 0.95, FUN = prop.test)
lim2 <- vpt(X = ct, N = pop, conf.level = 0.99, FUN = prop.test)

emf('d3mf5yr.emf', height = 6, width = 6, family = 'Tahoma')

par(mar = c(6, 6, 1, 1))
plot(1, type = 'n', xlim = c(0, 2000), ylim = c(0, 0.5), xlab = 'Number of 5-year-olds examined, 2018-2019', ylab = NA, axes = F)
lines(pop, lim1[,2])
lines(pop, lim1[,3])
lines(pop, lim2[,2], lty = 3)
lines(pop, lim2[,3], lty = 3)
points(d3$examined, d3$pr, pch = 4, cex = 0.7, col = d3$cols)
abline(h = n$pr)
with(d3[lad_name == 'Bradford'], points(examined, pr, pch = 13, cex = 1.5))
box()
axis(1)
axis(2, 0:5/10, paste0(0:5 * 10, '%'), las = 2)
title(ylab = 'Prevalence of decay (decayed, missing, or filled)', line = 4)

dev.off()