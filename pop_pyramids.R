library(data.table)
library(RColorBrewer)

setwd("C:/Users/rmhidle/OneDrive - University College London/bchds")

age_eth <- fread("https://raw.githubusercontent.com/danlewer/bradford-imd/main/census2021_age_eth_bradford.csv")
age_eth[, eth2 := factor(eth2, c('white', 'south_asian', 'other'))]
age_eth <- age_eth[, .(n = sum(n)), c('age', 'eth2', 'bradford')]

# --------------------
# Bradford vs. England
# --------------------

age <- xtabs(n ~ age + bradford, data = age_eth)

# find median
age_cumdist <- t(apply(age, 2, cumsum)) / colSums(age)
med_age <- apply(abs(age_cumdist - 0.5), 1, which.min)

age <- t(t(age) / colSums(age))
cols <- brewer.pal(4, 'Set3')
xs <- seq(0, 0.02, 0.005)
gap <- 0.005
ys <- seq(0, 90, 5)
ys_lab <- paste0(ys[-length(ys)], '-', ys[-1] - 1)
ys_mid <- ys[-length(ys)] + diff(ys)/2

png('pop_pyramid_bradford_england.png', height = 7, width = 7, units = 'in', res = 300)

par(xpd = NA)
plot(1, type = 'n', xlim = c(-0.02 - gap/2, 0.02 + gap/2), ylim = c(0, 100), axes = F, xlab = 'Percent of population', ylab = NA)
rect(gap/2, 0:90, age[,1] + gap/2, 1:91, col = cols[1])
rect(-gap/2, 0:90, -age[,2] - gap/2, 1:91, col = cols[2])
axis(1, xs + gap/2, paste0(xs * 100, '%'), pos = 0)
axis(1, -xs - gap/2, paste0(xs * 100, '%'), pos = 0)
axis(2, ys, pos = -gap/2, tck = 0.02, labels = NA)
axis(2, ys, pos = gap/2, tck = -0.02, labels = NA)
text(0, ys_mid, ys_lab)
text(0, max(ys_mid) + diff(ys)[1], paste0(max(ys), '+'))
text(0, max(ys_mid) + diff(ys)[1] * 2, 'Age')
text(0, max(ys_mid) + diff(ys)[1] * 2, 'Age', font = 2)
text(c(-0.01 - gap/2, 0.01 + gap/2), max(ys_mid) + diff(ys)[1] * 2, c('Bradford', 'England'), font = 2)
segments(-0.02 - gap/2, med_age[2], -gap/2)
segments(0.02 + gap/2, med_age[1], gap/2)
text(c(-0.02 - gap/2, 0.02 + gap/2), rev(med_age) + 5, paste0('Median\n', rev(med_age)))

dev.off()

# ------------------------------
# Bradford South Asian vs. White
# ------------------------------

age <- age_eth[bradford == T, xtabs(n ~ age + eth2)][,-3]
age <- t(t(age) / colSums(age))
age_cumdist <- t(apply(age, 2, cumsum)) / colSums(age)
med_age <- apply(abs(age_cumdist - 0.5), 1, which.min)

png('pop_pyramid_bradford_eth.png', height = 7, width = 7, units = 'in', res = 300)

par(xpd = NA)
plot(1, type = 'n', xlim = c(-0.02 - gap/2, 0.02 + gap/2), ylim = c(0, 100), axes = F, xlab = 'Percent of population', ylab = NA)
rect(gap/2, 0:90, age[,1] + gap/2, 1:91, col = cols[3])
rect(-gap/2, 0:90, -age[,2] - gap/2, 1:91, col = cols[4])
axis(1, xs + gap/2, paste0(xs * 100, '%'), pos = 0)
axis(1, -xs - gap/2, paste0(xs * 100, '%'), pos = 0)
axis(2, ys, pos = -gap/2, tck = 0.02, labels = NA)
axis(2, ys, pos = gap/2, tck = -0.02, labels = NA)
text(0, ys_mid, ys_lab)
text(0, max(ys_mid) + diff(ys)[1], paste0(max(ys), '+'))
text(0, max(ys_mid) + diff(ys)[1] * 2, 'Age')
text(0, max(ys_mid) + diff(ys)[1] * 2, 'Age', font = 2)
text(c(-0.01 - gap/2, 0.01 + gap/2), max(ys_mid) + diff(ys)[1] * 2, c('Bradford: South Asian', 'Bradford: White'), font = 2)
segments(-0.02 - gap/2, med_age[2], -gap/2)
segments(0.02 + gap/2, med_age[1], gap/2)
text(c(-0.02 - gap/2, 0.02 + gap/2), rev(med_age) + 5, paste0('Median\n', rev(med_age)))

dev.off()
