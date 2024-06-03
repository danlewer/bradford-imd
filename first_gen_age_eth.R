library(data.table)
library(RColorBrewer)

d <- fread("https://raw.githubusercontent.com/danlewer/bradford-imd/main/census2021_countryofbirth.csv")

# d <- fread("census2021_countryofbirth.csv")
d <- d[eth %in% c(1, 4)]
d[, eth_lab := factor(eth, c(1, 4), c('asian', 'white'))]

ns <- names(d)
ns <- ns[!(ns %in% c('con', 'n'))]
d <- d[, .(n = sum(n)), by = ns]
d[, age3 := factor(age_code, 1:6, c(1, 1, 2, 2, 3, 3))]

s <- dcast(d, eth_lab + age3 ~ country_birth, fun.aggregate = sum, value.var = 'n')
s[, tot := Other + UK]
s[, pc := Other / tot]

gap <- 0.5
x1 <- 0:5
x1 <- x1 + ifelse(x1 > 2, gap, 0)
xm <- c(0.5, 1.5, 2.5, 4, 5, 6)
cols <- brewer.pal(3, 'Pastel1')
age_labs <- c('Under\n25', '25 to\n49', '50 and\nover')
ys <- seq(40000, 70000, length.out = 3)

png('age_eth_migration.png', height = 5.5, width = 7.5, units = 'in', res = 300)

par(xpd = NA, mar = c(5, 5, 3, 8))
plot(1, type = 'n', xlim = c(0, 6.5), ylim = c(0, 75000), axes = F, xlab = NA, ylab = NA)
rect(x1, 0, x1 + 1, s$UK, col = cols[1])
rect(x1, s$UK, x1 + 1, s$tot, col = cols[2])
text(xm, -2000, age_labs, adj = c(0.5, 1))
axis(2, 0:7 * 10000, prettyNum(0:7 * 10000, big.mark = ','), pos = 0, las = 2)
axis(1, c(0, 3, 3.5, 6.5), labels = NA, pos = 0)
text(c(1.5, 5), 80000, c('Residents with\nAsian ethnicities', 'Residents with\nWhite ethnicities'), font = 2)
text(xm, s$UK/2, paste0(round(s$UK / s$tot * 100, 0), '%'))
text(xm, s$UK + s$Other/2, paste0(round(s$Other / s$tot * 100, 0), '%'))
rect(6.8, ys[-length(ys)], 7.3, ys[-1], col = cols[1:2])
text(7.4, ys[-length(ys)] + diff(ys)/2, c('Born in\nthe UK', 'First\ngeneration\nmigrant'), adj = 0)
text(3.5, -15000, 'Age', font = 2)
text(-1.5, 40000, 'Population 2021', font = 2, srt = 90)

dev.off()
