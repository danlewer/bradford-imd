library(rgeos)
library(rgdal)
library(maptools)
gpclibPermit()
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(devEMF)

setwd("~/Documents/bradford_imd")

imd <- read.csv("imd2019_domains.csv")
imd_bradford <- imd[imd$lad_name == 'Bradford',]
bradford_lsoas <- imd_bradford[, 'lsoa11']

#  ===
#  map
#  ---

# from https://geoportal.statistics.gov.uk/datasets/ons::lsoa-dec-2011-boundaries-super-generalised-clipped-bsc-ew-v3/explore?location=52.747302%2C-2.489798%2C7.07
tract <- readOGR('shp/LSOA_(Dec_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shp')

y <- fortify(tract, region = 'LSOA11CD')
y <- y[y$id %in% bradford_lsoas,]

cols <- brewer.pal(10, 'RdYlBu')

imd_labs <- names(imd)[grepl('_decile', names(imd))]
imd[, imd_labs] <- lapply(imd[, imd_labs], function (x) factor(x, 1:10))


for(i in imd_labs) {
  
  print(ggplot() + 
    geom_map(data = imd, aes(map_id=lsoa11, fill = get(i)), map=y) +
    geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
    scale_fill_discrete(type = cols, na.value = 'grey70') +
    theme_bw() + 
    ggtitle(i) +
    theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank()))
  
}


p1 <- ggplot() + 
  geom_map(data = imd, aes(map_id=lsoa11, fill = imd_decile), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_discrete(type = cols, na.value = 'grey70') +
  theme_bw() + 
  ggtitle('IMD') +
  theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

p2 <- ggplot() + 
  geom_map(data = imd, aes(map_id=lsoa11, fill = income_decile), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_discrete(type = cols, na.value = 'grey70') +
  theme_bw() + 
  ggtitle('Income') +
  theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

p3 <- ggplot() + 
  geom_map(data = imd, aes(map_id=lsoa11, fill = employment_decile), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_discrete(type = cols, na.value = 'grey70') +
  theme_bw() + 
  ggtitle('Employment') +
  theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

p4 <- ggplot() + 
  geom_map(data = imd, aes(map_id=lsoa11, fill = education_decile), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_discrete(type = cols, na.value = 'grey70') +
  theme_bw() + 
  ggtitle('Education') +
  theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

p5 <- ggplot() + 
  geom_map(data = imd, aes(map_id=lsoa11, fill = health_decile), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_discrete(type = cols, na.value = 'grey70') +
  theme_bw() + 
  ggtitle('Health') +
  theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

p6 <- ggplot() + 
  geom_map(data = imd, aes(map_id=lsoa11, fill = crime_decile), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_discrete(type = cols, na.value = 'grey70') +
  theme_bw() + 
  ggtitle('Crime') +
  theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

p7 <- ggplot() + 
  geom_map(data = imd, aes(map_id=lsoa11, fill = housing_services_decile), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_discrete(type = cols, na.value = 'grey70') +
  theme_bw() + 
  ggtitle('Access to services') +
  theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

p8 <- ggplot() + 
  geom_map(data = imd, aes(map_id=lsoa11, fill = living_decile), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_discrete(type = cols, na.value = 'grey70') +
  theme_bw() + 
  ggtitle('Living environment') +
  theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

p9 <- ggplot() + 
  geom_map(data = imd, aes(map_id=lsoa11, fill = idaci_decile), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_discrete(type = cols, na.value = 'grey70') +
  theme_bw() + 
  ggtitle('Child poverty') +
  theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

p10 <- ggplot() + 
  geom_map(data = imd, aes(map_id=lsoa11, fill = idaopi_decile), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_discrete(type = cols, na.value = 'grey70') +
  theme_bw() + 
  ggtitle('Elderly poverty') +
  theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

#png('imd_bradford_maps.png', height = 15, width = 15, units = 'in', res = 300)
emf('imd_bradford_maps.emf', height = 10, width = 10, units = 'in')
grid.arrange(p1, p2, p3, p4, p6, p7, p8, p9, p10, ncol = 3)
dev.off()


# ---------
# ethnicity
# ---------

eth <- read.csv("ethnicity.csv")
eth$pc_white <- eth$White / eth$total
eth$pc_pakistani <- eth$Asian...Asian.British..Pakistani / eth$total
eth$pc_white2 <- findInterval(eth$pc_white, 0:9/10)
eth$pc_white2 <- factor(eth$pc_white2, 1:10, paste0(0:9 * 10, '-', 1:10 * 10 - 1))
eth$pc_pakistani2 <- findInterval(eth$pc_pakistani, 0:9/10)
eth$pc_pakistani2 <- factor(eth$pc_pakistani2, 1:10, paste0(0:9 * 10, '-', 1:10 * 10 - 1))


# -------------
# ethnicity map
# -------------

cols <- brewer.pal(10, 'PiYG')
cols <- c(colorRampPalette(c('#E2891F', 'white'))(6)[1:5],
  colorRampPalette(c('white', '#18B4E5'))(6)[2:6])

p1 <- ggplot() + 
  geom_map(data = eth, aes(map_id=lsoa2011, fill = pc_white2), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_discrete(type = cols, na.value = 'grey70') +
  theme_bw() + 
  ggtitle('Percent White British heritage') +
  theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

p2 <- ggplot() + 
  geom_map(data = eth, aes(map_id=lsoa2011, fill = pc_pakistani2), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_discrete(type = cols, na.value = 'grey70') +
  theme_bw() + 
  ggtitle('Percent Pakistani heritage') +
  theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

emf('ethnicity_bradford_maps.emf', height = 4, width = 8, units = 'in')
grid.arrange(p1, p2, ncol = 2)
dev.off()