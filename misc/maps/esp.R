library(raster)
library(sp)

esp2 <- getData(country = 'ESP', level = 2)
esp3 <- getData(country = 'ESP', level = 3)
esp4 <- getData(country = 'ESP', level = 4)

# Remove canary
map4 <- esp4[!esp4@data$NAME_1 %in% c('Islas Canarias', 'Islas Baleares'),]
map3 <- esp3[!esp3@data$NAME_1 %in% c('Islas Canarias', 'Islas Baleares'),]
map2 <- esp2[!esp2@data$NAME_1 %in% c('Islas Canarias', 'Islas Baleares'),]

pdf('~/Desktop/carlos.pdf',
    height = 11,
    width = 8)
plot(map4, lwd = 0.3)
plot(map2, border = 'red', add = T, fill = NA, lwd = 0.3)
dev.off()
