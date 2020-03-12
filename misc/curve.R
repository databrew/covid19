library(covid19)
library(ggplot2)
library(dplyr)
library(tidyr)

vals <- (c(1:100 ))^3
vals <- c(vals, rev(vals))
dates <- 1:length(vals)


lo <- loess(vals ~ dates)
vals <- lo$fitted

valsb <- (1:100)^2.8
valsx <- seq(100^2.8, 100^2.7, length = 100)
valsb <- c(valsb, valsx)
lo <- loess(valsb ~ dates)
valsb <- lo$fitted



pd <- tibble(date = dates,
             Uncontrolled = vals,
             Controlled = valsb)
pd <- gather(pd, key, value, Uncontrolled: Controlled)
pd$key <- factor(pd$key,
                 levels = rev(c('Controlled', 'Uncontrolled')),
                 labels = rev(c('Amb mesures preventives',
                            'Sense mesures preventives')))

ylimit = max(pd$value[pd$key == 'Amb mesures preventives']) * 1.1

pd <- pd %>% filter(value >= 0)
ggplot(data = pd) +
  geom_line(aes(x = date,
                y = value,
                group = key,
                color = key),
            alpha = 0.8, size = 3) +
  # geom_bar(stat = 'identity', 
  #          position = 'dodge',
  #          aes(x = date,
  #              y = value,
  #              group = key,
  #              fill = key)) +
  geom_hline(yintercept = ylimit, lty = 2) +
  theme_simple() +
  scale_color_manual(name = '',
                     values = c('black', 'red')) +
  theme(axis.text = element_blank()) +
  labs(x = 'Dia',
       y = 'Casos diaris',
       title = '2 escenaris d\'un brot') +
  theme(legend.position = 'top') +
  theme(legend.text = element_text(size = 20)) +
  geom_text(data = tibble(x = 35,
                          y = ylimit * 1.0,
                          label = 'Capacitat màxima dels\nserveis sanitaris'),
            aes(x = x, y = y, label = label),
            size = 6) +
  theme(axis.title = element_text(size = 20)) +
  guides(color = guide_legend(ncol = 1)) +
  theme(plot.title = element_text(size = 35, hjust = 0.5))
