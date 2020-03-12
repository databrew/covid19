library(dygraphs)
library(covid19)
library(dplyr)
library(xts)

data <- prepare_day_zero_data()
don <- xts(x = data$value, order.by = data$date)

p <- dygraph(don,
             group = don$country) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)
p
