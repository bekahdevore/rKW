library(plotly)

ds <- data.frame(labels = c("In Labor Force", "Not participating"),
                 values = c(68, 32))

plot_ly(ds, labels = labels, values = values, type = "pie") %>%
  layout(title = "Louisville")