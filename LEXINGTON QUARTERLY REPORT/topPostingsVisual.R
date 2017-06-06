library(ggplot2)
library(scales)
library(dplyr)


## ADJUST LIVING WAGE HERE 
mitLivingWage <- 22.73

## ORIGINAL DATAFRAME
originalData <- read.csv('topJobPostings.csv')

## CREATE SEPERATE DATASETS FROM ORIGINAL
topPostings <- originalData %>% top_n(30, deduplicated)
livingWageTopPostings <- originalData %>% 
  filter(Median.Hourly.Earnings >= mitLivingWage) %>%
  top_n(30, deduplicated)

topPostingsPalette <- c(
  '#A4D7F4',
  '#9C0059',
  '#D31245',
  '#F8971D',
  '#8DC63F',
  '#767662',
  '#00853F'
)
livingWagePalette <- c(
  '#9C0059',
  '#8DC63F',
  '#00853F',
  '#767662',
  '#D31245',
  '#F8971D'
)

topPostings$Typical.Entry.Level.Education <- factor(topPostings$Typical.Entry.Level.Education, levels = c("No formal educational credential",
                                                                                                          "High school diploma or equivalent",
                                                                                                          "Postsecondary nondegree award"  ,
                                                                                                          "Some college, no degree",
                                                                                                          "Associate's degree",
                                                                                                          "Bachelor's degree",
                                                                                                          "Master's degree" ,
                                                                                                          "Doctoral or professional degree",
                                                                                                          "N/A" ))

livingWageTopPostings$Typical.Entry.Level.Education <- factor(livingWageTopPostings$Typical.Entry.Level.Education, levels = c("No formal educational credential",
                                                                                                                              "High school diploma or equivalent",
                                                                                                                              "Postsecondary nondegree award"  ,
                                                                                                                              "Some college, no degree",
                                                                                                                              "Associate's degree",
                                                                                                                              "Bachelor's degree",
                                                                                                                              "Master's degree" ,
                                                                                                                              "Doctoral or professional degree",
                                                                                                                              "N/A" ))
## MAKE A FUNCTION
makePlots <- function(dataTable, upperLimit, paletteNameHere) {
  p <- ggplot(dataTable, aes(x = reorder(Occupation, deduplicated), y = deduplicated, fill = Typical.Entry.Level.Education, 
                             label = paste(Pct..25.Hourly.Earnings, '-', Pct..75.Hourly.Earnings), 
                             width = .8)) + 
    geom_bar(stat = 'identity', color = '#767662')
  p                 + 
    coord_flip()    + 
    theme_minimal() + 
    geom_text(hjust = -.05, size = 3.2, color = 'black') +
    ylab('Job Postings in Quarter')   +
    theme(axis.ticks.y      = element_blank(), 
          axis.text.y       = element_text(size = 9, face = 'bold', color = 'black'), 
          axis.text.x       = element_text(size = 9,  color = 'black'), 
          legend.title      = element_text(size = 16, color = 'black'),
          legend.text       = element_text(size = 14,  color = 'black'), 
          legend.position   = c(.68, .7), 
          legend.key        = element_rect(color = 'white', size = 1),
          legend.key.size   = unit(1, 'lines'), 
          legend.background = element_blank(),
          axis.title        = element_blank(),
          panel.grid.major  = element_blank(), 
          panel.grid.minor  = element_blank()) + 
    scale_y_continuous(limits = c(0, upperLimit), expand = c(0,0), labels = comma) +
    scale_fill_manual(values = paletteNameHere, name  = 'Typical Entry-Level Education \n')
}

makePlots(topPostings, 3450, topPostingsPalette)
makePlots(livingWageTopPostings, 1900, livingWagePalette)

# png(file="topPostings.png", width = 200, height = 500)
# makePlots(topPostings, 3450)
# dev.off() 





## PIE CHARTS
pie2 <- function(x, labels = names(x), edges = 200, radius = 0.8,
                 clockwise = FALSE, init.angle = if(clockwise) 90 else 0,
                 density = NULL, angle = 45, col = NULL, border = NULL,
                 lty = NULL, main = NULL, ...){
  if (!is.numeric(x) || any(is.na(x) | x < 0)) 
    stop("'x' values must be positive.")
  if (is.null(labels)) 
    labels <- as.character(seq_along(x))
  else labels <- as.graphicsAnnot(labels)
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]) 
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col)) 
    col <- if (is.null(density)) 
      c("white", "lightblue", "mistyrose", "lightcyan", 
        "lavender", "cornsilk")
  else par("fg")
  if (!is.null(col)) 
    col <- rep_len(col, nx)
  if (!is.null(border)) 
    border <- rep_len(border, nx)
  if (!is.null(lty)) 
    lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density)) 
    density <- rep_len(density, nx)
  twopi <- if (clockwise) 
    -2 * pi
  else 2 * pi
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
    polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i], 
            border = border[i], col = col[i], lty = lty[i])
    P <- t2xy(mean(x[i + 0:1]))
    lab <- as.character(labels[i])
    if (!is.na(lab) && nzchar(lab)) {
      lines(c(1, 1.15) * P$x, c(1, 1.15) * P$y)
      text(1.3 * P$x, 1.3 * P$y, labels[i], xpd = TRUE, 
           adj = ifelse(P$x < 0, 1, 0), ...)
    }
  }
  title(main = main, ...)
  invisible(NULL)
}

makePieChart <- function(dataToEnter, paletteToEnter){
  dataToEnter[,'deduplicated'] <- round(dataToEnter[,'deduplicated'], 0)
  dataToEnter[,'deduplicated'] <- as.numeric(as.character(dataToEnter[,'deduplicated']))
  
  allPostings <- count(dataToEnter, Typical.Entry.Level.Education, 
                       wt = deduplicated)
  
  percent <- function(x, digits = 1, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }
  
  allPostings$percent <-  percent(allPostings$n/sum(allPostings$n))
  
  pie2(allPostings$n, 
       labels = paste(allPostings$Typical.Entry.Level.Education,
                      allPostings$percent, 
                      sep = '\n'), 
       col = paletteToEnter, 
       radius = .3, cex = .8)
}

makePieChart(topPostings, topPostingsPalette)
makePieChart(livingWageTopPostings, livingWagePalette)
