setwd('~/Dropbox/R/coursera/data_science_jh/5_reproducible_research/1')

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(stringr)

data <- read_csv('payments.csv')

# What is the relationship between mean covered charges (Average.Covered.Charges) 
# and mean total payments (Average.Total.Payments) in New York?

# Here I kept all the data as is, I considered reducing the values to per 1000 or
# per 10,000 USD but the readability of the axis was not affected by keeping the 
# original totals so I left them alone.  The other consideration was whether to use
# logarithmic scales however initial plotting suggested a linear relationship with few
# extreme values and so to improve the readability of the graph I left the axes showing
# the original reported values.

# Plot showing Average.Covered.Charges against Average.Total.Payments
title <- 'An increase in the value of Medicare covered services causes an increase\nin the value of Medicare payments to the supplier'
ggplot(data, aes(Average.Covered.Charges, Average.Total.Payments)) +
        theme_tufte() +
        geom_point(alpha = .4, colour = 'steelblue',
                   shape = 20, size = 0.5) +
        geom_line(method = 'lm',
                  stat = 'smooth',
                    alpha = 0.7,
                  colour = 'darkred', size = 0.3) +
        theme(axis.line = element_line(size = 0.2)) +
        labs(title = title,
             y = 'Mean Total Payments (USD)',
             x = 'Mean Covered Charges (USD)')

# Save the plot as a .pdf, for this instance all the standard options sufficed
ggsave('plot_1.pdf')

# For the second plot there would be a lot more data on the screen at the same time
# and so I felt it necessary to make some adjustments.  Firstly the axis values 
# become too tightly spaced under faceting and so I have reduced the scale to value
# per 10,000 USD.  Secondly the DRG Definitions provided are quite messy, I have taken
# some effort to clean them but haven't made any adjustments to the content (such)
# as the abreviations as I don't want to risk changing the meaning of the descriptions
# and my medical knowledge doesn't stretch to that kind of level.

# Reduce scale of values to per $10000 for readability when faceted
data['Average.Total.Payments'] <- data['Average.Total.Payments'] / 10000
data['Average.Covered.Charges'] <- data['Average.Covered.Charges'] / 10000

# Use regex/stringr to mildly clean up the DRG definitions by removing superfluous
# leading numbers, they're still quite messy but I don't want to risk altering the
# meaning...
data$DRG.Definition <- str_replace_all(data$DRG.Definition, '[0-9]* - ', '')
data$DRG.Definition <- str_replace_all(data$DRG.Definition, ',', ', ')

# Add a text wrap to the DRG definitions to improve readability
data$DRG.Definition <- str_wrap(data$DRG.Definition, width = 13)

# Plot chart

title1 <- 'An increase in the value of Medicare covered services causes an increase in the\nvalue of Medicare payments to the supplier across all states and diagnosis related groups'
ggplot(data, aes(`Average.Covered.Charges`, `Average.Total.Payments`)) +
        theme_tufte() +
        geom_point(alpha = .4, colour = 'steelblue',
                   shape = 20, size = 0.5) +
        geom_line(method = 'lm',
                  stat = 'smooth',
                  alpha = 0.7,
                  colour = 'darkred', size = 0.3) +
        theme(axis.line = element_line(size = 0.2),
              panel.border = element_rect(colour = 'grey',
                                          fill = 'transparent')) +
        facet_grid(Provider.State ~ DRG.Definition) +
        labs(title = title1,
             x = 'Mean Covered Charges (/10,000 USD)',
             y = 'Mean Total Payments (/10,000 USD')

# Save chart as .pdf, all default values sufficed
ggsave('plot_2.pdf')
