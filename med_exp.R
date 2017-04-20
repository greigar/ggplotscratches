# Coursera Reproducible Research - wk1
#
# 1. Make a plot that answers the question:
#   what is the relationship between mean covered charges (Average.Covered.Charges)
#                                and mean total payments (Average.Total.Payments)
#                            in New York?

library(readr)
library(dplyr)
library(ggplot2)

payments <- read_csv("payments.csv")

# Assume that it is New York city rather than NY state
# Could also breakdown by DRG.Definition (color = DRG.Definition)
payments %>% filter(Provider.City == 'NEW YORK') %>%                    # New York city
             ggplot(aes(x = Average.Covered.Charges,
                        y = Average.Total.Payments)) +
             geom_point() +                                             # create scatterplot
             labs(x = "Mean Covered Charges ($)",                       # set label text
                  y = "Mean Total Payments ($)") +
             ggtitle("Mean Total Payments and Mean Covered Charges",    # set title
                     "for New York City")

# save as 8 x 8
ggsave("plot1.pdf", width = 8, height = 8)



# 2. Make a plot (possibly multi-panel) that answers the question:
#   how does the relationship between mean covered charges (Average.Covered.Charges)
#                                 and mean total payments  (Average.Total.Payments)
#                             vary by medical condition (DRG.Definition)
#                             and the state in which care was received (Provider.State)?

payments %>% ggplot(aes(x     = Average.Covered.Charges,
                        y     = log10(Average.Total.Payments),            # take log10() to zoom in
                        color = DRG.Definition)) +
             geom_point(aes(alpha = 0.1)) +                               # set alpha low since point are close
             stat_smooth(method = "lm", color = "black", se = FALSE) +    # show LM line to reflect relationship
             facet_grid(DRG.Definition ~ Provider.State) +
             guides(alpha = FALSE,                                        # don't show alpha legend
                    color = guide_legend(title = "Medical Condition")) +  # set DRG.Definition legend title
             theme(legend.position = "bottom",                            # put legend at bottom since text is long
                   strip.text.y    = element_blank(),
                   legend.title    = element_text(size = 6),
                   legend.text     = element_text(size = 4 )) +
             scale_x_discrete(limits = c( 0,  100000,200000),             # only show some x labels due to space
                              labels = c("0","100",  "200") )  +
             labs(x = "Mean Covered Charges ($1000s)",
                  y = expression("Mean Total Payments"~log[10]~"($)")) +
             ggtitle("Mean Total Payments and Mean Covered Charges",    # set title
                     "By Medical Condition and State")

# save as 8 x 8
ggsave("plot2.pdf", width = 8, height = 8)


