library(tidyverse)
raw_data <- read_csv("~/data/electricity/downloads/NZE-energy-prices-for-3-schedules-at-3-grid-points-2017-09-03-tp-1-2017-09-03-tp-48.csv")
select_data <- raw_data[,c(1,3,4,8)]
names(select_data) <- c("poc", "period", "price", "run_type")

final_ben <- select_data %>% filter(poc == "BEN2201" & run_type == "F") %>% select(period, price)
five_ben  <- select_data %>% filter(poc == "BEN2201" & run_type == "5") %>% select(period, price)
final_five_ben_price <- tibble( final = final_ben$price, five = five_ben$price )

ggplot(final_five_ben_price, aes(x = five, y = final)) + geom_smooth(method = lm)

model <- lm(final ~ five, data = final_five_ben_price)

predict(model, data.frame( five = c(100) ) )
predict(model, data.frame( five = c(10) ) )

final            <- select_data %>% filter(run_type == "F") %>% select(price)
five             <- select_data %>% filter(run_type == "5") %>% select(price)
final_five_price <- tibble( final = final$price, five = five$price )

g <- ggplot(final_five_price, aes(x = five, y = final)) + geom_point()
g + geom_smooth(method = lm, se = FALSE)
g + geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)
g + geom_smooth(method = lm, formula = y ~ splines::ns(x, 2), se = FALSE)


model    <- lm(final ~ five, final_five_price)
model_bs <- lm(final ~ splines::bs(five, 3), final_five_price)
model_ns <- lm(final ~ splines::ns(five, 2), final_five_price)

anova(model, model_ns, model_bs)

# POC

final            <- select_data %>% filter(run_type == "F") %>% select(poc, price)
five             <- select_data %>% filter(run_type == "5") %>% select(poc, price)
final_five_price <- tibble( poc = final$poc, final = final$price, five = five$price )

g <- ggplot(final_five_price, aes(x = five, y = final, colour = poc)) + geom_point()
g + geom_smooth(method = lm, se = FALSE)
g + geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)
g + geom_smooth(method = lm, formula = y ~ splines::ns(x, 2), se = FALSE)


model    <- lm(final ~ five + poc, final_five_price)
model_bs <- lm(final ~ splines::bs(five, 3) + poc, final_five_price)
model_ns <- lm(final ~ splines::ns(five, 2) + poc, final_five_price)

anova(model, model_ns, model_bs)

