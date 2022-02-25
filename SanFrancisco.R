# install R Packages ----
# install.packages("readxl")
# install_github("vqv/ggbiplot")
# install.packages("ggbiplot")
# install.packages("stringr")

# load libraries -----
library(readxl)
library(stringr)
library(ggbiplot)
library(broom)

# disable scientific notation in R ------
options(scipen = 999)

# load Excel data -----
d <- read_xlsx("ADFC-0006-E.xlsx", sheet="data")
summary(d)

# categorize and clean data -----
d_clean <- d[, c(-1, -3, -4, -8)]
# d_clean$Address <- sub("#.*", "", d_clean$Address)
d_clean$Address <- paste(str_split(sub(" #.*", "", d_clean$Address), " ", n = 2, 
                                   simplify = TRUE)[, 2], d$`Zip Code`)
d_clean$Bedrooms <- ifelse(d_clean$Bedrooms < 8, 
                           d_clean$Bedrooms, 8)

d$Bedrooms <- ifelse(d$Bedrooms < 8, 
                           d$Bedrooms, 8)

mod3 <-lm(d$Price ~ d$Street, data = d)
m = lm(d$Price ~ d$Street, data = d)
tm = tidy(m)
tm
tm$term[tm$p.value < 0.05]

### Quantitative Analysis ----
# predictive regression model -----
mod_clean <- lm(Price ~ ., data=d_clean)
summary(mod_clean)

# calculate predicted values ----
d_clean$`Predicted Price` <- predict(mod_clean, data=d_clean)

# calculate residuals ----
d_clean$`Residual Price` <- d_clean$Price - d_clean$`Predicted Price`

# residuals errors distribution ----
round(stats::quantile(d_clean$`Residual Price`, probs=seq(0.05, 1, by=0.15)), 2)
d_clean %>% ggplot(aes(`Residual Price`)) + 
  geom_histogram(binwidth=100000, alpha=0.3, color="blue", fill="brown") + 
  labs(title = "Prediction Residuals", x = "Error ($)", y = "Frequency")

c("Error mean", mean(d_clean$`Residual Price`))
c("Error Standard deviation.", sd(d_clean$`Residual Price`))
sd_error <- round(sd(d_clean$`Residual Price`), 3)
c("Confidence Interval (95%):", -2*sd_error, +2*sd_error)


### Qualitative Analysis ----
# qualitative regression model -----
mod_short <- lm(Price ~ `Zip Code` + `Days listed` + `Bedrooms`+ `Square feet`+
                  Lotsize + Year + Neighborhood, data=d)
summary(mod_short)

# average price per Zip Code ----
avg_zip <- setNames(aggregate(x = d$Price, by = list(d$`Zip Code`), 
                                    FUN = "mean"), c("Zip Code", "Avg. Price"))
avg_zip <- avg_zip %>%
  mutate(color = (min(`Avg. Price`) == `Avg. Price` | 
                    max(`Avg. Price`) == `Avg. Price`))

summary(avg_zip)
ggplot(avg_zip, aes(`Zip Code`, `Avg. Price`, label=`Zip Code`)) + 
  geom_text(aes(label=`Zip Code`, color = color), 
            size=2.5, alpha=0.7, check_overlap =TRUE) + 
  scale_color_manual(values = c("blue", "red"))

rattle()
