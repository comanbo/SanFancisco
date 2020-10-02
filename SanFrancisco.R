library(dplyr)
library(lattice)
library(broom)
sf <- read_excel("ADFC-0006-E.xlsx", sheet = "data")
Price <- sf$Price
sqf <- sf$`Square feet`
dayslist <-sf$`Days listed`
bedrooms <-sf$Bedrooms
loft <-sf$Loft
lotsize <- sf$Lotsize
year <- sf$Year
zip <- as.character(sf$`Zip Code`)
area <- sf$Neighborhood
area <- recode(area, "Low" = 1, "Medium" = 2, "High" = 3)
ZIP <- sf$`Zip Code`
usdpersqf <- mean (Price/sqf)
violentcrime <-sf$`Violent crime`
propertycrime <- sf$`Property crime`
str(sf)

qqmath(Price)

qqmath(log(Price))
# why i use log price instead of price https://www.investopedia.com/ask/answers/05/logvslinear.asp

# model 1
mod1 <- lm(log(Price) ~ sqf + dayslist + bedrooms + loft + lotsize + year + area + as.factor(zip), data = sf)
summary(mod1)
plot(mod1)
predict(mod1)


trellis.par.set(fontsize=list(text=7))

xyplot(log(Price) ~ (dayslist + bedrooms + loft + lotsize + year + area) | zip, data = sf,
       type = c("p", "r"), cex=.2)


# model 2 - trial with 2 extra variables - failed. 
mod1 <- lm(log(Price) ~ sqf + dayslist + bedrooms + loft + lotsize + year + area + as.factor(zip) + violentcrime, data = sf)
summary(mod1)
plot(mod1)
predict(mod1)

# plotting price / sqfoot in all zip codes
xyplot(Price~sqf|zip,
       pch=19,
       cex=.2,
       panel=function(...) {
         panel.abline(a=0,b=usdpersqf);
         panel.xyplot(...);
       })