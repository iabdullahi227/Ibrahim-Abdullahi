# Panel data example
# Traffic deaths and alcohol taxes

# load the packages and the dataset
library(AER)
library(plm)
data(Fatalities)

# pdata.frame() declares the data as panel data.
Fatalities<- pdata.frame(Fatalities, index = c("state", "year"))

# obtain the dimension and inspect the structure
is.data.frame(Fatalities)

dim(Fatalities)

str(Fatalities)

# list the first few observations
head(Fatalities)

# summarize the variables 'state' and 'year'
summary(Fatalities[, c(1, 2)])

# define the fatality rate
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

# subset the data
Fatalities1982 <- subset(Fatalities, year == "1982")
Fatalities1988 <- subset(Fatalities, year == "1988")

# estimate simple regression models using 1982 and 1988 data
fatal1982_mod <- lm(fatal_rate ~ beertax, data = Fatalities1982)
fatal1988_mod <- lm(fatal_rate ~ beertax, data = Fatalities1988)

coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1")

coeftest(fatal1988_mod, vcov. = vcovHC, type = "HC1")

# plot the observations and add the estimated regression line for 1982 data
plot(x = as.double(Fatalities1982$beertax), 
     y = as.double(Fatalities1982$fatal_rate), 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1982",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")

abline(fatal1982_mod, lwd = 1.5, col="darkred")
legend("topright",lty=1,col="darkred","Estimated Regression Line")


# plot observations and add estimated regression line for 1988 data
plot(x = as.double(Fatalities1988$beertax), 
     y = as.double(Fatalities1988$fatal_rate), 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1988",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")

abline(fatal1988_mod, lwd = 1.5,col="darkred")

#> In both plots, each point represents observations of beer tax 
#> and fatality rate for a given state in the respective year. 
#> The regression results indicate a positive relationship between the 
#> beer tax and the fatality rate for both years. 
#> The estimated coefficient on beer tax for the 1988 data is almost 
#> three times as large as for the 1982 dataset. 

## Two time periods

# compute the differences 
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax

# estimate a regression using differenced data
fatal_diff_mod <- lm(diff_fatal_rate ~ diff_beertax)

coeftest(fatal_diff_mod, vcov = vcovHC, type = "HC1")

legend("bottomright",lty=1,col="darkred","Estimated Regression Line")


# plot the differenced data
plot(x = as.double(diff_beertax), 
     y = as.double(diff_fatal_rate), 
     xlab = "Change in beer tax (in 1988 dollars)",
     ylab = "Change in fatality rate (fatalities per 10000)",
     main = "Changes in Traffic Fatality Rates and Beer Taxes in 1982-1988",
     cex.main=1,
     xlim = c(-0.6, 0.6),
     ylim = c(-1.5, 1),
     pch = 20, 
     col = "steelblue")

# add the regression line to plot
abline(fatal_diff_mod, lwd = 1.5,col="darkred")
#add legend
legend("topright",lty=1,col="darkred","Estimated Regression Line")

#>The estimated coefficient on beer tax is now negative and 
#>significantly different from zero at 5%. 
#>Its interpretation is that raising the beer tax by $1 causes 
#>traffic fatalities to decrease by 1.04 per 10000 people. 
#>This is rather large as the average fatality rate is approximately 
#>2 persons per 10000 people.

# compute mean fatality rate over all states for all time periods
mean(Fatalities$fatal_rate)

#> this outcome is likely to be a consequence of omitting factors 
#> in the single-year regression that influence the fatality rate 
#> and are correlated with the beer tax and change over time. The 
#> message is that we need to be more careful and control for such 
#> factors before drawing conclusions about the effect of a raise in 
#> beer taxes.

# Fixed Effects

fatal_fe_lm_mod <- lm(fatal_rate ~ beertax + state - 1, data = Fatalities)
fatal_fe_lm_mod

# obtain demeaned data
fatal_demeaned <- with(Fatalities,
                       data.frame(fatal_rate = fatal_rate - ave(fatal_rate, state),
                                  beertax = beertax - ave(beertax, state)))

# estimate the regression
summary(lm(fatal_rate ~ beertax - 1, data = fatal_demeaned))

# Alternatively, we can download the plm package
# install and load the 'plm' package
## install.packages("plm")

library(plm)

# estimate the fixed effects regression with plm()
fatal_fe_mod <- plm(fatal_rate ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "within")


coeftest(fatal_fe_mod, vcov. = vcovHC, type = "HC1")

# Time fixed effects

#>Controlling for variables that are constant across entities but 
#>vary over time can be done by including time fixed effects

#> The following code chunk shows how to estimate the combined entity 
#> and time fixed effects model of the relation between fatalities 
#> and beer tax


# estimate a combined time and entity fixed effects regression model

# via lm()
fatal_tefe_lm_mod <- lm(fatal_rate ~ beertax + state + year - 1, data = Fatalities)
fatal_tefe_lm_mod

# via plm()
fatal_tefe_mod <- plm(fatal_rate ~ beertax, 
                      data = Fatalities,
                      index = c("state", "year"), 
                      model = "within", 
                      effect = "twoways")

coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")

# check the class of 'state' and 'year'
class(Fatalities$state)
#> [1] "pseries" "factor"
class(Fatalities$year)
#> [1] "pseries" "factor"

# check class of the model object
class(fatal_tefe_lm_mod)
#> [1] "lm"


# obtain a summary based on heteroskedasticity-robust standard errors 
# (no adjustment for heteroskedasticity only)

coeftest(fatal_tefe_lm_mod, vcov = vcovHC, type = "HC1")[1, ]
#>   Estimate Std. Error    t value   Pr(>|t|) 
#> -0.6399800  0.2547149 -2.5125346  0.0125470

# check class of the (plm) model object
class(fatal_tefe_mod)
#> [1] "plm"        "panelmodel"

# obtain a summary based on clustered standard errors 
# (adjustment for autocorrelation + heteroskedasticity)
coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")
