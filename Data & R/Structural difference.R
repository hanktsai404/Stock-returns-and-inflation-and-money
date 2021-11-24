# This program is for testing structural difference in the effect inflation
# and money have on stock prices
# Creator: Hank Tsai 
# b07703014@ntu.edu.tw
# Department of Finance, National Taiwan University

# Lag function
lag = function(seq, time)
{
  n = length(seq)
  for(i in c(1:time))
  {
    seq = c(NA, seq[-n])
  }
  return(seq)
}

library(ggplot2)
library(scales)
library(ggthemes)
# Barro's money equation
money_data = read.csv("money_data_preprocessed.csv")
money_data = money_data[complete.cases(money_data),]
money_data$Date = as.Date(money_data$Date)
money_data$RDM_lag1 = lag(money_data$RDM, 1)
money_data$RDM_lag2 = lag(money_data$RDM, 2)
money_data$RDM_lag3 = lag(money_data$RDM, 3)
money_data$RDM_lag4 = lag(money_data$RDM, 4)
money_data$RDM_lag5 = lag(money_data$RDM, 5)
money_data$RDM_lag6 = lag(money_data$RDM, 6)
money_data$UnempIdx_lag1 = lag(money_data$UnempIdx, 1)
money_data$UnempIdx_lag2 = lag(money_data$UnempIdx, 2)
money_data$UnempIdx_lag3 = lag(money_data$UnempIdx, 3)

money_lm = lm(RDM ~ RDM_lag1 + RDM_lag2 + RDM_lag3 + RDM_lag4 + RDM_lag5 +
                RDM_lag6 + FEDV + UnempIdx_lag1 + UnempIdx_lag2 + UnempIdx_lag3,
              data = money_data)
summary(money_lm)
library(lmtest)
dwtest(money_lm)  # No sign of arising autocorrelation issue
money_data$ex_DM = c(NA, NA, NA, NA, NA, NA, money_lm$fitted.values)
money_data$unex_DM = money_data$RDM - ex_DM

ggplot(data = money_data, aes(x = RDM, y = unex_DM)) + geom_point() +
  theme_economist_white() + labs(x = "Real Money Growth Rate", y = "Unexpected Money Growth Rate") +
  ggtitle("Real Money Growth and Unexpected Money Growth")
# The diagram shows that the unexpected growth rate is correlated with money growth rate itself,
# meaning that the higher the growth rate will be, the ability of precise estimation is lower

ggplot(money_data, aes(Date, unex_DM, group = 1, na.rm = TRUE)) + geom_line(color = "#2828FF", size = 0.5) + 
  labs(x = "Time", y = "Unexpected Money Growth Rate") + geom_smooth(color = "black", size = 1, method = "lm", se = FALSE, na.rm = TRUE) +
  scale_x_date(breaks = as.Date(c("1975/3/1", "1985/3/1", "1995/3/1", "2005/3/1", "2015/3/1")), date_labels = "%Y") +
  theme_economist_white() + scale_colour_economist() + ggtitle("Unexpected Money Growth")
# Extreme value happens in Q4, 2008

library(tseries)
# Unexpected inflation (Fama's method)
return_data = read.csv("return_data_preprocessed.csv")

leg = return_data$Dummy == 0
for(i in c(1:length(leg)))
{
  if(leg[i])
  {
    leg[i] = "Before 2008 Q4"
  }
  else
  {
    leg[i] = "After 2008 Q4"
  }
}
return_data$leg = leg

return_data$Date = as.Date(return_data$Date)
return_data$ex_DM = money_data$ex_DM
return_data$unex_DM = money_data$unex_DM

adf.test(log(return_data$DollarIdx))
return_data$DIchange = log(return_data$DollarIdx) - log(lag(return_data$DollarIdx, 1))

TB_minus_I = lag(return_data$TB, 1) - return_data$Inf.

# ADF test with no intercept
library(fUnitRoots)
adfTest(TB_minus_I, type = c("nc"))
Inf_lm = lm(TB_minus_I ~ 0 + lag(TB_minus_I, 1))
summary(Inf_lm)
ER = c(NA, NA, Inf_lm$fitted.values)
return_data$ex_Inf = return_data$TB - ER
return_data$unex_Inf = return_data$Inf. - return_data$ex_Inf

ggplot(data = return_data, aes(x = Inf., y = unex_Inf)) + geom_point() +
  theme_economist_white() + labs(x = "Realized Inflation", y = "Unexpected inflation") +
  ggtitle("Realized Inflation and Unexpected Inflation")

ggplot(return_data, aes(Date, unex_Inf, group = 1, na.rm = TRUE)) + geom_line(color = "#2828FF", size = 0.5) + 
  labs(x = "Time", y = "Unexpected Inflation") + geom_smooth(color = "black", size = 1, method = "lm", se = FALSE, na.rm = TRUE) +
  scale_x_date(breaks = as.Date(c("1975/3/1", "1985/3/1", "1995/3/1", "2005/3/1", "2015/3/1")), date_labels = "%Y") +
  theme_economist_white() + scale_colour_economist() + ggtitle("Unexpected Inflation")

return_data$DIchange_lag1 = lag(return_data$DIchange, 1)
return_data$DIchange_lag2 = lag(return_data$DIchange, 2)


library(sandwich)
# Mod1: Return, DI
mod1_lm = lm(Rreturn ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth + DIchange + DIchange_lag1 + DIchange_lag2, data = return_data)
summary(mod1_lm)  # Invalid, we need Newey-West
mod1_HC = NeweyWest(mod1_lm)
coeftest(mod1_lm, vcov. = mod1_HC)

# Mod2: Return, No DI
mod2_lm = lm(Rreturn ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth, data = return_data)
summary(mod2_lm)  # Invalid, we need Newey-West
mod2_HC = NeweyWest(mod2_lm)
coeftest(mod2_lm, vcov. = mod2_HC)

# Mod3: Return, before 2008 Q4
before_data = return_data[return_data$Dummy == 0,]
after_data = return_data[return_data$Dummy == 1,]
mod3_lm = lm(Rreturn ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth, data = before_data)
summary(mod3_lm)  # Invalid, we need Newey-West
mod3_HC = NeweyWest(mod3_lm)
coeftest(mod3_lm, vcov. = mod3_HC)

# Mod4: Return, after 2008 Q4
mod4_lm = lm(Rreturn ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth, data = after_data)
summary(mod4_lm)  # Invalid, we need Newey-West
mod4_HC = NeweyWest(mod4_lm)
coeftest(mod4_lm, vcov. = mod4_HC)

# Mod5: Return, Chow on ex_DM
return_data$DumXex_DM = return_data$Dummy * return_data$ex_DM
mod5_lm = lm(Rreturn ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth + Dummy + DumXex_DM, data = return_data)
mod5_HC = NeweyWest(mod5_lm)
library(car)
linearHypothesis(mod5_lm, c("Dummy = 0", "DumXex_DM = 0"), test = "F", vcov. = mod5_HC)
# No structural change
interceptB = mean(before_data$Rreturn) - coef(mod3_lm)["ex_DM"]*mean(before_data$ex_DM, na.rm = TRUE)
interceptA = mean(after_data$Rreturn) - coef(mod4_lm)["ex_DM"]*mean(after_data$ex_DM, na.rm = TRUE)
ggplot(data = return_data, aes(x = ex_DM, y = Rreturn, color = leg)) + geom_point(size = 3) + 
  geom_abline(intercept = interceptB, slope = coef(mod3_lm)["ex_DM"], size = 1, color = "#003060") +
  geom_abline(intercept = interceptA, slope = coef(mod4_lm)["ex_DM"], size = 1, color = "#2894FF") +
  labs(x = "Expected Money Growth Rate", y = "Real Stock Return", col = "Legend") + theme_economist_white() + 
  scale_colour_economist() + ggtitle("Return and Expected Money growth")

# Mod6: Return, Chow on unex_DM
return_data$DumXunex_DM = return_data$Dummy * return_data$unex_DM
mod6_lm = lm(Rreturn ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth + Dummy + DumXunex_DM, data = return_data)
mod6_HC = NeweyWest(mod6_lm)
coeftest(mod6_lm, vcov. = mod6_HC)
linearHypothesis(mod6_lm, c("Dummy = 0", "DumXunex_DM = 0"), test = "F", vcov. = mod6_HC)
# Structural change
interceptB = mean(before_data$Rreturn) - coef(mod3_lm)["unex_DM"]*mean(before_data$unex_DM, na.rm = TRUE)
interceptA = mean(after_data$Rreturn) - coef(mod4_lm)["unex_DM"]*mean(after_data$unex_DM, na.rm = TRUE)
ggplot(data = return_data, aes(x = unex_DM, y = Rreturn, color = leg)) + geom_point(size = 3) + 
  geom_abline(intercept = interceptB, slope = coef(mod3_lm)["unex_DM"], size = 1, color = "#003060") +
  geom_abline(intercept = interceptA, slope = coef(mod4_lm)["unex_DM"], size = 1, color = "#2894FF") +
  labs(x = "Unexpected Money Growth Rate", y = "Real Stock Return", col = "Legend") + theme_economist_white() + 
  scale_colour_economist() + ggtitle("Return and Unexpected Money growth")

# Mod7: Return, Chow on ex_Inf
return_data$DumXex_Inf = return_data$Dummy * return_data$ex_Inf
mod7_lm = lm(Rreturn ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth + Dummy + DumXex_Inf, data = return_data)
mod7_HC = NeweyWest(mod7_lm)
linearHypothesis(mod7_lm, c("Dummy = 0", "DumXex_Inf = 0"), test = "F", vcov. = mod7_HC)
# No structural change? 
interceptB = mean(before_data$Rreturn) - coef(mod3_lm)["ex_Inf"]*mean(before_data$ex_Inf, na.rm = TRUE)
interceptA = mean(after_data$Rreturn) - coef(mod4_lm)["ex_Inf"]*mean(after_data$ex_Inf, na.rm = TRUE)
ggplot(data = return_data, aes(x = ex_Inf, y = Rreturn, color = leg)) + geom_point(size = 3) + 
  geom_abline(intercept = interceptB, slope = coef(mod3_lm)["ex_Inf"], size = 1, color = "#003060") +
  geom_abline(intercept = interceptA, slope = coef(mod4_lm)["ex_Inf"], size = 1, color = "#2894FF") +
  labs(x = "Expected Inflation", y = "Real Stock Return", col = "Legend") + theme_economist_white() + 
  scale_colour_economist() + ggtitle("Return and Expected Inflation")

# Mod8: Return, Chow on unex_Inf
return_data$DumXunex_Inf = return_data$Dummy * return_data$unex_Inf
mod8_lm = lm(Rreturn ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth + Dummy + DumXunex_Inf, data = return_data)
mod8_HC = NeweyWest(mod8_lm)
linearHypothesis(mod8_lm, c("Dummy = 0", "DumXunex_Inf = 0"), test = "F", vcov. = mod8_HC)
# Structural change
interceptB = mean(before_data$Rreturn) - coef(mod3_lm)["unex_Inf"]*mean(before_data$unex_Inf, na.rm = TRUE)
interceptA = mean(after_data$Rreturn) - coef(mod4_lm)["unex_Inf"]*mean(after_data$unex_Inf, na.rm = TRUE)
ggplot(data = return_data, aes(x = unex_Inf, y = Rreturn, color = leg)) + geom_point(size = 3) + 
  geom_abline(intercept = interceptB, slope = coef(mod3_lm)["unex_Inf"], size = 1, color = "#003060") +
  geom_abline(intercept = interceptA, slope = coef(mod4_lm)["unex_Inf"], size = 1, color = "#2894FF") +
  labs(x = "Unexpected Inflation", y = "Real Stock Return", col = "Legend") + theme_economist_white() + 
  scale_colour_economist() + ggtitle("Return and Unexpected Inflation")

# Mod9: Risk premium, DI
return_data$risk_pre = return_data$Rreturn + return_data$Inf. - return_data$TB
mod9_lm = lm(risk_pre ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth + DIchange + DIchange_lag1 + DIchange_lag2, data = return_data)
summary(mod9_lm)  # Invalid, we need Newey-West
mod9_HC = NeweyWest(mod9_lm)
coeftest(mod9_lm, vcov. = mod9_HC)

# Mod9a: Risk premium, no DI
mod9a_lm = lm(risk_pre ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth, data = return_data)
summary(mod9a_lm)  # Invalid, we need Newey-West
mod9a_HC = NeweyWest(mod9a_lm)
coeftest(mod9a_lm, vcov. = mod9a_HC)

# Mod10: Risk premium, before 2008 Q4
before_data = return_data[return_data$Dummy == 0,]
after_data = return_data[return_data$Dummy == 1,]
mod10_lm = lm(risk_pre ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth, data = before_data)
summary(mod10_lm)  # Invalid, we need Newey-West
mod10_HC = NeweyWest(mod10_lm)
coeftest(mod10_lm, vcov. = mod10_HC)

# Mod11: Risk premium, after 2008 Q4
mod11_lm = lm(risk_pre ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth, data = after_data)
summary(mod11_lm)  # Invalid, we need Newey-West
mod11_HC = NeweyWest(mod11_lm)
coeftest(mod11_lm, vcov. = mod11_HC)

# Mod12: Risk premium, Chow ex_DM
mod12_lm = lm(risk_pre ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth + Dummy + DumXex_DM, data = return_data)
mod12_HC = NeweyWest(mod12_lm)
linearHypothesis(mod12_lm, c("Dummy = 0", "DumXex_DM = 0"), test = "F", vcov. = mod12_HC)
# Structural change
interceptB = mean(before_data$risk_pre) - coef(mod10_lm)["ex_DM"]*mean(before_data$ex_DM, na.rm = TRUE)
interceptA = mean(after_data$risk_pre) - coef(mod11_lm)["ex_DM"]*mean(after_data$ex_DM, na.rm = TRUE)
ggplot(data = return_data, aes(x = ex_DM, y = risk_pre, color = leg)) + geom_point(size = 3) + 
  geom_abline(intercept = interceptB, slope = coef(mod10_lm)["ex_DM"], size = 1, color = "#003060") +
  geom_abline(intercept = interceptA, slope = coef(mod11_lm)["ex_DM"], size = 1, color = "#2894FF") +
  labs(x = "Expected Money Growth Rate", y = "Risk Premium", col = "Legend") + theme_economist_white() + 
  scale_colour_economist() + ggtitle("Risk Premium and Expected Money growth")

# Mod13: Risk premium, chow unex_DM
mod13_lm = lm(risk_pre ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth + Dummy + DumXunex_DM, data = return_data)
mod13_HC = NeweyWest(mod13_lm)
linearHypothesis(mod13_lm, c("Dummy = 0", "DumXunex_DM = 0"), test = "F", vcov. = mod13_HC)
# Structural change
interceptB = mean(before_data$risk_pre) - coef(mod10_lm)["unex_DM"]*mean(before_data$unex_DM, na.rm = TRUE)
interceptA = mean(after_data$risk_pre) - coef(mod11_lm)["unex_DM"]*mean(after_data$unex_DM, na.rm = TRUE)
ggplot(data = return_data, aes(x = unex_DM, y = risk_pre, color = leg)) + geom_point(size = 3) + 
  geom_abline(intercept = interceptB, slope = coef(mod10_lm)["unex_DM"], size = 1, color = "#003060") +
  geom_abline(intercept = interceptA, slope = coef(mod11_lm)["unex_DM"], size = 1, color = "#2894FF") +
  labs(x = "Unexpected Money Growth Rate", y = "Risk Premium", col = "Legend") + theme_economist_white() + 
  scale_colour_economist() + ggtitle("Risk Premium and Unexpected Money growth")

# Mod14: Risk premium, chow ex_Inf
mod14_lm = lm(risk_pre ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth + Dummy + DumXex_Inf, data = return_data)
mod14_HC = NeweyWest(mod14_lm)
linearHypothesis(mod14_lm, c("Dummy = 0", "DumXex_Inf = 0"), test = "F", vcov. = mod14_HC)
# Structural change
interceptB = mean(before_data$risk_pre) - coef(mod10_lm)["ex_Inf"]*mean(before_data$ex_Inf, na.rm = TRUE)
interceptA = mean(after_data$risk_pre) - coef(mod11_lm)["ex_Inf"]*mean(after_data$ex_Inf, na.rm = TRUE)
ggplot(data = return_data, aes(x = ex_Inf, y = risk_pre, color = leg)) + geom_point(size = 3) + 
  geom_abline(intercept = interceptB, slope = coef(mod10_lm)["ex_Inf"], size = 1, color = "#003060") +
  geom_abline(intercept = interceptA, slope = coef(mod11_lm)["ex_Inf"], size = 1, color = "#2894FF") +
  labs(x = "Expected Inflation", y = "Risk Premium", col = "Legend") + theme_economist_white() + 
  scale_colour_economist() + ggtitle("Risk Premium and Expected Inflation")

# Mod15: Risk premium, chow unex_Inf
mod15_lm = lm(risk_pre ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth + Dummy + DumXunex_Inf, data = return_data)
mod15_HC = NeweyWest(mod15_lm)
linearHypothesis(mod15_lm, c("Dummy = 0", "DumXunex_Inf = 0"), test = "F", vcov. = mod15_HC)
# Structural change
interceptB = mean(before_data$risk_pre) - coef(mod10_lm)["unex_Inf"]*mean(before_data$unex_Inf, na.rm = TRUE)
interceptA = mean(after_data$risk_pre) - coef(mod11_lm)["unex_Inf"]*mean(after_data$unex_Inf, na.rm = TRUE)
ggplot(data = return_data, aes(x = unex_Inf, y = risk_pre, color = leg)) + geom_point(size = 3) + 
  geom_abline(intercept = interceptB, slope = coef(mod10_lm)["unex_Inf"], size = 1, color = "#003060") +
  geom_abline(intercept = interceptA, slope = coef(mod11_lm)["unex_Inf"], size = 1, color = "#2894FF") +
  labs(x = "Unexpected Inflation", y = "Risk Premium", col = "Legend") + theme_economist_white() + 
  scale_colour_economist() + ggtitle("Risk Premium and Unexpected Inflation")

# Mod16: Can return predict GDP growth?
return_data$return_lag1 = lag(return_data$Rreturn, 1)
return_data$return_lag2 = lag(return_data$Rreturn, 2)
return_data$return_lag3 = lag(return_data$Rreturn, 3)
mod16_lm = lm(RGDPgrowth ~ return_lag1 + return_lag2 + return_lag3 + ex_DM + unex_DM + ex_Inf + unex_Inf, data = return_data)
summary(mod16_lm)
mod16_HC = NeweyWest(mod16_lm)
coeftest(mod16_lm, vcov. = mod16_HC)
intercept = mean(return_data$RGDPgrowth) - coef(mod16_lm)["return_lag1"]*mean(return_data$return_lag1, na.rm = TRUE)
ggplot(data = return_data, aes(x = return_lag1, y = RGDPgrowth)) + geom_point(size = 3) +
  geom_abline(intercept = intercept, slope = coef(mod16_lm)["return_lag1"]) + 
  labs(x = "Real Stock Return (lagged 1 Qtr)", y = "Real GDP geowth") + 
  theme_economist_white() + scale_colour_economist() + ggtitle("Real GDP Growth and Stock Return")

# Mod17: Can risk premium predit GDP growth?
return_data$risk_lag1 = lag(return_data$risk_pre, 1)
return_data$risk_lag2 = lag(return_data$risk_pre, 2)
return_data$risk_lag3 = lag(return_data$risk_pre, 3)
mod17_lm = lm(RGDPgrowth ~ risk_lag1 + risk_lag2 + risk_lag3 + ex_DM + unex_DM + ex_Inf + unex_Inf, data = return_data)
summary(mod17_lm)
mod17_HC = NeweyWest(mod17_lm)
coeftest(mod17_lm, vcov. = mod17_HC)
intercept = mean(return_data$RGDPgrowth) - coef(mod17_lm)["risk_lag1"]*mean(return_data$risk_lag1, na.rm = TRUE)
ggplot(data = return_data, aes(x = risk_lag1, y = RGDPgrowth)) + geom_point(size = 3) +
  geom_abline(intercept = intercept, slope = coef(mod17_lm)["risk_lag1"]) + 
  labs(x = "Risk Premium (lagged 1 Qtr)", y = "Real GDP geowth") + 
  theme_economist_white() + scale_colour_economist() + ggtitle("Real GDP Growth and Risk Premium")

# Mod18: mod16 before 2008 Q4
before_data$return_lag1 = lag(before_data$Rreturn, 1)
before_data$return_lag2 = lag(before_data$Rreturn, 2)
before_data$return_lag3 = lag(before_data$Rreturn, 3)
mod18_lm = lm(RGDPgrowth ~ return_lag1 + return_lag2 + return_lag3 + ex_DM + unex_DM + ex_Inf + unex_Inf, data = before_data)
summary(mod18_lm)
mod18_HC = NeweyWest(mod18_lm)
coeftest(mod18_lm, vcov. = mod18_HC)

# Mod19: mod16 after 2008 Q4
after_data$return_lag1 = lag(after_data$Rreturn, 1)
after_data$return_lag2 = lag(after_data$Rreturn, 2)
after_data$return_lag3 = lag(after_data$Rreturn, 3)
mod19_lm = lm(RGDPgrowth ~ return_lag1 + return_lag2 + return_lag3 + ex_DM + unex_DM + ex_Inf + unex_Inf, data = after_data)
summary(mod19_lm)
mod19_HC = NeweyWest(mod19_lm)
coeftest(mod19_lm, vcov. = mod19_HC)

# Mod20: chow overall, Return
library(gap)
no_na_data = return_data[,c("Date","ex_DM", "unex_DM", "ex_Inf", "unex_Inf", "RGDPgrowth", "risk_pre", "Rreturn", "Dummy")]
no_na_data = no_na_data[complete.cases(no_na_data),]
chow.test(no_na_data[no_na_data$Dummy == 0,c("Rreturn")], data.matrix(no_na_data[no_na_data$Dummy == 0,c("ex_DM", "unex_DM", "ex_Inf", "unex_Inf", "RGDPgrowth")]),
          no_na_data[no_na_data$Dummy == 1,c("Rreturn")], data.matrix(no_na_data[no_na_data$Dummy == 1,c("ex_DM", "unex_DM", "ex_Inf", "unex_Inf", "RGDPgrowth")]))

return_data$DumXGDP = return_data$Dummy * return_data$RGDPgrowth
mod20_lm = lm(Rreturn ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth + Dummy + DumXex_DM + DumXunex_DM + DumXex_Inf + DumXunex_Inf + DumXGDP, data = return_data)
mod20_HC = NeweyWest(mod20_lm)
linearHypothesis(mod20_lm, c("Dummy = 0", "DumXex_DM = 0", "DumXunex_DM = 0", "DumXex_Inf = 0", "DumXunex_Inf = 0", "DumXGDP = 0"), vcov. = mod20_HC)

# Mod21: chow overall, risk premium
chow.test(no_na_data[no_na_data$Dummy == 0,c("risk_pre")], data.matrix(no_na_data[no_na_data$Dummy == 0,c("ex_DM", "unex_DM", "ex_Inf", "unex_Inf", "RGDPgrowth")]),
          no_na_data[no_na_data$Dummy == 1,c("risk_pre")], data.matrix(no_na_data[no_na_data$Dummy == 1,c("ex_DM", "unex_DM", "ex_Inf", "unex_Inf", "RGDPgrowth")]))

mod21_lm = lm(risk_pre ~ ex_DM + unex_DM + ex_Inf + unex_Inf + RGDPgrowth + Dummy + DumXex_DM + DumXunex_DM + DumXex_Inf + DumXunex_Inf + DumXGDP, data = return_data)
mod21_HC = NeweyWest(mod21_lm)
linearHypothesis(mod21_lm, c("Dummy = 0", "DumXex_DM = 0", "DumXunex_DM = 0", "DumXex_Inf = 0", "DumXunex_Inf = 0", "DumXGDP = 0"), vcov. = mod21_HC)


