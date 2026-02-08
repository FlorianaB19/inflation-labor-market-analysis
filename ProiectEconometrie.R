rm(list = ls()) 
directory <- "C:/Users/Violeta/Desktop/facultate/AN II/SEM 2/ECONOMETRIE/"

# Instalare si activare pachete
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", 
                  "olsrr", "moments","whitestrap","ggplot2","DataCombine","car", 
                  "caret","mltools","MLmetrics","tseries",
                  "gplots","plm","readxl","foreign","lmtest")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}
annualEarnings <- read.csv(paste0(directory, "LifeExpectancy.csv"))

#APLICATIA 1******************************

# 3) Analiza descriptiva a variabilelor ----------------------------------
# Calcularea coeficientului de corelatie
vars_of_interest <- annualEarnings %>% select(Annual_net_earnings, Inflation_rate)
cor(vars_of_interest) 

# Calcularea valorilor descriptive
stargazer(vars_of_interest,type='text')

cat("\nStatistici descriptive:\n")
print(summary(vars_of_interest))

cat("\nDeviația standard:\n")
print(sapply(vars_of_interest, sd, na.rm = TRUE))

# Crearea graficelor pentru distribuție
par(mfrow = c(1, 2))
hist(vars_of_interest$Annual_net_earnings, main = "Distribuția Annual_net_earnings", 
     xlab = "Annual_net_earnings", col = "lightblue", border = "black")
hist(vars_of_interest$Inflation_rate, main = "Distribuția Inflation_rate", 
     xlab = "Inflation_rate", col = "lightgreen", border = "black")



# 4) Reprezentarea corelogramei si masurarea intensitatii legaturii --------
library(ggplot2)
ggplot(vars_of_interest, aes(x = Inflation_rate, y = Annual_net_earnings)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Corelograma dintre Annual_net_earnings si Inflation_rate", 
       x = "Inflation_rate", y = "Annual_net_earnings") +
  theme_minimal()

# Calcularea coeficientului de corelație Pearson
correlation <- cor(vars_of_interest$Annual_net_earnings, vars_of_interest$Inflation_rate, use = "complete.obs")
cat("\nCoeficientul de corelație Pearson este:\n")
print(correlation)



# 5) Regresie Simpla -----------------------------------------------------
rs_annualEarnings <- lm(formula = Annual_net_earnings ~ Inflation_rate, data = annualEarnings)
summary(rs_annualEarnings)

#Bonitatea
# Afisarea lui R-squared
summary(rs_annualEarnings)$r.squared *100

# Afisarea lui R-squared ajustat
summary(rs_annualEarnings)$adj.r.squared * 100

# Verificarea semnificativității parametrilor
cat("\nSemnificația parametrilor:\n")
cat("P-valoarea pentru intercept: ", coef(summary(rs_annualEarnings))[1, 4], "\n")
cat("P-valoarea pentru panta: ", coef(summary(rs_annualEarnings))[2, 4], "\n")
cat("P-valoare model: ", pf(summary(rs_annualEarnings)$fstatistic[1], 
                            summary(rs_annualEarnings)$fstatistic[2], 
                            summary(rs_annualEarnings)$fstatistic[3], 
                            lower.tail = FALSE), "\n")

# Ipoteze

# Este modelul liniar in parametri?
# Da, deoarece poate fi scris ca o functie liniara => ipoteza 1 acceptata
# Annual_Net_Earnings = 24982 -1523.6 * Inflation_rate + epsilon

# Nr observatii > nr variabile independente
nobs(rs_annualEarnings) > (rs_annualEarnings$rank-1) # ipoteza 2 acceptata

# Modelul de regresie este corect specificat => ipoteza 3 acceptata
# Relatia dintre variabile este liniara si reflectata corect de model, asa cum arata coeficientul negativ pentru rata inflatiei (-1523.6). 

# Verificarea variabilitatii
var(vars_of_interest$Inflation_rate) # este pozitiva => ipoteza 4 acceptata

# Verificarea mediei reziduurilor
mean(rs_annualEarnings$residuals) #medie aproape de 0 => ipoteza 5 acceptata

# Verificare multicoliniaritate 
#=> nu este cazul la model simplu de regresie => ipoteaz 6 acceptata

# Verificare corelatie intre reziduuri si variabile independente
cor.test(vars_of_interest$Inflation_rate, rs_annualEarnings$residuals)
# nivel de corelatie foarte mic si p-value > 0.1 => nu sunt corelate => ipoteza 7 acceptata



# 6) Prognoza
#1. Testarea heteroschedasticitatii
ggplot(data = annualEarnings, mapping = aes(x = Inflation_rate, y = rs_annualEarnings$residuals)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Inflation_rate')
#din plot pare sa avem heteroschedasticitate

# Teste: Breusch-Pagan si White
# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)
bptest(rs_annualEarnings) #>0.1 => la 99% reziduurile sunt homoschedastice
white_test(rs_annualEarnings)#>0.1 => la 99% reziduurile sunt homoschedastice

#2. Testarea autocorelarii
acf(rs_annualEarnings$residuals) # nu pare sa avem reziduuri autocorelate

# Teste: Durbin-Watson si Breusch-Godfrey
# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate
dwtest(rs_annualEarnings) #>0.1 => reziduurile nu sunt autocorelate la ordin 1
bgtest(rs_annualEarnings) #>0.1 => reziduurile nu sunt autocorelate la ordin 1
bgtest(rs_annualEarnings,order=2) #>0.1 => reziduurile nu sunt autocorelate la ordin 2

#3. Testarea normalitatii
ols_plot_resid_qq(rs_annualEarnings)
ols_plot_cooksd_bar(rs_annualEarnings)
# Teste: Jarque-Bera si Shapiro Wilk
# H0: distributie normala 
# H1: distributie nenormala
jarque.test(rs_annualEarnings$residuals) #>0.1 => reizuuri normal distribuite
shapiro.test(rs_annualEarnings$residuals) #>0.1 => reizuuri normal distribuite

# Toate ipotezele sunt acceptate => Modelul este bun pentru prognoze

set.seed(111)
training.samples <- annualEarnings$Annual_net_earnings %>%
  createDataPartition(p = 0.5, list = FALSE)
train.data  <- annualEarnings[training.samples, ]
test.data <- annualEarnings[-training.samples, ]

# Model actual
summary(rs_annualEarnings)

# Model pentru setul de antrenare
rs_train <- lm(formula = Annual_net_earnings ~ Inflation_rate, data = train.data)
summary(rs_train)

# Toti coeficientii sunt semnificativi,
# Modelul este valid pe ambele zone, p-value < 0.05
# R^2 asemanatoare
# Sunt doar mici diferente intre valori, in rest modelele sunt asemanatoare


# Evaluare:
#RMSE - Root Mean Squared Error
RMSE(income_pred, test.data$Annual_net_earnings)

#MAE - Mean Absolute Error
MAE(income_pred, test.data$Annual_net_earnings)

#MSE - Mean Squared Error
mse(income_pred, test.data$Annual_net_earnings)

#MAPE - Mean Absolute Percentage Error
MAPE(income_pred, test.data$Annual_net_earnings)

# Realizarea unei prognoze
prognoza <- data.frame(Inflation_rate = c(12))
pred_scenariu <- predict(rs_annualEarnings,newdata = prognoza,interval="confidence")
pred_scenariu # Incasarile anuale atunci cand inflatia este de 12% vor fi in medie de 6698.56 euro

#///////////////////////////////////////////////
#APLICATIA 2******************************

# Regresie multipla -------------------------------------------------
annualEarnings <- read.csv(paste0(directory, "LifeExpectancy.csv"))

# 1) Imbunatatirea modelului
rm_annualEarnings <- lm(annualEarnings$Annual_net_earnings ~  annualEarnings$Inflation_rate + annualEarnings$Unemployment_Rate)



# 2) Interpretare
summary(rm_annualEarnings)

#Bonitatea
# Afisarea lui R-squared
summary(rm_annualEarnings)$r.squared *100 #modelul explica in proportie de 55% variatia var dependente

# Afisarea lui R-squared ajustat
summary(rm_annualEarnings)$adj.r.squared * 100

# Verificarea semnificativității parametrilor
cat("\nSemnificația parametrilor:\n")
cat("P-valoarea pentru intercept: ", coef(summary(rm_annualEarnings))[1, 4], "\n")
cat("P-valoarea pentru panta: ", coef(summary(rm_annualEarnings))[2, 4], "\n")
cat("P-valoare model: ", pf(summary(rm_annualEarnings)$fstatistic[1], 
                            summary(rm_annualEarnings)$fstatistic[2], 
                            summary(rm_annualEarnings)$fstatistic[3], 
                            lower.tail = FALSE), "\n")
#p-value < 0.05 => modelul este valid din punct de vedere statistic la 99%



# 4) Imbunatatirea modelului
rm_annualEarnings2 <- lm(annualEarnings$Annual_net_earnings ~  annualEarnings$Inflation_rate + annualEarnings$Unemployment_Rate + annualEarnings$Tari_vest)
summary(rm_annualEarnings2)

#Bonitatea
# Afisarea lui R-squared
summary(rm_annualEarnings2)$r.squared *100 #modelul explica in proportie de 81% variatia var dependente

# Afisarea lui R-squared ajustat
summary(rm_annualEarnings2)$adj.r.squared * 100

# Verificarea semnificativității parametrilor
cat("\nSemnificația parametrilor:\n")
cat("P-valoarea pentru intercept: ", coef(summary(rm_annualEarnings2))[1, 4], "\n")
cat("P-valoarea pentru panta: ", coef(summary(rm_annualEarnings2))[2, 4], "\n")
cat("P-valoare model: ", pf(summary(rm_annualEarnings2)$fstatistic[1], 
                            summary(rm_annualEarnings2)$fstatistic[2], 
                            summary(rm_annualEarnings2)$fstatistic[3], 
                            lower.tail = FALSE), "\n")
#p-value < 0.05 => modelul este valid din punct de vedere statistic la 99%



# 3) Testarea Ipotezelor

# IPOTEZA 1
# Este modelul liniar in parametri?
# Da, deoarece poate fi scris ca o functie liniara => ipoteza 1 acceptata
# Annual_Net_Earnings = 21574.9 - 825.5 * Inflation_rate - 1096.6 * Unemployment_Rate + 9981.1 * Tari_vest + epsilon

# IPOTEZA 2
# Nr observatii > nr variabile independente
nobs(rm_annualEarnings2) > (rm_annualEarnings2$rank-1) # ipoteza 2 acceptata

# IPOTEZA 3
# Modelul de regresie este corect specificat => ipoteza 3 acceptata

# IPOTEZA 4
# Verificarea variabilitatii
var(annualEarnings$Inflation_rate) # este pozitiva => ipoteza 4 acceptata
var(annualEarnings$Unemployment_Rate) # este pozitiva => ipoteza 4 acceptata
var(annualEarnings$Tari_vest) # este pozitiva => ipoteza 4 acceptata

# IPOTEZA 5
# Verificarea mediei reziduurilor
mean(rm_annualEarnings2$residuals) #medie aproape de 0 => ipoteza 5 acceptata

# IPOTEZA 6
# Verificare multicoliniaritate 
vif(rm_annualEarnings2) #nicio valoare nu este > 10 => nu avem multicoliniaritate
#=> ipoteza 6 acceptata

# IPOTEZA 7
# Verificare corelatie intre reziduuri si variabile independente
cor.test(annualEarnings$Inflation_rate, rm_annualEarnings2$residuals)
cor.test(annualEarnings$Unemployment_Rate, rm_annualEarnings2$residuals)
cor.test(annualEarnings$Tari_vest, rm_annualEarnings2$residuals)
# nivel de corelatie foarte mic si p-value > 0.1 => nu sunt corelate => ipoteza 7 acceptata

# IPOTEZA 8
#1. Testarea heteroschedasticitatii
ggplot(data = annualEarnings, mapping = aes(x = Inflation_rate, y = rm_annualEarnings2$residuals)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Inflation_rate')

ggplot(data = annualEarnings, mapping = aes(x = rm_annualEarnings2$fitted.values, y = rm_annualEarnings2$residuals)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Inflation_rate')
#din plot pare sa avem heteroschedasticitate

# Teste: Breusch-Pagan si White
# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)
bptest(rm_annualEarnings2) #>0.1 => la 99% reziduurile sunt homoschedastice
white_test(rm_annualEarnings2)#>0.1 => la 99% reziduurile sunt homoschedastice


# IPOTEZA 9
#2. Testarea autocorelarii
acf(rm_annualEarnings2$residuals) # nu pare sa avem reziduuri autocorelate

# Teste: Durbin-Watson si Breusch-Godfrey
# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate
dwtest(rm_annualEarnings2) #>0.1 => reziduurile nu sunt autocorelate la ordin 1
bgtest(rm_annualEarnings2) #>0.1 => reziduurile nu sunt autocorelate la ordin 1
bgtest(rm_annualEarnings2,order=2) #>0.1 => reziduurile nu sunt autocorelate la ordin 2

# IPOTEZA 10
#3. Testarea normalitatii
ols_plot_resid_qq(rm_annualEarnings2)
ols_plot_cooksd_bar(rm_annualEarnings2)
# Teste: Jarque-Bera si Shapiro Wilk
# H0: distributie normala 
# H1: distributie nenormala
jarque.test(rm_annualEarnings2$residuals) #<0.05 => reizuurile nu sunt normal distribuite
shapiro.test(rm_annualEarnings2$residuals) #<0.05 => reizuurile nu sunt normal distribuite

annualEarnings2 <- annualEarnings[-c(12,13,18,22),]
rm_annualEarnings3  <- lm(Annual_net_earnings ~ Inflation_rate + Unemployment_Rate + Tari_vest, data = annualEarnings2)
ols_plot_resid_qq(rm_annualEarnings3)
ols_plot_cooksd_bar(rm_annualEarnings3)

jarque.test(rm_annualEarnings3$residuals) #>0.05 => reizuurile sunt normal distribuite
shapiro.test(rm_annualEarnings3$residuals) #>0.05 => reizuurile sunt normal distribuite

# Toate ipotezele sunt acceptate => Modelul este bun pentru prognoze



# 5) Prognoze

set.seed(111)
training.samples <- annualEarnings2$Annual_net_earnings %>%
  createDataPartition(p = 0.5, list = FALSE)
train.data  <- annualEarnings2[training.samples, ]
test.data <- annualEarnings2[-training.samples, ]

# Model actual
rm_annualEarnings3  <- lm(Annual_net_earnings ~ Inflation_rate + Unemployment_Rate + Tari_vest, data = annualEarnings2)
summary(rm_annualEarnings3)
vif(rm_annualEarnings3)

# Model pentru setul de antrenare
rm_train <- lm(Annual_net_earnings ~ Inflation_rate + Unemployment_Rate + Tari_vest, data = train.data)
summary(rm_train)

# Toti coeficientii sunt semnificativi,
# Modelul este valid pe ambele zone, p-value < 0.05
# R^2 asemanatoare
# Sunt doar mici diferente intre valori, in rest modelele sunt asemanatoare

# Predictia modelului pe setul de testare
income_pred_2 <- predict(rm_train,newdata = test.data)
income_pred_2 #predictii estimate de model

# Evaluare:
#RMSE - Root Mean Squared Error
RMSE(income_pred_2, test.data$Annual_net_earnings)

#MAE - Mean Absolute Error
MAE(income_pred_2, test.data$Annual_net_earnings)

#MSE - Mean Squared Error
mse(income_pred_2, test.data$Annual_net_earnings)

#MAPE - Mean Absolute Percentage Error
MAPE(income_pred_2, test.data$Annual_net_earnings)

# Realizarea unei prognoze
prognoza2 <- data.frame(Inflation_rate = c(11),
                        Unemployment_Rate = c(3),
                        Tari_vest = c(0))
pred_scenariu_2 <- predict(rm_annualEarnings3,newdata = prognoza2,interval="confidence")
pred_scenariu_2 # Pentru o tara Est-Europeana, atunci cand inflatia este de 12% si rata somajului este de 5%,incasarile anuale vor fi in medie de 5938.625 euro


