install.packages("rugarch")
library(rugarch) 
library(readxl)

setwd("C:/Users/macie/Desktop/projekt")

getwd()

# dane wczytuję z pliku "data file"
data <- read_excel("C:/Users/macie/Desktop/projekt/data file.xlsx")
View(data)

head(data)
tail(data)

# tworzymy zmienna rate, będącą logarytmiczną stopą zwrotu
rate <- 100*log(data[2:nrow(data),2]/data[1:(nrow(data)-1),2])
colnames(rate) <- "close"
rate <- as.matrix(rate)

# dodajemy sztuczne zmienne zero-jedynkowe korygujące outlayery
dummy <- matrix(0, nrow = nrow(rate), ncol = 5)

colnames(dummy) <- c("Dummy1", "Dummy2", "Dummy3", "Dummy4", "Dummy5")

dummy[200,1] <- 1
dummy[256,2] <- 1
dummy[340,3] <- 1
dummy[380,4] <- 1
dummy[400,5] <- 1

colSums(dummy)

################################################################################################################
# AR(5) - GARCH(1,1) Normalny

# Specyfikacja
specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                                  submodel = NULL,
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(0,0), 
                                              include.mean = TRUE,
                                              archm = FALSE, 
                                              archpow = 1, 
                                              arfima = FALSE, 
                                              external.regressors = dummy,
                                              archex = FALSE),
                            distribution.model = "norm",
                            start.pars = list(),
                            fixed.pars = list())
# Estymacja
ar0.garch11.norm <- ugarchfit(data = rate, 
                              spec = specification,
                              solver = "hybrid")
                            
# Analiza
show(ar0.garch11.norm)
plot(ar0.garch11.norm)

ar0.garch11.norm.vcov <- vcov(ar0.garch11.norm, robust=TRUE)

################################################################################################################
# AR(5) - GARCH(1,1) Studenta

# Specyfikacja
specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                                  submodel = NULL,
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(0,0), 
                                              include.mean = TRUE,
                                              archm = FALSE, 
                                              archpow = 1, 
                                              arfima = FALSE, 
                                              external.regressors = dummy,
                                              archex = FALSE),
                            distribution.model = "std",
                            start.pars = list(),
                            fixed.pars = list())
# Estymacja
ar0.garch11.std <- ugarchfit(data = rate, 
                              spec = specification,
                              solver = "hybrid")

# Analiza
show(ar0.garch11.std)
plot(ar0.garch11.std)

ar0.garch11.std.vcov <- vcov(ar0.garch11.std, robus=TRUE)

################################################################################################################
# AR(5) - GJR - GARCH(1,1) Normalny

# Specyfikacja
specification <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1),
                                                  submodel = NULL,
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(0,0), 
                                              include.mean = TRUE,
                                              archm = FALSE, 
                                              archpow = 1, 
                                              arfima = FALSE, 
                                              external.regressors = dummy,
                                              archex = FALSE),
                            distribution.model = "norm",
                            start.pars = list(),
                            fixed.pars = list())
# Estymacja
ar0.gjrgarch11.norm <- ugarchfit(data = rate, 
                             spec = specification,
                             solver = "hybrid")

# Analiza
show(ar0.gjrgarch11.norm)
plot(ar0.gjrgarch11.norm)

ar0.gjrgarch11.norm.vcov <- vcov(ar0.gjrgarch11.norm, robus=TRUE)

################################################################################################################
# AR(5) - GJR - GARCH(1,1) Student

# Specyfikacja
specification <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1),
                                                  submodel = NULL,
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(0,0), 
                                              include.mean = TRUE,
                                              archm = FALSE, 
                                              archpow = 1, 
                                              arfima = FALSE, 
                                              external.regressors = dummy,
                                              archex = FALSE),
                            distribution.model = "std",
                            start.pars = list(),
                            fixed.pars = list())
# Estymacja
ar0.gjrgarch11.std <- ugarchfit(data = rate, 
                                 spec = specification,
                                 solver = "hybrid")

# Analiza
show(ar0.gjrgarch11.std)
plot(ar0.gjrgarch11.std)

ar0.gjrgarch11.std.vcov <- vcov(ar0.gjrgarch11.std, robus=TRUE)

################################################################################################################
# AR(5) - E - GARCH(1,1) Normalny

# Specyfikacja
specification <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1),
                                                  submodel = NULL,
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(0,0), 
                                              include.mean = TRUE,
                                              archm = FALSE, 
                                              archpow = 1, 
                                              arfima = FALSE, 
                                              external.regressors = dummy,
                                              archex = FALSE),
                            distribution.model = "norm",
                            start.pars = list(),
                            fixed.pars = list())
# Estymacja
ar0.egarch11.norm <- ugarchfit(data = rate, 
                                spec = specification,
                                solver = "hybrid")

# Analiza
show(ar0.egarch11.norm)
plot(ar0.egarch11.norm)

ar0.egarch11.norm.vcov <- vcov(ar0.egarch11.norm, robus=TRUE)

################################################################################################################
# AR(5) - E - GARCH(1,1) Student

# Specyfikacja
specification <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1),
                                                  submodel = NULL,
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(0,0), 
                                              include.mean = TRUE,
                                              archm = FALSE, 
                                              archpow = 1, 
                                              arfima = FALSE, 
                                              external.regressors = dummy,
                                              archex = FALSE),
                            distribution.model = "std",
                            start.pars = list(),
                            fixed.pars = list())
# Estymacja
ar0.egarch11.std <- ugarchfit(data = rate, 
                               spec = specification,
                               solver = "hybrid")

# Analiza
show(ar0.egarch11.std)
plot(ar0.egarch11.std)

ar0.egarch11.std.vcov <- vcov(ar0.egarch11.std, robus=TRUE)

################################################################################################################
# AR(5) - GARCH(1,1) in-Mean Normalny

# Specyfikacja
specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                                  submodel = NULL,
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(0,0), 
                                              include.mean = TRUE,
                                              archm = TRUE, 
                                              archpow = 2, 
                                              arfima = FALSE, 
                                              external.regressors = dummy,
                                              archex = FALSE),
                            distribution.model = "norm",
                            start.pars = list(),
                            fixed.pars = list())
# Estymacja
ar0.garch11.nm.norm <- ugarchfit(data = rate, 
                              spec = specification,
                              solver = "hybrid")

# Analiza
show(ar0.garch11.nm.norm)
plot(ar0.garch11.nm.norm)

ar0.garch11.nm.norm.vcov <- vcov(ar0.garch11.nm.norm, robus=TRUE)

################################################################################################################
# AR(5) - GARCH(1,1) in-Mean Studenta

# Specyfikacja
specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                                  submodel = NULL,
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(0,0), 
                                              include.mean = TRUE,
                                              archm = TRUE, 
                                              archpow = 2, 
                                              arfima = FALSE, 
                                              external.regressors = dummy,
                                              archex = FALSE),
                            distribution.model = "std",
                            start.pars = list(),
                            fixed.pars = list())
# Estymacja
ar0.garch11.nm.std <- ugarchfit(data = rate, 
                             spec = specification,
                             solver = "hybrid")

# Analiza
show(ar0.garch11.nm.std)
plot(ar0.garch11.nm.std)

ar0.garch11.nm.std.vcov <- vcov(ar0.garch11.nm.std, robus=TRUE)

################################################################################################################
# AR(5) - GJR - GARCH(1,1) in-Mean Normalny

# Specyfikacja
specification <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1),
                                                  submodel = NULL,
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(0,0), 
                                              include.mean = TRUE,
                                              archm = TRUE, 
                                              archpow =2, 
                                              arfima = FALSE, 
                                              external.regressors = dummy,
                                              archex = FALSE),
                            distribution.model = "norm",
                            start.pars = list(),
                            fixed.pars = list())
# Estymacja
ar0.gjrgarch11.nm.norm <- ugarchfit(data = rate, 
                                 spec = specification,
                                 solver = "hybrid")

# Analiza
show(ar0.gjrgarch11.nm.norm)
plot(ar0.gjrgarch11.nm.norm)

ar0.gjrgarch11.norm.nm.vcov <- vcov(ar0.gjrgarch11.nm.norm, robus=TRUE)

################################################################################################################
# AR(5) - GJR - GARCH(1,1) in-Mean Student

# Specyfikacja
specification <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1),
                                                  submodel = NULL,
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(0,0), 
                                              include.mean = TRUE,
                                              archm = TRUE, 
                                              archpow = 2, 
                                              arfima = FALSE, 
                                              external.regressors = dummy,
                                              archex = FALSE),
                            distribution.model = "std",
                            start.pars = list(),
                            fixed.pars = list())
# Estymacja
ar0.gjrgarch11.nm.std <- ugarchfit(data = rate, 
                                spec = specification,
                                solver = "hybrid")

# Analiza
show(ar0.gjrgarch11.nm.std)
plot(ar0.gjrgarch11.nm.std)

ar0.gjrgarch11.nm.std.vcov <- vcov(ar0.gjrgarch11.nm.std, robus=TRUE)

################################################################################################################
# AR(5) - E - GARCH(1,1) in-Mean Normalny

# Specyfikacja
specification <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1),
                                                  submodel = NULL,
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(0,0), 
                                              include.mean = TRUE,
                                              archm = TRUE, 
                                              archpow = 2, 
                                              arfima = FALSE, 
                                              external.regressors = dummy,
                                              archex = FALSE),
                            distribution.model = "norm",
                            start.pars = list(),
                            fixed.pars = list())
# Estymacja
ar0.egarch11.nm.norm <- ugarchfit(data = rate, 
                               spec = specification,
                               solver = "hybrid")

# Analiza
show(ar0.egarch11.nm.norm)
plot(ar0.egarch11.nm.norm)

ar0.egarch11.nm.norm.vcov <- vcov(ar0.egarch11.nm.norm, robus=TRUE)

################################################################################################################
# AR(5) - E - GARCH(1,1) in-Mean Student

# Specyfikacja
specification <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1),
                                                  submodel = NULL,
                                                  external.regressors = NULL,
                                                  variance.targeting = FALSE),
                            mean.model = list(armaOrder = c(0,0), 
                                              include.mean = TRUE,
                                              archm = TRUE, 
                                              archpow = 2, 
                                              arfima = FALSE, 
                                              external.regressors = dummy,
                                              archex = FALSE),
                            distribution.model = "std",
                            start.pars = list(),
                            fixed.pars = list())
# Estymacja
ar0.egarch11.nm.std <- ugarchfit(data = rate, 
                              spec = specification,
                              solver = "hybrid")

# Analiza
show(ar0.egarch11.nm.std)
plot(ar0.egarch11.nm.std)

ar0.egarch11.nm.std.vcov <- vcov(ar0.egarch11.nm.std, robus=TRUE)

################################################################################################################
# kryteria informacyjne

kryteria <- cbind(
  infocriteria(ar0.garch11.norm),
  infocriteria(ar0.garch11.nm.std),
  infocriteria(ar0.garch11.nm.norm),
  infocriteria(ar0.garch11.nm.std),
  infocriteria(ar0.gjrgarch11.norm),
  infocriteria(ar0.gjrgarch11.std),
  infocriteria(ar0.gjrgarch11.nm.norm),
  infocriteria(ar0.gjrgarch11.nm.std),
  infocriteria(ar0.egarch11.norm),
  infocriteria(ar0.egarch11.std)
)

colnames(kryteria) <- c("GARCH(1,1) Norm", 
                        "GARCH(1,1) Std", 
                        "GARCH(1,1) iM Norm", 
                        "GARCH(1,1) iM Std",
                        "GJR-GARCH(1,1) Norm",
                        "GJR-GARCH(1,1) Std",
                        "GJR-GARCH(1,1) iM Norm",
                        "GJR-GARCH(1,1) iM Std",
                        "E-GARCH(1,1) Norm",
                        "E-GARCH(1,1) Std"
                        )























