# Seleccionamos el archivo "hitter.final"
filename <- file.choose()

# Cargamos los datos del fichero de texto como CSV
data <- read.csv(filename,sep="\t", header=FALSE, 
    col.names=c("name", "bat_1986", "hits_1986", "homeruns_1986", 
        "runs_1986", "runs_batted_1986", "walks_1986", "years_major_leagues", 
        "times_at_bat_career", "hits_career", "homeruns_career", "runs_career", 
        "runs_batted_career", "walks_career", "league_end_1986", 
        "division_end_1986", "team_end_1986", "position_1986", 
        "put_outs_1986", "assits_1986", "errors_1986", "annual_salary_1987", 
        "league_beginning_1987", "team_beginning_1987"))

# Seleccionamos las variables que nos interesan: 
# el número de homeruns como variable discreta, 
# y el salario como la variable continua
homeruns <- data$homeruns_1986
salary <- data$annual_salary_1987

#------------------------------------------------------------------------------
# Diagramas de la variable discreta
#------------------------------------------------------------------------------

# Diagrama de barras
aux <- as.data.frame(table(homeruns))
ybar <- aux$Freq
xbar <- as.character(aux$homeruns)
barplot(ybar, names.arg=xbar, space=0.5, col=3, main="Diagrama de barras 
    del número de homeruns durante la temporada 1986", 
    xlab="Número de homeruns", ylab="Frecuencia" )
abline(h=0)

# Diagrama de sectores
pie(ybar, labels=xbar, main="Diagrama de sectores del número de homeruns")

#------------------------------------------------------------------------------
# Diagramas de la variable continua
#------------------------------------------------------------------------------

# Histograma
hist(salary, breaks=,main="Histograma del salario durante la temporada 1986", 
    xlab="Salario (en miles de dólares)", ylab="Frecuencia", col=2)

# Diagrama de tallos y hojas
stem(salary, scale=4)

#------------------------------------------------------------------------------
# Medidas
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Medidas de centralización
#------------------------------------------------------------------------------

# Media

# Eliminamos los NA, si lo hubiera
homeruns_mean <- mean(homeruns, na.rm = TRUE)
salary_mean <- mean(salary, na.rm = TRUE)

# Mediana
homeruns_median <- median(homeruns, na.rm = TRUE)
salary_median <- median(salary, na.rm = TRUE)

# Moda

# Definimos una función para la moda
mode <- function(x) {
  temp <- table(as.vector(x))
  return(names(temp)[temp == max(temp)])
}

homeruns_mode <- mode(homeruns);

#------------------------------------------------------------------------------
# Medidas de posición
#------------------------------------------------------------------------------

homeruns_quantiles <- quantile(homeruns, c(0.25, 0.75, 0.2, 0.9), na.rm = TRUE)
salary_quantiles <- quantile(salary, c(0.25, 0.75, 0.2, 0.9), na.rm = TRUE)

#------------------------------------------------------------------------------
# Medidas de dispersión
#------------------------------------------------------------------------------


# Cuasivarianza
homeruns_qvar <- var(homeruns, na.rm = TRUE)
salary_qvar <- var(salary, na.rm = TRUE)

# Varianza

# Definimos una función para la varianza
variance <- function(x) {
  n <- sum(!is.na(x))
  return ((n-1)*var(x, na.rm = TRUE)/n)
}

homeruns_var <- variance(homeruns)
salary_var <- variance(salary)

# Desviación estándar

homeruns_std = sqrt(var(homeruns, na.rm = TRUE))
salary_std = sqrt(var(salary, na.rm = TRUE))

# Rango
homeruns_range <- max(homeruns, na.rm = TRUE) - min(homeruns, na.rm = TRUE)
salary_range <- max(salary, na.rm = TRUE) - min(salary, na.rm = TRUE)

# Rango intercuartílico

homeruns_iqr <- IQR(homeruns, na.rm = TRUE)
salary_iqr <- IQR(salary, na.rm = TRUE)

#------------------------------------------------------------------------------
# Diagrama de cajas
#------------------------------------------------------------------------------


boxplot(homeruns, range=1.5, main="Diagrama de cajas para el número de homeruns")
boxplot(salary, range=1.5, main="Diagrama de cajas para el salario")

#------------------------------------------------------------------------------
# Coeficientes de asimetría y curtosis
#------------------------------------------------------------------------------

# Cargamos la libería fBasics
library(fBasics)

# Asimetría
homeruns_skeness <- skewness(homeruns, na.rm = TRUE)
salary_skeness <- skewness(salary, na.rm = TRUE)

# Curtosis
homeruns_kurtosis <- kurtosis(homeruns, na.rm = TRUE)
salary_kurtosis <- kurtosis(salary, na.rm = TRUE)
