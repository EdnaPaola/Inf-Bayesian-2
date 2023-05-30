# Cargar la librería necesaria
library(stats)

# Definir los parámetros
L <- 0.001
K <- 1000
s <- 30
tiempos <- seq(0, 10, length.out = 26)

# Crear una función para generar la distribución normal con media 0 y varianza s^2
generar_error <- function(n, s) {
  rnorm(n, mean = 0, sd = s)
}

# Crear una función para simular los datos
simular_datos <- function(tiempos, L, K, s) {
  n <- length(tiempos)
  X0 <- 100  
  
  # Inicializar el vector para almacenar los datos simulados
  y <- numeric(n)
  
  # Inicializar el primer valor de y
  y[1] <- 0  
  
  # Simular los datos para cada tiempo
  for (i in 2:n) {
    t <- tiempos[i]
    error <- generar_error(1, s)
    denominador <- K + X0 * (exp(L * K * t) - 1)
    y[i] <- (K * X0 * exp(L * K * t)) / denominador + error
  }
  
  return(y)
}

# Simular los datos
set.seed(11)  # Establecer una semilla para reproducibilidad
datos_simulados <- simular_datos(tiempos, L, K, s)

# Imprimir los datos simulados
print(datos_simulados)
#graficar los datos simulados
plot(datos_simulados)
