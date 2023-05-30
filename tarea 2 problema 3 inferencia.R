# Instalar y cargar la librería rstan si no está instalada
if (!require(rstan)) {
  install.packages("rstan")
}
library(rstan)

# Definir el modelo en el lenguaje Stan
stan_code <- "
data {
  int<lower=0> N;  // Número de observaciones
  vector[N] y;  // Datos observados
  vector[N] tiempos;  // Vector de tiempos
  real<lower=0> x0;  // Parámetro x0
  real<lower=0> s;  // Desviacion estandar
}

parameters {
  real<lower=0> theta1;  // Parámetro theta1
  real<lower=0> theta2;  // Parámetro theta2
}

model {
  // Prior para theta1
  theta1 ~ gamma(1, 1/0.001);

  // Prior para theta2
  theta2 ~ gamma(1, .001);

  // Verosimilitud
  for (n in 1:N) {
    real denominator = theta2 + x0 * (exp(theta1 * theta2 * tiempos[n]) - 1);
    y[n] ~ normal((theta2 * x0*exp(theta1 * theta2 * tiempos[n])) / denominator, s);
  }
}
"

# Crear los datos para Stan
stan_data <- list(
  N = length(datos_simulados),
  y = datos_simulados,
  tiempos = tiempos,
  x0 = 100,
  s=30
)

# Compilar el modelo de Stan
stan_model <- stan_model(model_code = stan_code)

# Realizar el ajuste de los datos con modificaciones en el número de iteraciones y el período de burn-in
stan_fit <- sampling(stan_model, data = stan_data, iter = 10000, warmup = 5000)


# Extraer las muestras de los parámetros
theta1_samples <- extract(stan_fit, pars = "theta1")$theta1
theta2_samples <- extract(stan_fit, pars = "theta2")$theta2

# Mostrar la distribución posterior de theta1
plot(density(theta1_samples), main = "Distribución posterior de Theta1")
abline(v = .001, col = "red", lwd = 2)
# Mostrar la distribución posterior de theta2
plot(density(theta2_samples), main = "Distribución posterior de Theta2")
abline(v = 1000, col = "red", lwd = 2)
summary(theta2_samples)
summary(theta1_samples)
