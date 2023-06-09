# Instalar y cargar la librer�a rstan si no est� instalada
if (!require(rstan)) {
  install.packages("rstan")
}
library(rstan)

# Definir el modelo en el lenguaje Stan
stan_code <- "
data {
  int<lower=0> N;  // N�mero de observaciones
  vector[N] y;  // Datos observados
  vector[N] tiempos;  // Vector de tiempos
  real<lower=0> x0;  // Par�metro x0
  real<lower=0> s;  // Desviacion estandar
}

parameters {
  real<lower=0> theta1;  // Par�metro theta1
  real<lower=0> theta2;  // Par�metro theta2
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

# Realizar el ajuste de los datos con modificaciones en el n�mero de iteraciones y el per�odo de burn-in
stan_fit <- sampling(stan_model, data = stan_data, iter = 10000, warmup = 5000)


# Extraer las muestras de los par�metros
theta1_samples <- extract(stan_fit, pars = "theta1")$theta1
theta2_samples <- extract(stan_fit, pars = "theta2")$theta2

# Mostrar la distribuci�n posterior de theta1
plot(density(theta1_samples), main = "Distribuci�n posterior de Theta1")
abline(v = .001, col = "red", lwd = 2)
# Mostrar la distribuci�n posterior de theta2
plot(density(theta2_samples), main = "Distribuci�n posterior de Theta2")
abline(v = 1000, col = "red", lwd = 2)
summary(theta2_samples)
summary(theta1_samples)
