# Funciones útiles
media_muestral = function(sample){
  return(sum(sample) / length(sample))
}

# Ejercicio 1 

# Sea Xi - U(0, b)

b_momentos = function(sample) {
  return(2 * media_muestral(sample));
}

b_maxima_verosimilitud = function(sample) {
  return(max(sample));
}

# Ejercicio 2
b_mediana = function(sample){
  return(2*median(sample));
}

# Ejercicio 3
b = 1
sample = runif(15, min=0, max=b)

print(b_momentos(sample))
print(b_maxima_verosimilitud(sample))
print(b_mediana(sample))

# Ejercicio 4
# a)
sample = runif(15, min=0, max=b)

# b)
estimador_momentos = b_momentos(sample)
estimador_maxima_verosimilitud = b_maxima_verosimilitud(sample)
estimador_mediana = b_mediana(sample)

print(b_momentos(sample))
print(b_maxima_verosimilitud(sample))
print(b_mediana(sample))

# c)
cantidad_de_repeticiones = 1000 

variable_aleatoria_con_b_momentos = c()
variable_aleatoria_con_b_maxima_verosimilitud = c()
variable_aleatoria_con_b_mediana = c()

for(i in 1:cantidad_de_repeticiones){
  sample = runif(15, min=0, max=1)
  variable_aleatoria_con_b_momentos = c(variable_aleatoria_con_b_momentos, b_momentos(sample))
  variable_aleatoria_con_b_maxima_verosimilitud = c(variable_aleatoria_con_b_maxima_verosimilitud, b_maxima_verosimilitud(sample))
  variable_aleatoria_con_b_mediana = c(variable_aleatoria_con_b_mediana, b_mediana(sample))
}

# d)
media_muestral_b_momentos = media_muestral(variable_aleatoria_con_b_momentos)
media_muestral_b_maxima_verosimilitud = media_muestral(variable_aleatoria_con_b_maxima_verosimilitud)
media_muestral_b_mediana = media_muestral(variable_aleatoria_con_b_mediana)

error_b_momentos = b - media_muestral_b_momentos
error_b_maxima_verosimilitud = b - media_muestral_b_maxima_verosimilitud
error_b_mediana = b - media_muestral_b_mediana

print(error_b_momentos)
print(error_b_maxima_verosimilitud)
print(error_b_mediana)