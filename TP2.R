# Ejercicio 1 

# Sea Xi - U(0, b)

b_momentos = function(sample) {
  return(2*sum(sample) / length(sample));
}

b_maxima_verosimilitud = function(sample) {
  return(max(sample));
}

# Ejercicio 2
b_mediana = function(sample){
  return(2*median(sample));
}

# Ejercicio 3
sample = runif(15, min=0, max=1)

print(b_momentos(sample))
print(b_maxima_verosimilitud(sample))
print(b_mediana(sample))