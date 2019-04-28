### Funciones utiles: 
album_completo = function(cantidad_de_figuritas) {
  # Devuelve un album lleno.
  return(rep(TRUE, cantidad_de_figuritas))
}

album_vacio = function(cantidad_de_figuritas) {
  # Devuelve un album completo.
  return(rep(FALSE, cantidad_de_figuritas))
}

## Ejercicio 1
album = album_vacio(cantidad_de_figuritas=6)

sample = sample(1:6, 1, replace = TRUE)

album[sample] = TRUE

print(album)

## Ejercicio 2
# mejor nombre para album_lleno seria es_album_lleno. Devuelve un booleano y no una objeto. Se mantiene así para mantener enunciado
album_lleno = function(album){
  return(all(album))
}

mock_album_completo = album_completo(cantidad_de_figuritas=6)
mock_album_incompleto = album_vacio(cantidad_de_figuritas=6)

print(album_lleno(mock_album_completo))
print(album_lleno(mock_album_incompleto))

## Ejercicio 3
# Aclaración: se agregó un parámetro más con valor default True (mantiene la usabilidad definida en el enunciado) para 
# reutilizar código en los siguientes ejercicios. 
generar_sobre = function(tam_album, tam_sobre, con_repetidos = TRUE){
  return(sample(1:tam_album, tam_sobre, replace=con_repetidos))
}

sobre = generar_sobre(tam_album=6, tam_sobre=3)

album_vacio = album_vacio(cantidad_de_figuritas=6)

album_vacio[sobre[1]] = TRUE
album_vacio[sobre[2]] = TRUE
album_vacio[sobre[3]] = TRUE

print(album_vacio)

## Ejercicio 4
pegar_sobre = function(album,  sobre){
  for(figurita in sobre){
    album[figurita] = TRUE
  }
  return(album)
}

album_vacio = album_vacio(cantidad_de_figuritas=10)
sobre_1 = generar_sobre(tam_album=length(album_vacio), tam_sobre=3)
sobre_2 = generar_sobre(tam_album=length(album_vacio), tam_sobre=3)

print(sobre_1)
album = pegar_sobre(album_vacio, sobre_1)
print(album)

print(sobre_2)
album = pegar_sobre(album, sobre_2)
print(album)

## Ejercicio 5
cuantas_figuritas = function(tam_album, tam_sobre, sobre_con_repetidos=TRUE){
  cantidad_de_sobres_usados = 0
  album = rep(FALSE, tam_album)
  while(!album_lleno(album=album)) {
    sobre = generar_sobre(tam_album=tam_album, tam_sobre=tam_sobre, con_repetidos=sobre_con_repetidos)
    album = pegar_sobre(album=album, sobre=sobre)
    cantidad_de_sobres_usados = cantidad_de_sobres_usados + 1
  }
  return(cantidad_de_sobres_usados)
}

## Ejercicio 6

primer_intento = cuantas_figuritas(tam_album=6, tam_sobre=1)
segundo_intento = cuantas_figuritas(tam_album=6, tam_sobre=1)
tercer_intento = cuantas_figuritas(tam_album=6, tam_sobre=1)

print(primer_intento)
print(segundo_intento)
print(tercer_intento)
print((primer_intento == segundo_intento) & (primer_intento == tercer_intento) & (segundo_intento == tercer_intento))

## Ejercicio 7
generar_experimentos = function(cantidad_de_repeticiones, con_repeticion=TRUE){
  resultados_de_experimentos = c()
  for(i in 1:cantidad_de_repeticiones) {
    resultados_de_experimentos = c(resultados_de_experimentos, cuantas_figuritas(tam_album=670, tam_sobre=5, sobre_con_repetidos=con_repeticion))
  }  
  return(resultados_de_experimentos)
}

cantidad_de_repeticiones = 1000
resultados_de_experimentos = generar_experimentos(cantidad_de_repeticiones)

plot(1:cantidad_de_repeticiones, resultados_de_experimentos, main = 'Resultados de experimentos')

# Cuál es la probabilidad de terminar el album con 800 sobres o menos?
casos_exitosos = length(which(resultados_de_experimentos <= 800))
print(casos_exitosos / cantidad_de_repeticiones)

# La cantidad de sobres que hacen falta comprar para completar el album con probabilidad mayor o igual a 0.9.
# P(X < x) >= 0.9
cantidad_sobres_con_probabilidad_mayor = quantile(resultados_de_experimentos, probs = 0.90)
print(cantidad_sobres_con_probabilidad_mayor)

# Cual es el valor esperado de sobres para completar el album?
esperanza = mean(resultados_de_experimentos)
print(esperanza)

# cual es el desvio de la cantidad de sobres para completar el album?
desviacion = sd(resultados_de_experimentos)
print(desviacion)

# Ejercicio 8
cantidad_de_repeticiones = 1000
resultados_de_experimentos = generar_experimentos(cantidad_de_repeticiones, con_repeticion=FALSE)

plot(1:cantidad_de_repeticiones, resultados_de_experimentos, main = 'Resultados de experimentos')

# Cuál es la probabilidad de terminar el album con 800 sobres o menos?
casos_exitosos = length(which(resultados_de_experimentos <= 800))
print(casos_exitosos / cantidad_de_repeticiones)

# La cantidad de sobres que hacen falta comprar para completar el album con probabilidad mayor o igual a 0.9.
# P(X < x) >= 0.9
cantidad_sobres_con_probabilidad_mayor = quantile(resultados_de_experimentos, probs = 0.90)
print(cantidad_sobres_con_probabilidad_mayor)

# Cual es el valor esperado de sobres para completar el album?
esperanza = mean(resultados_de_experimentos)
print(esperanza)

# cual es el desvio de la cantidad de sobres para completar el album?
desviacion = sd(resultados_de_experimentos)
print(desviacion)

