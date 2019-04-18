
### Definir las siguientes funciones

## Ejercicio 1
album = rep(FALSE, 6) 

sample = sample(1:6, 1, replace = TRUE)

album[sample] = TRUE

print(album)

## Ejercicio 2

album_lleno = function(album){
  return(all(album))
}

mock_album_completo = rep(TRUE, 6)
mock_album_incompleto = rep(FALSE, 6)

print(album_lleno(mock_album_completo))
print(album_lleno(mock_album_incompleto))

## Ejercicio 3

generar_sobre = function(tam_album, tam_sobre){
  return(sample(1:tam_album, tam_sobre, replace=TRUE))
}

sobre = generar_sobre(tam_album=6, tam_sobre=3)

album_vacio = rep(FALSE, 6)

# Linda forma de hacer TDD! Felicitaciones! 
album_vacio[sobre[1]] = TRUE
album_vacio[sobre[2]] = TRUE
album_vacio[sobre[3]] = TRUE


## Ejercicio 4
pegar_sobre = function(album,  sobre){
  for(figurita in sobre){
    album[figurita] = TRUE
  }
  return(album)
}

album_vacio = rep(FALSE, 10)
sobre_1 = generar_sobre(tam_album=length(album_vacio), tam_sobre=3)
sobre_2 = generar_sobre(tam_album=length(album_vacio), tam_sobre=3)

print(sobre_1)
album = pegar_sobre(album_vacio, sobre_1)
print(album)

print(sobre_2)
album = pegar_sobre(album, sobre_2)
print(album)

## Ejercicio 5
cuantas_figuritas = function(tam_album, tam_sobre){
  cantidad_de_sobres_usados = 0
  album = rep(FALSE, tam_album)
  while(!album_lleno(album=album)) {
    sobre = generar_sobre(tam_album=tam_album, tam_sobre=tam_sobre)
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
cantidad_de_repeticiones = 1000
resultados_de_experimentos = c()
for(i in 1:cantidad_de_repeticiones) {
   resultados_de_experimentos = c(resultados_de_experimentos, cuantas_figuritas(tam_album=670, tam_sobre=5))
}

plot(1:cantidad_de_repeticiones, resultados_de_experimentos, main = 'Resultados de experimentos')

# Cuál es la probabilidad de terminar el album con 800 sobres o menos?
casos_exitosos = length(which(resultados_de_experimentos <= 800))
print(casos_exitosos / cantidad_de_repeticiones)

# La cantidad de sobres que hacen falta comprar para completar el album con probabilidad mayor o igual a 0.9.
# P(X < x) >= 0.9

# P(casos_exitosos / cantidad_de_repeticiones) >= 0.9 <=> ????????

