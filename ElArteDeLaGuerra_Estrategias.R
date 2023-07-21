# Instalar y cargar paquetes necesarios (si aún no están instalados)
# install.packages("tm")
# install.packages("stringr")
library(tm)
library(stringr)

#Mirar cual es el folder de trabajo actual y asegurarse que en el est? el documento de la obra
getwd()

#Lee el texto desde el archivo "El Arte De La Guerra Sun Tzu.txt" y lo coloca en un vector
text.v <- scan("El Arte De La Guerra Sun Tzu.txt", what="character", sep="\n")

# Guarda lal?nea del texto en donde inicia la obra en start.v
start.v <- which(text.v == "/CAPITULO I")
start.v

# Guarda la linea del texto en donde finaliza la obra en start.v
end.v <- which(text.v == "FIN")
end.v

start.metadata.v <- text.v[1:start.v -1]
start.metadata.v

# El metadato del final de la obra inicia en la l?nea (end.v+1) y finaliza en la ?ltima de la obra
end.metadata.v <- text.v[(end.v+1):length(text.v)]
end.metadata.v

# Colocamos todo el metadato de la obra en la variable (metadata.v)
metadata.v <- c(start.metadata.v, end.metadata.v)
metadata.v

# Ahora, las lineas de la novela son las que se encuentra entre la l?nea de inicio (start.v) y la de fin de la obra (end.v)
# Colocaremos est?s l?neas en la variable (novel.lines.v)
novel.lines.v <-  text.v[start.v:end.v]
novel.lines.v

class(novel.lines.v)

texto_unido <- novel.lines.v

texto_unido <- gsub("[[:punct:]]", " ", texto_unido)

# Convertir todas las palabras a minúsculas
texto_unido <- tolower(texto_unido)

# Eliminar puntuación y caracteres especiales
texto_unido <- gsub("[[:punct:]]", " ", texto_unido)

# Crear un Corpus de texto
corpus <- Corpus(VectorSource(texto_unido))

# Preprocesamiento del texto
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))  # Puedes cambiar "spanish" al idioma adecuado
corpus <- tm_map(corpus, stripWhitespace)

# Convertir el Corpus a una matriz de términos de documentos (DTM)
dtm <- DocumentTermMatrix(corpus)

# Buscar frases que mencionen estrategias militares
expresion_regular_estrategias <- "([^.!?]*estrategia[^.!?]*)"
frases_estrategias <- str_extract_all(texto_unido, expresion_regular_estrategias)

# Filtrar las frases que mencionen estrategias militares
estrategias_militares <- unlist(frases_estrategias)

# Eliminar frases duplicadas (opcional)
estrategias_militares <- unique(estrategias_militares)

# Imprimir la lista de estrategias militares
cat("Estrategias Militares de Sun Tzu: \n")
cat(estrategias_militares, sep = "\n" )
