# ...

# Leer el texto desde el archivo "El Arte De La Guerra Sun Tzu.txt" y colocarlo en un vector
text.v <- scan("El Arte De La Guerra Sun Tzu.txt", what = "character", sep = "\n")

# ...

# Dividir el texto de la novela en frases usando saltos de línea como separador
phrases <- strsplit(novel.v, "\n")[[1]]

# Función para extraer el número del capítulo al que pertenece una frase
getChapterNumber <- function(phrase) {
  # Buscar el patrón "/CAPITULO" seguido de un número romano en la frase
  match <- regexpr("/CAPITULO [IVXLCDM]+", phrase, ignore.case = TRUE)
  if (match > 0) {
    # Extraer el número romano y convertirlo a un número arábigo
    roman_numeral <- regmatches(phrase, match)
    arabic_numeral <- as.roman(roman_numeral)
    return(arabic_numeral)
  } else {
    return(NA) # Devolver NA si no se encuentra el patrón del capítulo
  }
}

# Imprimir cada frase capturada entre los saltos de línea junto con el número del capítulo
for (phrase in phrases) {
  chapter_number <- getChapterNumber(phrase)
  cat("Capítulo ", ifelse(!is.na(chapter_number), chapter_number, "N/A"), ":", phrase, "\n\n")
}

# Imprimir cada frase capturada entre los saltos de línea que contenga la palabra "conocer" junto con el número del capítulo
cat("Frases que contienen la palabra 'conocer':\n")
for (phrase in phrases) {
  if (grepl("\\bconocer\\b", phrase, ignore.case = TRUE)) {
    chapter_number <- getChapterNumber(phrase)
    cat("Capítulo ", ifelse(!is.na(chapter_number), chapter_number, "N/A"), ":", phrase, "\n\n")
  }
}
