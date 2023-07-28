# Carga las librerias necesarias 
library(ggplot2)
library(wordcloud2)
library(shiny)
library(shinydashboard)
###################################################Frases###################################################  
# ... (Código para leer el texto y procesar las frases que has proporcionado)
#Mirar cual es el folder de trabajo actual y asegurarse que en el est? el documento de la obra
getwd()

# Leer el texto desde el archivo "El Arte De La Guerra Sun Tzu.txt" y colocarlo en un vector
text.v <- scan("El Arte De La Guerra Sun Tzu.txt", what="character", sep="\n")

# Guarda la linea del texto en donde inicia la obra en start.v
start.v <- which(text.v == "/CAPITULO I.")
start.v

# Guarda la linea del texto en donde finaliza la obra en end.v
end.v <- which(text.v == "FIN")
end.v

# Guardar el metadato del inicio de la obra, es decir, las líneas antes del inicio del primer capítulo
start.metadata.v <- text.v[1:start.v -1]
start.metadata.v

# Guardar el metadato del final de la obra, es decir, las líneas después del final del último capítulo
end.metadata.v <- text.v[(end.v+1):length(text.v)]
end.metadata.v

# Colocamos todo el metadato de la obra en la variable (metadata.v)
metadata.v <- c(start.metadata.v, end.metadata.v)
metadata.v

# Ahora, las lineas de la novela son las que se encuentra entre la linea de inicio (start.v) y la de fin de la obra (end.v)
# Colocaremos estas lineas en la variable (obra.lines.v)
obra.lines.v <-  text.v[start.v:end.v]

# Combine todas las líneas de la novela en una sola línea sin separador
obra.v <- paste(obra.lines.v, collapse = "")

# Convertir el texto de la obra a minúsculas
obra.v <- tolower(obra.v)

# Agregue saltos de línea después de cada punto en el texto de la novela
obra.v <- gsub("\\.", ".\n", obra.v)

# Divida el texto de la novela en frases usando saltos de línea como separador
phrases <- strsplit(obra.v, "\n")[[1]]

# Obtener la última posición (longitud) de obra.lines.v
last.position.v<-length(obra.lines.v)
last.position.v

# Encontrar las posiciones donde aparece el patrón "CAPITULO \\d" (donde \\d representa un dígito) en obra.lines.v
cap.posicion.v <- grep("CAPITULO", obra.lines.v)

# Función para dividir la obra por capítulo
dividir_por_capitulo <- function(texto, posicion_capitulos) {
  capitulos <- list()
  for (i in 1:(length(posicion_capitulos)-1)) {
    capitulos[[i]] <- texto[posicion_capitulos[i]:posicion_capitulos[i+1]]
  }
  capitulos[[length(posicion_capitulos)]] <- texto[posicion_capitulos[length(posicion_capitulos)]:length(texto)]
  return(capitulos)
}

# Dividir la obra por capítulo
capitulos <- dividir_por_capitulo(text.v, cap.posicion.v)

# Función para contar la frecuencia de una palabra en un texto
contar_palabra <- function(texto, palabra) {
  frecuencia <- length(grep(palabra, texto, ignore.case = TRUE))
  return(frecuencia)
}

# Crear un data frame con la frecuencia de las palabras "arte" y "guerra" en cada capítulo
  frecuencia_por_capitulo <- data.frame(
  Capitulo = 1:length(capitulos),
  Frecuencia_Arte = sapply(capitulos, function(cap) contar_palabra(cap, "arte")),
  Frecuencia_Guerra = sapply(capitulos, function(cap) contar_palabra(cap, "guerra"))
)
  
######################################################################################################################## 
###################################################Tabla de capitulos################################################### 
  # Crear un data frame con los datos impresos
  datos_impresos <- data.frame(
    Capitulo = 1:length(cap.posicion.v),
    Posicion = cap.posicion.v,
    Linea = obra.lines.v[cap.posicion.v]
  )
  
  # Imprimir la tabla
  print(datos_impresos)
  


######################################################################################################################### 
###################################################Diagrama de barras####################################################  
# Lista de palabras clave
palabras_claves <- c("estrategia",
                     "guerra",
                     "conquista",
                     "conocer",
                     "táctica",
                     "liderazgo",
                     "planificación",
                     "adaptación",
                     "dominio",
                     "inteligencia",
                     "engaño",
                     "movimiento",
                     "control",
                     "observación",
                     "ocupación",
                     "flanqueo",
                     "ataque",
                     "defensa",
                     "disposición",
                     "confrontación",
                     "sabiduría")

# Función para contar el número de frases que contienen una palabra clave
contar_frases_palabra <- function(palabra) {
  sum(grepl(paste0("\\b", palabra, "\\b"), phrases, ignore.case = TRUE))
}

# Contar cuántas palabras contienen cada frase
num_palabras <- sapply(palabras_claves, contar_frases_palabra)

# Crear un data frame para el gráfico
datos_grafico <- data.frame(
  Palabra = palabras_claves,
  Numero = num_palabras
)

######################################################################################################################### 
###################################################Diagrama de barras####################################################  
# Lista de palabras clave
palabras_claves <- c("estrategia", "guerra", "conquista", "conocer", "táctica", "liderazgo", "planificación", "adaptación", "dominio", "inteligencia", "engaño", "movimiento", "control", "observación", "ocupación", "flanqueo", "ataque", "defensa", "disposición", "confrontación", "sabiduría")

# Función para contar el número de frases que contienen una palabra clave
contar_frases_palabra <- function(palabra) {
  sum(grepl(paste0("\\b", palabra, "\\b"), phrases, ignore.case = TRUE))
}

# Contar cuántas palabras contienen cada frase
num_palabras <- sapply(palabras_claves, contar_frases_palabra)

# Crear un data frame para el gráfico
datos_grafico <- data.frame(
  Palabra = palabras_claves,
  Numero = num_palabras
)

######################################################################################################################### 
####################################################Nube de palabras##################################################### 
# ... (Código para crear una nube de palabras con los verbos más frecuentes que has proporcionado)

# Unir todas las líneas del texto en una sola cadena
texto_completo <- paste(obra.v, collapse = " ")

# Función para filtrar palabras y quedarnos solo con los verbos
filtrar_verbos <- function(texto) {
  # Tokenizar el texto en palabras
  palabras <- strsplit(texto, "\\s+")[[1]]
  # Utilizar un diccionario de verbos en español (puedes ampliarlo según tus necesidades)
  diccionario_verbos <- c("abandonar", "adaptar", "aprovechar", "aprovecharse", "arrasar", "asaltar", "asediar", "atacar", "atormentar", "atropellar", "atrincherar", "avanzar", "alejar", "aliarse", "apoyar", "asignar", "capturar", "castigar", "ceder", "citar", "combatir", "comunicar", "concebir", "concentrar", "conducir", "confundir", "contradecir", "contrarrestar", "contratar", "controlar", "conocer", "conocerte", "cortejar", "cubrir", "decidir", "defender", "desarrollar", "desmoralizar", "desplegar", "despojar", "destruir", "desviar", "debilitar", "dominar", "emboscar", "emplear", "enfrentar", "entrenar", "esconder", "esperar", "escalar", "esquivar", "establecer", "estrangular", "evitar", "exiliar", "explotar", "extender", "extinguir", "fingir", "flanquear", "fomentar", "forzar", "fortalecer", "ganar", "herir", "impedir", "incorporar", "infiltrar", "inhibir", "inspirar", "interceptar", "interrogar", "investigar", "luchar", "mantener", "mover", "negar", "negociar", "observar", "ocupar", "perder", "perseguir", "planificar", "preparar", "prevenir", "provocar", "recuperar", "reclutar", "recompensar", "rendir", "resistir", "retirar", "retirarse", "reprimir", "sacrificar", "superar", "sorprender", "tender", "tormentar", "transitar")
  
  # Filtrar las palabras que sean verbos
  filtrar_verbos <- palabras[palabras %in% diccionario_verbos]
  return(paste(filtrar_verbos, collapse = " "))
}

# Filtrar solo los verbos del texto
texto_verbos <- filtrar_verbos(texto_completo)

##########################################Dashboard######################################################### 
# Crea el dashboard con las visualizaciones
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard de El Arte De La Guerra"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Frases que contienen 'conocer'", tabName = "frases", icon = icon("book")),
      menuItem("Tabla de Capítulos", tabName = "capitulos", icon = icon("list-ol")),
      menuItem("Diagrama de Barras", tabName = "barras", icon = icon("bar-chart")),
      menuItem("Nube de Verbos", tabName = "nube", icon = icon("cloud"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "frases",
              fluidRow(
                verbatimTextOutput("frases_output")
              )
      ),
       tabItem(tabName = "capitulos",
              dataTableOutput("capitulos_table")
      ),
      
      tabItem(tabName = "barras",
              fluidRow(
                column(width = 6,
                       plotOutput("plot1", height = 250)
                ),
                column(width = 6,
                       plotOutput("plot2", height = 250)
                )
              )
      ),
      
      tabItem(tabName = "nube",
              wordcloud2Output("nube_palabras")
      ),
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)


server <- function(input, output) {
  # Frases que contienen "conocer"
  output$frases_output <- renderPrint({
    frases_contienen_conocer <- phrases[grepl("\\bconocer\\b", phrases, ignore.case = TRUE)]
    frases_contienen_conocer
  })
  
  # Tabla de capítulos
  output$capitulos_table <- renderDataTable({
    datos_impresos
  })
  
  # Diagrama de barras
  output$plot1 <- renderPlot({
    ggplot(frecuencia_por_capitulo, aes(x = Capitulo, y = Frecuencia_Arte, fill = "Arte")) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      geom_bar(aes(x = Capitulo, y = Frecuencia_Guerra, fill = "Guerra"), stat = "identity", position = "dodge", color = "black") +
      labs(x = "Capítulo", y = "Frecuencia", title = "Frecuencia de 'arte' y 'guerra' por capítulo") +
      scale_fill_manual(values = c("Arte" = "blue", "Guerra" = "red")) +
      theme_minimal()
  })
  output$plot2 <- renderPlot({
    ggplot(datos_grafico, aes(x = Palabra, y = Numero, fill = Palabra)) +
      geom_bar(stat = "identity", color = "black") +
      labs(x = NULL, y = "Número de frases", title = "Frases que contienen palabras clave") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Nube de palabras
  output$nube_palabras <- renderWordcloud2({
    wordcloud2(data = data.frame(word = names(table(strsplit(texto_verbos, "\\s+"))),
                                 freq = as.numeric(table(strsplit(texto_verbos, "\\s+")))),
               color = "random-light",
               backgroundColor = "black",
               size = 1.5,
               minRotation = -pi/4,
               maxRotation = -pi/4)
  })
}

shinyApp(ui, server)

