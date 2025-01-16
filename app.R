#library(shiny)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(treemap)
library(fmsb)
library(DT)


# Limpieza de datos
data <- read.csv("GamingStudy_data.csv")
# En el dataset hay algunos valores que superan el valor 100, incluso algunos valores imposibles.
# Vamos a descartar todos estos casos extremos
data <- subset(data, Hours <= 100)

# Asignamos "other" a categorias registradas de forma poco consistente en su forma de ser egistradas, que aparecen muy poco en la variable "earnings"
data$earnings <- ifelse(!(data$earnings %in% c("I play for fun", "I play mostly for fun but earn a little on the side (tournament winnings, streaming, etc)", "I earn a living by playing this game")), "Other/Mix of proposed cathegories", data$earnings)

# Asignamos "other" a categorias registradas de forma poco consistente que aparecen muy poco en la variable "Playstyle"
data$Playstyle <- ifelse(!(data$Playstyle %in% c("Multiplayer - online - with real life friends", "Multiplayer - online - with strangers", "Multiplayer - online - with online acquaintances or teammates", "Singleplayer", "Multiplayer - offline (people in the same room)", "All of the above")), "Other/Mix of proposed cathegories", data$Playstyle)


# Traducimos a español algunos nombres, y homogeneizamos la forma en la que que están escritos 
colnames(data)[colnames(data) == "Narcissism"] <- "Narcisismo"
colnames(data)[colnames(data) == "GAD_T"] <- "Ansiedad"
colnames(data)[colnames(data) == "SWL_T"] <- "Satisfacción con la vida"
colnames(data)[colnames(data) == "SPIN_T"] <- "Inventario de fobia social"
colnames(data)[colnames(data) == "earnings"] <- "Earnings"
# Guardamos el dataset con otro nombre para tener siempre acceso al dataset inicial con el que empieza a desarrollarse la app
initialData <- data

# La primera parte del código consiste en crear la interfaz de usuario
ui <- fluidPage(

  # Sección con las pestañas de la app
  navbarPage(
    "PR 2 - Visualización de datos",
    
    tabPanel("Introducción",
             h4(strong("Descripción general de la app")),
             p(style="text-align: justify;", "Esta es una app en la que se analizará una base de datos sobre salud mental y videojuegos. La base de datos se puede consultar en el siguiente enlace: https://osf.io/vnbxk/"),
             p(style="text-align: justify;","En el dataset se recogen datos tanto sobre habitos de consumo de videojuegos y variables demográficas que nos sirven para describir al perfil del usuario, como variables psicolóigas útiles para comprender la salud mental de los participantes (Ansiedad, fobia social...)."),
             p(p(style="text-align: justify;","El objetivo de esta app es explorar los datos para encontrar relaciones especificas entre los datos que nos ayuden a comprender cual es la relación entre el consumo de videojuegos y la salud mental (y si se pueden asociar negativamente). Así mismo también queremos conocer cuál es la experiencia cualitativa de los usuarios y que diferencias pueden darse entre subgrupos extremos. Al final de la exposición que se hace en esta app")
             )),  
    
    # Definimos la primera pestaña de la app
    tabPanel("Filtrado según variables demográficas", 
      fluidPage(
               
      # Panel con los filtros que se aplicarán
      sidebarPanel(
        
        # Recuadro de texto explicando los filtros
        wellPanel(
          h4(strong("Descripción de los filtros")),
          p(style="text-align: justify;", "Dispones de 3 filtros con los que explorar el dataset. Los primeros 2, son las horas dedicadas a los videojuegos semanalmente, y la edad de los participantes. El tercero será una variable categórica que se escoge libremente, primero debes seleccionar que variable usarás para filtrar, y posteriormente que categorias de esta usarás para filtrar. Los datos se actualizarán teniendo en cuenta la configuración de todos los filtros simultaneamente.")
        ),
        
        
        # Filtrado de datos mediante las horas semanales de juego
        fluidRow(
          column(width = 12,
                 sliderInput(inputId = "hoursFilter",
                             label = "Filtrar por horas semanales de juego:",
                             min = 1,
                             max = 100,
                             value = c(1,100))
          )),
        
        # Grafico de la distribución de las horas semanales de juego justo debajo del
        # filtro para saber en que zonas del coninuo hay más valores al filtrar
        fluidRow(
          column(width = 12,
                 plotOutput(output="plotHours", height = "115px"))
        ),
        
        # Filtrado de datos mediante la edad de los participantes (el filtrado se hace sobre el primer filtrado)
        fluidRow(
        column(width = 12,
               sliderInput(inputId = "ageFilter",
                           label = "Filtrar por edad:",
                           min = 18,
                           max = 63,
                           value = c(18,63))
        )),
        
        # Grafico de la distribución de la edad justo debajo del filtro para saber en que zonas
        # del coninuo hay más valores al filtrar. Cuando se usa el primer filtro, este gráfico
        # cambia como consecuencia
        fluidRow(column(width = 12,
                        plotOutput(output="plotAge", height = "115px"))),
        
        # Desplegable con el que seleccionar una variable categórica con la que filtrar los datos
        fluidRow(
          column(width = 12,
                 selectInput(inputId = "categoricFilter",
                             label = "Filtrar mediante una variable categorica:",
                             choices = c("", "Work", "Residence", "Playstyle", "Game", "Earnings", "Platform", "Gender"))
                 ),
          # Desplegable con las categorías que se quieren usar para filtrar los datos
          # Se aplica sobre el resultado de los 2 anteriores filtros
          column(width = 12,
                selectInput(inputId = "category_filter",
                             label = "Categorías de la variable seleccionada",
                             choices = c(""),
                             multiple = TRUE)
                 
                 )
        )
        
        ),
      

      # Panel principal de la pestaña, donde se verá el resultado gráfico del filtrado
      mainPanel(
        fluidRow(
          column(width = 12,
                 # Recuadro de texto explicando esta segunda pestaña de la app
                 wellPanel(
                   h4(strong("Exploración de las variables demográficas del dataset")),
                   p(style="text-align: justify;", "En esta segunda pestaña de la app podrás explorar como son las variables demográficas de la misma y filtrar los datos para encontrar subsets que puedan parecerte interesantes. En el gráfico princiap (debajo de este texto) se mostrará un treemap de la variable que selecciones en el desplegable que hay justo encima del gráfico. El treemap se hará a partir de los datos filtrados que obtengas al aplicar filtros en el panel que hay a la izquierda. Solo estamos visualizando variables demográficas categóricas por que las 2 numéricas se están usando para filtrar (aún así, dispones de boxplots debajo del slider de cada filtro en el que también puedes ver como es la distribución de estas variables).")
                 ))
        ),
        # Desplegable con el que seleccionar que variable categórica queremos visualizar
        fluidRow(
          column(width = 12,
                 selectInput(inputId = "plot_var",
                             label = "Mostrar TreeMap de la variable:",
                             choices = c("Work", "Residence", "Playstyle", "Game", "Earnings", "Platform", "Gender"))
                 )
        ),
        
        # Treemap de la variable categórica filtrada
        fluidRow(
                 column(width = 12,
                        plotOutput(outputId = "treeMap")
                        )
                 ),
        wellPanel(
          h4(strong("Unas últimas puntualizaciones")),
          p(style="text-align: justify;", "Debes tener en cuenta que las opciones de filtrado que selecciones en esta pestaña, se mantendrán activas en la siguiente pestaña, donde explorarás las variables psicológicas en los datos que hayas filtrado. Explora estos datos hasta familiarizarte con ellos y puedas establecer alguna hipotesis de las relaciones que esperas encontrarte en las variables psicológicas en la siguiente pestaña. Estudiarás las relaciones de las variables 'Ansiedad', 'Narcisismo', 'Fobia social' y 'Satisfacción con la vida'. La siguiente página te servirá para confirmar o desmentir las hipotesis que hayas planteado en esta exploración.")
        )
      ) 

  )
),
    
    # Segunda pestaña de la app, en ella se exploran las variables psicológicas presentes en los datos
    tabPanel("Exploración variables psicológicas",
  
        # Creación de la página
        fluidPage(
          
          # Panel lateral con opciones para seleccionar las variables que queremos visualizar
          sidebarPanel(width = 4,
            
            # Los siguientes 2 "selectInput" se usarán para seleccionar las variables para un Jointplot           
                       
            # Definición de una de las variables que se mostrarán
            selectInput(inputId = "column_1",
                        label = "Variable X",
                        choices = c("Ansiedad", "Satisfacción con la vida", "Inventario de fobia social", "Narcisismo"),
                        selected = "Ansiedad"),
            # Definición de una segunda las variables que se mostrarán
            selectInput(inputId = "column_2",
                        label = "Variable Y",
                        choices = c("Ansiedad", "Satisfacción con la vida", "Inventario de fobia social", "Narcisismo"),
                        selected = "Inventario de fobia social"),        
                       
            # Texto explicando la página y haciendo algunas recomendaciones de como explorar el Jointplot
            wellPanel(
              h4(strong("Desplegables superiores")),
              p(style="text-align: justify;", "Encima de este texto dispones de 2 desplegables donde puedes seleccionar con que variables psicológicas se hará el JointPlot que hay la derecha de este panel (Gráfico superior). Es recomendable hacer una primera exploración de este gráfico sin filtrar mediante ninguna categoría y ajustando los 2 filtros numéricos a todo su rango, de modo que puedas ver cuales son las relaciones generales entre estas variables, y posteriormente vuelvas a explorarlas aplicando filtros de interés en la segunda pestaña."),
              p(style="text-align: justify;", "Notarás que con la configuración inicial de los filtros, la mayoría de valores son bajos y las relaciones entre las variables, son o inexistentes o moderadas. Hay subsets especificos donde las relaciones son más fuertes, y hay más casos en las partes superiores de las distribuciones. Esta pestaña es perfecta para identificar cuando ocurre esto."),
              h4("Sección inferior del panel principal (Radarplot)"),
              p(style="text-align: justify;", "Debajo del Jointplot tienes una sección en la que puedes explorar las respuestas individuales a las preguntas de los cuestionarios con los que se obtienen las variables psicológicas. Dependiendo de la selección hecha en el desplegable que hay debajo del Jointplot, se mostrará tanto un 'Radarplot' de las puntuaciones de cada pregunta, como una tabla con su contenido, lo que permitirá analizar a nivel cualitativo las variables psicológicas recogids en el dataset."),
              h4("Notas importantes."),
              p(style="text-align: justify;", "Cuando hayas seleccionado un subset en la segunda pestaña, en el radarplot podrás ver superpuestos tanto un gráfico para los datos completos, como del subset escogido, de modo que podrás hacer comparaciones directas y comprender como de diferente es el subgrupo escogido."),
              p(style="text-align: justify;", "Si quieres encontrar algún subset con características intersantes, prueba a escoger a aquellas personas que dediquen entre 50 y 100 horas semanales a los videojuegos, entre 28 y 63 años, que en la categoría 'Work' hayan seleccionado 'Unemployed/Jumping jobs'."),
              p(style="text-align: justify;", "Verás que los valores son algo más pronunciados en este caso. Al igual que este subset, podrás encontrar más casos que son distintos a la muestra completa.")

              ),
                       
          ),
          
          # Panel principal en el que se mostrarán los gráficos 
          mainPanel(
            
            # Creación del jointplot con las 2 variables que se seleccionen
            plotOutput(outputId = "jointPlot"),
            
            # Introducción del radarplot
            wellPanel(
              h4(strong("Radarplot")),
              p(style="text-align: justify;", "A continuación, se muestra la sección del panel principal dedicada a explorar las respuestas individuales de los cuestionarios psicológicos, mediante un radarplot. Están disponibles las variables 'Ansiedad', 'Fobia social', y 'Satisfacción con la vida'. La variable 'Narcisimo' no se encuentra en esta sección por que consiste en una sola pregunta."),
            ),
            
            # Debajo del jointplot habrá otro desplegable para decidir de que cuestionario
            # explorar las puntuaciones de cada una de sus preguntas
            fluidRow(column(
              width = 12,
              selectInput(inputId = "radarVal",
                          label = "Radarplot of variable: ",
                          choices = c("Ansiedad", "Satisfacción con la vida", "Fobia social"))
            )),
            
            # Mostramos gráficamente un radar plot con las puntuaciones de todos las preguntas
            # del cuestionario seleccionado, y una tabla con el contenido de las preguntas
            fluidRow(
              column(width=7, plotOutput(outputId = "radarPlot", height = "550px")),
              column(width=5, dataTableOutput(outputId = "radarPlotTable"))
            ),
            
            wellPanel(
              h4(strong("Conclusión.")),
              p(style="text-align: justify;", "Habrás notado que los valores de las variables psicológicas evaluadas no presentan niveles particularmente elevados en la muestra general, y tras explorarlo, probablemente en ningún subconjunto específico. Aunque existen casos asociados a hábitos o preferencias de consumo de videojuegos particulares que reflejan valores intermedios en las variables de salud mental medidas y sus relaciones, no se observa una prevalencia significativa de casos extremos en las variables que indican problemas de salud mental, tales casos son relativamente poco frecuentes. Esto nos lleva a concluir que el consumo de videojuegos, en términos generales, no guarda una relación fuerte con la salud mental. Más bien, son ciertos hábitos o circunstancias específicas de consumo (como dedicar más de 50 horas semanales a jugar), podrían estar vinculados a diferencias sutiles en los indicadores de salud mental."),
              p(style="text-align: justify;", "No obstante, aunque esto sea cierto, una app de estas características es útil para detectar cuando se dan los casos extremos, como es la experiencia de las personas que representan dichos casos, y otorga la posibilidad de proponer soluciones a dichos casos. No obstante, es importante recalcar, que los casos extremos más que darse debido al comportamiento en si, se da debido a lo extremo que resulta, y al impacto en la vida de la persona de otras variables que se dan conjuntamente al habito particular en cuestión.")
              ),
            
            
            
          )
        )
  )

  )
)

# Server, aqui se definirá la lógica de la creación de los gráficos.
# Los inputs definidos en la anterior sección interactuarán con el server
server <- function(input, output, session) {
  
  
  # Boxplot de las horas semanales dedicadas a jugar videojuegos que está debajo del filtro de esta variable
  output$plotHours <- renderPlot({
    
    par(mar = c(0, 0, 0, 0) + 0.1)
    boxplot(initialData$Hours,
            horizontal = TRUE,
            col="lightblue",
            border="black")
  })
  
  # Definición del objeto reactivo con el que se filtrarán los datos en función de los valores escogidos en
  # el primer slider (el que filtra los datos según las horas dedicadas a jugar videojuegos semanalmente)
  filter_data_byHours <- reactive({
    # Filtrado de valores en el rango definido en el slider
    initialData %>%
      filter(data[["Hours"]] >= input$hoursFilter[1], data[["Hours"]] <= input$hoursFilter[2])
  })
  
  # Boxplot de la edad de los participantes que se encuentra justo debajo del segundo filtro (que filtra por la edad)
  output$plotAge <- renderPlot({
    data <- filter_data_byHours()
    
    par(mar = c(0, 0, 0, 0) + 0.1)
    boxplot(data$Age,
            horizontal = TRUE,
            col="lightblue",
            border="black")}
  )
  
  # Implementación del filtrado de los participantes por su edad
  filter_data_byHours_PlusAge <- reactive({
    # Obtenemos los datos reslutantes al aplicar el primer filtro de las horas semanales dedicadas a jugar videojuegos
    data <- filter_data_byHours()
    # Filtrado de valores en el rango definido en el slider
    data %>%
      filter(data[["Age"]] >= input$ageFilter[1], data[["Age"]] <= input$ageFilter[2])
  })
  
  
  # Filtrado por las categorias seleccionadas
  filter_data_byCategory <- reactive({
    # Primero se obtienen los datos tras aplicar los primeros 2 filtros
    data <- filter_data_byHours_PlusAge()
    
    # Filtrar por la categoría seleccionada de la variable seleccionada
    data <- data %>% filter(.data[[input$categoricFilter]] %in% input$category_filter)
    
    data
  })
  
  
  # Definimos una forma de obtener las categorias de la variable categórica que escojamos para filtrar
  observe({
    # Variable seleccionada en el primero de los 2 desplegables para filtrar por la variable categórica
    selected_var <- input$categoricFilter
    
    # Obtenemos las categorías únicas de esa variable
    unique_categories <- unique(initialData[[selected_var]])
    
    # Actualizamos el selectInput que filtra los datos por categoría, para mostrar las categorías
    updateSelectInput(session, "category_filter",
                      choices = unique_categories,
                      selected = unique_categories[1])
  })
  
  
  # Creación del treemap en el panel principal
  output$treeMap <- renderPlot({
    # Si no se ha seleccionado una variable categórica para filtrar, se escogen los
    # datos resultantes de los filtros por horas semanales de juego y edad
    if (input$categoricFilter=="") {
      data <- filter_data_byHours_PlusAge()
    } else {data <- filter_data_byCategory()} # Si se ha seleccionado, se escogen los datos obtenidos con los 3 filtros
    
    
    # Contamos cuantas observaciones quedan tras aplicar el filtro
    num_observaciones <- nrow(data)
    # Hacemos un treemap de la variable indicada en el desplegable del panel principal
    treemap_var <- data %>%
      group_by(.data[[input$plot_var]]) %>%
      summarize(Count = n()) %>%
      na.omit()
    
    
    # Mostramos el treemap. En su titulo indicamos cuantas observaciones lo originan
    treemap(
      treemap_var,
      index = input$plot_var,  # Columna categórica
      vSize = "Count",         # Métrica de tamaño
      title = paste("Treemap de la variable", input$plot_var, "- Observaciones filtradas:", num_observaciones),
      palette = "Blues"
    )
  })
  
  
  
  # Jointplot de la segunda pestaña de la app
  output$jointPlot <- renderPlot({
    # Si no se ha seleccionado una variable categórica para filtrar, se escogen los
    # datos resultantes de los filtros por horas semanales de juego y edad
    if (input$categoricFilter=="") {
      data <- filter_data_byHours_PlusAge()
    } else {
      data <- filter_data_byCategory() # Si se ha seleccionado, se escogen los datos obtenidos con los 3 filtros
    }

    
    # Creamos el dataframe con el que se hará el jointplot
    plot_data <- data.frame(x = data[[input$column_1]], y = data[[input$column_2]]) %>% na.omit()
    
    # Correlación entre las variables seleccionadas
    corr_value <- cor(plot_data$x, plot_data$y)
    
    # Creamos un scatterplot con una linea de tendencia
    p <- ggplot(plot_data, aes(x = x, y = y)) +
      geom_point(alpha = 0.05) +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(
        # Etiquetas
        title = paste("Relación entre", input$column_1, "y", input$column_2),
        x = input$column_1,
        y = input$column_2,
        # Texto que indica el valor de la correlación
        caption = paste("Correlación: ", round(corr_value, 2))
      ) +
      theme_minimal() +
      # Tamaño de las etiquetas
      theme(
        plot.caption = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
      )
    
    # Agregagamos histogramas marginales a los margenes del scatterplot, con lo que conformamos el jointplot
    ggMarginal(p, type = "histogram", fill = "#1e4ed8", color = "black", margins = "both", size = 4)
  })
  
  
  # Creación del radarplot del cuestionario que se escoja en el desplegable debajo del jointplot
  output$radarPlot <- renderPlot({
    # Si no se ha seleccionado una variable categórica para filtrar, se escogen los
    # datos resultantes de los filtros por horas semanales de juego y edad
    if (input$categoricFilter=="") {
      data <- filter_data_byHours_PlusAge()
    } else {
      data <- filter_data_byCategory() # Si se ha seleccionado, se escogen los datos obtenidos con los 3 filtros
    }
    
    # Definición de los datos del radarplot de ansiedad
    if (input$radarVal == "Ansiedad") {
      columnas <- c("GAD1", "GAD2", "GAD3", "GAD4", "GAD5", "GAD6", "GAD7")
      # Medias de los items del cuestionario en todo el dataset y en el filtrado
      radarplot_data <- data.frame(t(colMeans(initialData[,columnas])))
      radarplot_data <- rbind(t(colMeans(data[,columnas])), radarplot_data)
      # Máximo y minimo de la escala (según la fuente de los datos)
      radarplot_data <- rbind(rep(3, 7), rep(0, 7),radarplot_data)
      
      
      
    # Definición de los datos del radarplot de satisfacción con la vida  
    } else if (input$radarVal == "Satisfacción con la vida") {
      columnas <- c("SWL1", "SWL2", "SWL3", "SWL4", "SWL5")
      # Medias de los items del cuestionario en todo el dataset y en el filtrado
      radarplot_data <- data.frame(t(colMeans(initialData[,columnas])))
      radarplot_data <- rbind(t(colMeans(data[,columnas])), radarplot_data)
      # Máximo y minimo de la escala (según la fuente de los datos)
      radarplot_data <- rbind(rep(7, 5), rep(1, 5),radarplot_data)
    
      
    
    # Definición de los datos del radarplot de Fobia social        
    } else if (input$radarVal == "Fobia social") {
      columnas <- c("SPIN1", "SPIN2", "SPIN3", "SPIN4", "SPIN5", "SPIN6", "SPIN7", "SPIN8", "SPIN9", "SPIN10", "SPIN11", "SPIN12", "SPIN13", "SPIN14", "SPIN15", "SPIN16", "SPIN17")
      # Medias de los items del cuestionario en todo el dataset y en el filtrado
      radarplot_data <- data.frame(t(colMeans(initialData[,columnas], na.rm = TRUE)))
      radarplot_data <- rbind(t(colMeans(data[,columnas])), radarplot_data)
      # Máximo y minimo de la escala (según la fuente de los datos)
      radarplot_data <- rbind(rep(4, 17), rep(0, 17),radarplot_data)
    }
    
    
    # Creación del radarplot usando los datos que se hayan escogido
    par(mar = c(2, 2, 2, 2))
    radarchart(radarplot_data, pfcol = c("#0000FF40" ,"#FF000060"), vlcex = 0.8)
    labels <- c("Muestra filtrada", "Población general")
    legend("topright", legend = labels, col = c("#0000FF40" ,"#FF000060"), lty = 1, lwd = 2, bty = "n")
    
  })
  
  
  # Creación de la tabla con el contenido de los items del cuestionario
  output$radarPlotTable <- renderDataTable({
    # Definición de la tabla con los items del cuestionario de ansiedad
    # Obtenidos de: https://osf.io/vyr5f
    if (input$radarVal == "Ansiedad") {
      identificadores <- c("GAD1", "GAD2", "GAD3", "GAD4", "GAD5", "GAD6", "GAD7")
      items <- c(
        "Feeling nervous, anxious, or on edge",
        "Not being able to stop or control worrying",
        "Worrying too much about different things",
        "Trouble relaxing",
        "Being so restless that it's hard to sit still",
        "Becoming easily annoyed or irritable",
        "Feeling afraid as if something awful might happen"
      )
      cuestionario <- data.frame(identificadores, items)
      
      
    # Definición de la tabla con los items del cuestionario de satisfacción con la vida
    # Obtenidos de: https://osf.io/vyr5f  
    } else if (input$radarVal == "Satisfacción con la vida") {
      
      identificadores <- c("SWL1", "SWL2", "SWL3", "SWL4", "SWL5")
      items <- c(
        "In most ways my life is close to my ideal",
        "The conditions of my life are excellent",
        "I am satisfied with life",
        "So far I have gotten the important things I want in life",
        "If I could live my life over, I would change almost nothing"
      )
      cuestionario <- data.frame(identificadores, items)
    
      
    # Definición de la tabla con los items del cuestionario de satisfacción con la vida
    # Obtenidos de: https://www.cambridge.org/core/journals/the-british-journal-of-psychiatry/article/psychometric-properties-of-the-social-phobia-inventory-spin/9E4A3EE20D2B1A6C222CDB5807AC086A 
    } else if (input$radarVal == "Fobia social") {
      
      identificadores <- c("SPIN1", "SPIN2", "SPIN3", "SPIN4", "SPIN5", "SPIN6", "SPIN7", "SPIN8", "SPIN9", "SPIN10", "SPIN11", "SPIN12", "SPIN13", "SPIN14", "SPIN15", "SPIN16", "SPIN17")
      items <- c(
        "Fear of people in authority",
        "Bothered by blushing",
        "Fear of parties and social events",
        "Avoids talking to strangers",
        "Fear of criticism",
        "Avoids embarrassment",
        "Distressed by sweating",
        "Avoids parties",
        "Avoids being the centre of attention",
        "Fear of talking to strangers",
        "Avoids speeches",
        "Avoids criticism",
        "Distressed by palpitations",
        "Fear of others watching",
        "Fear of embarrassment",
        "Avoids talking to authority",
        "Distressed by trembling or shaking"
      )
      cuestionario <- data.frame(identificadores, items)
      
    }
    datatable(cuestionario)
  })
  
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
