#poner los descripores
variables<-c("importancia","certeza")

preguntastotales<-read.csv("preguntas.csv")

#preguntastotales<-data.frame(id=c(1,2,3,4,5,6,7),
#                      texto=c("Pregunta 1","Pregunta 2","Pregunta 3","Pregunta 4","Pregunta 5","Pregunta 6","Pregunta 7"))

pasos<-data.frame(
                  idbloque=c(1,2,3,4,5),
                  start=c(1,13,19,21,31),
                  stop=c(12,18,20,30,40),
                  nombrebloque=c("Mercado Nacional","Adaptaciones del producto","Comunicacion","EnoTurismo","Politico-Institucional"),
                  color=c("pink","skyblue","aquamarine","BurlyWood" ,"CadetBlue")
                  )
#poner el path donde se van a guardar las copas enviadas

ultimopaso<-nrow(pasos)
P<-"./responses"


##############################################################


library(shiny)
library(shinyjs)
  
slider<-function(x){sliderInput(x, x,
                                  0, 10, 0.00, 0.01,ticks = FALSE)}
  
preguntasoutput<-function(x){
          #print(x)
          #lapply(variables,slider)
          h4((x[2]), 
              selectInput(paste("certeza_",x[1],sep=""), "certeza",
                         choices=seq(1,10),selected = 5),
           
               selectInput(paste("importancia_",x[1],sep="") , "importancia",
                          choices=seq(1,10),selected = 5),
             
             sliderInput(x, x,
                         0, 10, 0.00, 0.01,ticks = FALSE)
             
                  )
    }
######################


outputDir <-P

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}


###########################################3


# Define the fields we want to save from the form

fieldsMandatory <- c("nombre" )            
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

#appCSS <-".mandatory_star { color: red; }"    

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
 
   includeCSS("styles.css"),
    titlePanel("Prospectiva"),
    div(
      id = "form",
      textInput("nombre", "Nombre", ""),
      shinyjs::useShinyjs()
    ),
   mainPanel (
   htmlOutput("bloque"),   
   uiOutput("ui"),
     
      actionButton("submit", "Enviar"),
      div(id = "form", "" ),
      shinyjs::hidden(
        div(
          id = "thankyou_msg",
          h3("Gracias, sus respuestas han sido enviadas con éxito!")
        )
      )  
   )
   
   ),
  
 ########################################################################################################## 
 
  server = function(input, output, session) {
    paso <-reactiveVal()
    paso(1)
    
    get_preguntas<-reactive({
      
      preguntas<-preguntastotales[which(preguntastotales$id >=pasos[paso(),]$start & preguntastotales$id<=pasos[paso(),]$stop),]
      preguntas
    })
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      fields <- c("nombre", apply(t(variables),1,function(variable) apply(get_preguntas(), 1, function(x) paste(variable,"_",x[1],sep=""))))
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    observe({
      # check if all mandatory fields have a value
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      # enable/disable the submit button
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
 
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
      saveData(formData())
      paso(paso()+1)
      preguntas<-get_preguntas()
      shinyjs::hide("form")
      shinyjs::runjs("scroll(0,0)")
      if (paso() > ultimopaso){
        shinyjs::hide("submit")  
        shinyjs::hide("bloque")
          shinyjs::show("thankyou_msg")
       }      
    })
    
    output$ui<-renderUI({
      HTML(
        "scroll(0,0);"
      )
      preguntas<-get_preguntas()
      apply(preguntas,1,preguntasoutput)
    })
    
   #output$bloque<-renderPrint(h3(pasos[paso(),]$nombrebloque)
                                 
                              
                                # )
   
    #output$bloque<-renderText(paste("<h3><span style=\"background-color:",pasos[paso(),]$color,"\">", pasos[paso(),]$nombrebloque,"</span></h3>"))
  # output$bloque<-renderText(paste("<span style=\"background-color:red\"><H3>", pasos[paso(),]$nombrebloque,"</H3></span>"))
  # output$bloque<-renderText("<h3><span style=\"background-color: #FFFF00\">Yellow text.</span></h3>")                             
  output$bloque<-renderText(paste("<style>
                                      h3 { background-color:",pasos[paso(),]$color,"; }
                            </style>
                            <h3>",pasos[paso(),]$nombrebloque,"</h3>"))
                            
                            
    
    }
)

