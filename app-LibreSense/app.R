
library(shiny)
library(shinyjs)

#Write the descriptors
a<-c("Int aroma","Cítrico","Miel","Vegetal","Fruta fresca","Fruta cocida","Floral","Acidez",
     "Astringencia","Amargo")
#Write sample names and codes
b<-c("345"="Muestra1","879"="Muestra2","674"="Muestra3")
typeof(b)

#Path for saving the results
P<-"./responses"


  
  slider<-function(x){sliderInput(x, x,
                                  0, 10, 0.00, 0.01,ticks = FALSE)}
  


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
fields <- c("nombre", "copa" , a)
fieldsMandatory <- c("nombre", "copa")            
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-".mandatory_star { color: red; }"    

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    titlePanel("Panel"),
    div(
      id = "form",
      textInput("nombre", "Nombre", ""),
      shinyjs::useShinyjs(),

      
     
      
      
      selectInput(inputId = "copa",
                  label = "copa",
                  choices = c(b,""),
                  selected = "")
      ),
   mainPanel (
     
  
     lapply(a,slider),

      actionButton("submit", "Submit"),
      div(id = "form", "" ),
      shinyjs::hidden(
        div(
          id = "thankyou_msg",
          h3("Gracias, su copa fue enviada con exito!"),
          actionLink("submit_another", "Enviar otra copa")
        )
      )  
     
      
      
      
      
      
      
      
      
    )),
  
 ########################################################################################################## 
  
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData()) 
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
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    })
    
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    
    
    
  }
)

