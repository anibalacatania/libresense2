#' Sensory Board Panel
#'
#' @param products_file A character path to the csv file containing the products to evaluate.
#' @param attributes_file A character path to the csv file containing the attributes to evaluate.
#' @param design_file A character path to the csv file containing the experimental design.
#' @param answers_dir A character path to the folder in which to save user responses (if it does not
#'   exist, it will create it).
#' @param dest_url An optional character including the URL to use as destination host and port.
#'   For example: 192.168.100.7:4000 .
#' @param numeric_range A numeric vector indicating the range for numeric inputs.
#'
#' @importFrom dplyr `%>%` bind_rows mutate_at tibble vars
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom readr cols read_csv write_csv
#' @importFrom shiny actionButton fluidPage getQueryString modalDialog reactiveVal
#' @importFrom shiny reactiveValuesToList removeModal renderUI selectInput selectizeInput shinyApp
#' @importFrom shiny showModal showNotification sliderInput textInput
#' @importFrom shiny uiOutput updateQueryString updateSelectInput updateSelectizeInput
#' @importFrom shiny updateSliderInput updateTextInput
#' @importFrom shinyjs disable extendShinyjs js useShinyjs
#' @importFrom shinythemes shinytheme
#' @importFrom stats setNames
#' @importFrom tidyselect everything
#'
#' @export
#'
run_panel <- function(
                      products_file, attributes_file, design_file = NULL, answers_dir = "Answers",
                      dest_url = NULL, numeric_range = c(0, 10)) {

  ### Global variables.

  # Set default host/port, if not provided as `dest_url`.
  host <- getOption("shiny.host", "127.0.0.1")
  port <- getOption("shiny.port")
  if (!is.null(dest_url)) {
    dest_url <- strsplit(dest_url, ":")[[1]]
    if (length(dest_url) != 2) {
      stop("`dest_url` should follow the format HOST:PORT , for example, 192.168.100.7:4000 .")
    }
    host <- dest_url[[1]]
    port <- as.numeric(dest_url[[2]])
  }

  # Load configuration files.
  products <- read_csv(products_file, col_types = cols())
  attributes <- read_csv(attributes_file, col_types = cols())
  design <- tibble(as.data.frame(t(seq_len(nrow(products)))))
  if (!is.null(design_file)) {
    design <- read_csv(design_file, col_types = cols())
  }
  colnames(design) <- paste0("N ", seq_along(design))
  design <- mutate(design, Valuador = NA)
  design <- reactiveVal(design)
  # Create answers directory.
  dir.create(answers_dir, showWarnings = FALSE, recursive = TRUE)
  # Logged panelists.
  panel <- reactiveVal()


  ### UI.

  ui <- fluidPage(
    # Set a dark theme.
    theme = shinytheme("cyborg"),
    # Use shinyjs, to add the function `shinyjs.scrolltop` - scrolls to the top of the window).
    useShinyjs(),
    extendShinyjs(
      text = "shinyjs.scrolltop = function() {window.scrollTo(0, 0)};",
      functions = "scrolltop"
    ),
    # Selector of the product to evaluate.
    selectInput("product", "", choices = NULL),
    # UI for the different attributes inputs.
    uiOutput("attributes"),
    actionButton("submit", "Enviar"),
    align = "center"
  )


  ### Server.

  # Set server side functionality.
  server <- function(input, output, session) {
    ### User variables.
    username <- reactiveVal("") # Logged user.
    product <- reactiveVal("") # Current product.

    # Prepare products selector.
    updateSelectInput(session, "product", label = colnames(products)[[1]], choices = products[, 1])
    disable("product")

    # Prepare attributes inputs.
    output$attributes <- renderUI({
      map(seq_len(nrow(attributes)), function(i) {
        create_ui(attributes[i, ], numeric_range)
      })
    })

    # Try to get the username and product from the query string.
    observeEvent(getQueryString()$user, username(getQueryString()$user))
    observeEvent(getQueryString()$product, product(getQueryString()$product))
    # Ask to get the username.
    username_modal(session)
    observeEvent(input$submitName, {
      if (nchar(input$username) == 0) {
        showNotification("Ingrese su nombre.", type = "error")
        username_modal(session)
        return()
      }
      if (input$username %in% panel()) {
        showNotification(
          "El nombre ya había sido seleccionado, asegúrese que no esté repetido.",
          type = "warning"
        )
      }
      username(input$username)
      dir.create(glue("{answers_dir}/{input$username}"), showWarnings = FALSE, recursive = TRUE)
      removeModal()
    })
    # If logged in, update values.
    observeEvent(username(), {
      req(nchar(username()) > 0)
      # Assign and get current user's design.
      design(assign_design(design(), username(), products, answers_dir))
      # Set selector order according to current user's design.
      curr_design <- filter(design(), Valuador == username()) %>%
        select(-Valuador) %>%
        as.numeric()
      updateSelectInput(session, "product", choices = products[curr_design, 1, drop = TRUE])
      # Add the user to the panel.
      panel(unique(c(panel(), username())))
      # Add username to the query string, so if they update, it will remember it.
      updateQueryString(glue("?user={username()}"), mode = "replace")
      removeModal()
    })
    # If product changed, update the selector.
    observeEvent(product(), {
      req(nchar(product()) > 0)
      updateQueryString(glue("?user={username()}&product={product()}"), mode = "replace")
      updateSelectInput(session, "product", selected = product())
    })

    # Submit a result.
    observeEvent(input$submit, {
      # Get the attributes inputs.
      reactiveValuesToList(input)[make.names(as.character(attributes$Nombre))] %>%
        setNames(as.character(attributes$Nombre)) %>%
        # If they are text, paste them with commas.
        map(~ ifelse(!is.numeric(.x), paste(.x, collapse = ", "), .x)) %>%
        bind_rows() %>%
        write_csv(glue("{answers_dir}/{username()}/{input$product}.csv"))
      # Update select input to the next product.
      curr_design <- filter(design(), Valuador == username()) %>%
        select(-Valuador) %>%
        as.numeric()
      curr_design <- products[curr_design, 1, drop = TRUE]
      act_prod <- which(curr_design == input$product)
      if (act_prod < length(curr_design)) {
        # Reset default values.
        map(seq_len(nrow(attributes)), function(i) {
          reset_ui(attributes[i, ], numeric_range, session)
        })
        product(curr_design[[act_prod + 1]])
      }
      js$scrolltop() # Scroll to top.
      showNotification("Valuación guardada", type = "message")
    })
  }

  # Run the app.
  shinyApp(ui, server, options = list(host = host, port = port))
}


# Shows the modal in which to enter the "username".
username_modal <- function(session) {
  showModal(
    modalDialog(
      textInput("username", "Tu nombre (es tu identificador)"),
      actionButton("submitName", "Enviar"),
      align = "center",
      title = "Bienvenida/o",
      footer = NULL,
      size = "s"
    ),
    session
  )
}

# Creates the UI for each attribute.
create_ui <- function(attribute, numeric_range) {
  type <- trimws(strsplit(attribute$Valores, ":|,")[[1]])
  switch(
    type[[1]],
    Numeric = sliderInput(
      make.names(as.character(attribute$Nombre)),
      label = as.character(attribute$Nombre),
      min = numeric_range[[1]], max = numeric_range[[2]], value = numeric_range[[1]], step = .5
    ),
    Text = selectizeInput(
      make.names(as.character(attribute$Nombre)),
      label = as.character(attribute$Nombre),
      choices = NULL, multiple = TRUE, options = list(create = TRUE)
    )
  )
}

# Resets the UI for each attribute.
reset_ui <- function(attribute, numeric_range, session) {
  type <- trimws(strsplit(attribute$Valores, ":|,")[[1]])
  switch(
    type[[1]],
    Numeric = updateSliderInput(
      session, make.names(as.character(attribute$Nombre)), value = numeric_range[[1]]
    ),
    Text = updateSelectizeInput(
      session, make.names(as.character(attribute$Nombre)), selected = ""
    )
  )
}

# Assigns a slot in the design to the username.
assign_design <- function(design, username, products, answers_dir) {
  if (all(!is.na(design$Valuador))) {
    # Add empty slots by repeating the design.
    design <- bind_rows(design, mutate(design, Valuador = NA))
  }
  if (!username %in% design$Valuador) {
    design$Valuador[is.na(design$Valuador)][[1]] <- username
  }
  # Save the assigned design in a file.
  filter(design, !is.na(Valuador)) %>%
    mutate_at(vars(-Valuador), function(x) products[x, 1, drop = TRUE]) %>%
    select(Valuador, everything()) %>%
    write_csv(glue("{answers_dir}/diseno.csv"))
  design
}
