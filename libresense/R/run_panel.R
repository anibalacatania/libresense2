#' Sensory Board Panel
#'
#' @param products_file A character path to the csv file containing the products to evaluate.
#' @param attributes_file A character path to the csv file containing the attributes to evaluate.
#' @param design_file A character path to the csv file containing the experimental design.
#' @param answers_dir A character path to the folder in which to save user responses (if it does not
#'   exist, it will create it).
#' @param product_name One of {"NombreProducto", "CodigoProducto", "CodigoMuestra"},
#'   indicating which product label the panelist will see. "NombreProducto" is the products' names
#'   as appear in `products_file`; "CodigoProducto" is a random code assigned to each different
#'   product; "CodigoMuestra" is a random code assigned to each different product-panelist
#'   combination.
#' @param dest_url An optional character including the URL to use as destination host and port.
#'   For example: 192.168.100.7:4000 .
#' @param numeric_range A numeric vector indicating the range for numeric inputs.
#'
#' @importFrom dplyr `%>%` bind_rows filter filter_at left_join mutate_at pull tibble vars
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom readr cols read_csv write_csv
#' @importFrom shiny actionButton checkboxInput fluidPage getQueryString modalDialog reactiveVal
#' @importFrom shiny reactiveValuesToList removeModal renderUI selectInput selectizeInput shinyApp
#' @importFrom shiny showModal showNotification sliderInput textInput
#' @importFrom shiny uiOutput updateCheckboxInput updateQueryString updateSelectInput
#' @importFrom shiny updateSelectizeInput updateSliderInput updateTextInput
#' @importFrom shinyjs disabled extendShinyjs js useShinyjs
#' @importFrom shinythemes shinytheme
#' @importFrom stats setNames
#' @importFrom tidyselect everything
#'
#' @export
#'
run_panel <- function(
                      products_file, attributes_file, design_file = NULL, answers_dir = "Answers",
                      product_name = "NombreProducto", dest_url = NULL, numeric_range = c(0, 10)) {

  ### Input variables check.

  if (!product_name %in% c("NombreProducto", "CodigoProducto", "CodigoMuestra")) {
    stop('`product_name` must be one of {"NombreProducto", "CodigoProducto", "CodigoMuestra"}')
  }

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
  design <- tibble(Muestra = seq_len(nrow(products)))
  if (!is.null(design_file)) {
    design <- read_csv(design_file, col_types = cols())
  }
  # Create design columns if don't exist.
  fixed_panelists <- unique(design$Valuador)
  if (is.null(fixed_panelists)) {
    design <- mutate(design, Valuador = "")
  }
  if (!"NombreProducto" %in% colnames(design)) {
    design <- mutate(design, NombreProducto = products[Muestra, 1, drop = TRUE])
  }
  if (!"CodigoProducto" %in% colnames(design)) {
    samples_code <- tibble(
      Muestra = unique(design$Muestra),
      CodigoProducto = paste0("P", sample(1000, nrow(products)) - 1)
    )
    design <- left_join(design, samples_code, by = "Muestra")
  }
  if (!"CodigoMuestra" %in% colnames(design)) {
    design <- mutate(design, CodigoMuestra = paste0("M", sample(1000, nrow(design)) - 1))
  }
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
    # Disable browser navigation.
    shiny::tags$script(paste(
      "history.pushState(null, document.title, location.href);",
      "window.addEventListener('popstate', function () {",
      "  history.pushState(null, document.title, location.href);",
      "});",
      sep = "\n"
    )),
    # Show username.
    disabled(textInput("panelist_name", "Panelista")),
    # Selector of the product to evaluate.
    disabled(selectInput("product", "", choices = NULL)),
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
    # Control if the panelist has finished.
    finished <- reactiveVal(FALSE)

    # Prepare products selector.
    updateSelectInput(session, "product", label = colnames(products)[[1]])

    # Prepare attributes inputs.
    output$attributes <- renderUI({
      map(seq_len(nrow(attributes)), function(i) {
        create_ui(attributes[i, ], numeric_range)
      })
    })

    # Disable everything if evaluation has finished.
    observeEvent(finished(), {
      req(finished())
      showModal(modalDialog(
        title = "Muchas gracias, ha finalizado la evaluación.", size = "l", footer = NULL
      ))
    })

    # Try to get the username and product from the query string.
    observeEvent(getQueryString()$user, username(getQueryString()$user))
    observeEvent(getQueryString()$product, product(getQueryString()$product))
    observeEvent(getQueryString(), finished("finished" %in% names(getQueryString())))
    observeEvent(username(), updateTextInput(session, "panelist_name", value = username()))
    # Ask to get the username.
    username_modal(session, fixed_panelists)
    observeEvent(input$submitName, {
      if (nchar(input$username) == 0) {
        showNotification("Ingrese su nombre.", type = "error")
        username_modal(session, fixed_panelists)
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
      curr_design <- filter(design(), Valuador == username())
      updateSelectInput(session, "product", choices = curr_design[[product_name]])
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
      curr_design <- filter(design(), Valuador == username())
      prod_name <- filter_at(curr_design, product_name, ~ .x == input$product)$NombreProducto
      reactiveValuesToList(input)[make.names(as.character(attributes$Nombre))] %>%
        setNames(as.character(attributes$Nombre)) %>%
        # If they are text, paste them with commas.
        map(~ ifelse(!is.numeric(.x), paste(.x, collapse = ", "), .x)) %>%
        bind_rows() %>%
        write_csv(glue("{answers_dir}/{username()}/{prod_name}.csv"))
      # Update select input to the next product.
      curr_prod_order <- curr_design[[product_name]]
      act_prod <- which(curr_prod_order == input$product)
      if (act_prod < nrow(curr_design)) {
        # Reset default values.
        map(seq_len(nrow(attributes)), function(i) {
          reset_ui(attributes[i, ], numeric_range, session)
        })
        product(curr_prod_order[[act_prod + 1]])
      } else {
        finished(TRUE)
        updateQueryString(glue("?finished"), mode = "replace")
      }
      js$scrolltop() # Scroll to top.
      showNotification("Valuación guardada", type = "message")
    })
  }

  # Run the app.
  shinyApp(ui, server, options = list(host = host, port = port))
}


# Shows the modal in which to enter the "username".
username_modal <- function(session, fixed_panelists) {
  if (is.null(fixed_panelists)) {
    username_input <- textInput("username", "Tu nombre (es tu identificador)")
  } else {
    username_input <- selectInput(
      "username", "Tu nombre (es tu identificador)",
      choices = fixed_panelists
    )
  }
  showModal(
    modalDialog(
      username_input,
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
      min = numeric_range[[1]], max = numeric_range[[2]], value = mean(numeric_range), step = .5
    ),
    Check = checkboxInput(
      make.names(as.character(attribute$Nombre)),
      label = as.character(attribute$Nombre)
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
      session, make.names(as.character(attribute$Nombre)),
      value = mean(numeric_range)
    ),
    Check = updateCheckboxInput(session, make.names(as.character(attribute$Nombre)), value = FALSE),
    Text = updateSelectizeInput(
      session, make.names(as.character(attribute$Nombre)),
      selected = ""
    )
  )
}

# Assigns a slot in the design to the username.
assign_design <- function(design, username, products, answers_dir) {
  if (all(design$Valuador != "")) {
    # Add empty slots by repeating the design.
    design <- bind_rows(design, mutate(design, Valuador = "", CodigoMuestra = ""))
    # Randomly get new sample codes.
    design$CodigoMuestra[design$CodigoMuestra == ""] <- paste0("M", setdiff(
      sample(1000, 1000) - 1, as.numeric(gsub("M", "", design$CodigoMuestra))
    )[seq_len(sum(design$CodigoMuestra == ""))])
  }
  if (!username %in% design$Valuador) {
    design$Valuador[design$Valuador == ""][seq_len(nrow(products))] <- username
  }
  # Save the assigned design in a file.
  filter(design, Valuador != "") %>%
    select(Valuador, everything()) %>%
    write_csv(glue("{answers_dir}/diseno.csv"))
  design
}
