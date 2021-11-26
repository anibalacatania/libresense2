#' Sensory Board
#'
#' @param answers_dir A character path to the folder from which to load user responses.
#' @param dest_url An optional character including the URL to use as destination host and port.
#'   For example: 192.168.100.7:4001 .
#' @param panel_url An optional character including the panel's URL to show it as a QR code.
#'   For example: http://192.168.100.7:4000 .
#' @param numeric_range A numeric vector indicating the range for numeric inputs.
#'
#' @importFrom agricolae bar.group LSD.test
#' @importFrom dplyr `%>%` arrange as_tibble bind_rows filter group_by group_map if_else mutate n
#' @importFrom dplyr pull select slice_max summarise_if tibble
#' @importFrom DT datatable dataTableOutput renderDataTable
#' @importFrom FactoMineR PCA plot.PCA
#' @importFrom ggplot2 aes coord_cartesian element_text facet_wrap geom_boxplot geom_text geom_tile
#' @importFrom ggplot2 ggplot scale_color_gradient scale_size_area scale_y_continuous theme theme_bw
#' @importFrom ggplot2 theme_minimal theme_void xlab ylab
#' @importFrom ggradar ggradar
#' @importFrom ggwordcloud geom_text_wordcloud
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette
#' @importFrom patchwork wrap_plots
#' @importFrom purrr map negate
#' @importFrom qrcode qrcode_gen
#' @importFrom readr cols read_csv
#' @importFrom SensoMineR decat
#' @importFrom shiny br em h1 h4 h5 h6 hr img
#' @importFrom shiny checkboxInput column conditionalPanel fluidPage fluidRow mainPanel
#' @importFrom shiny observeEvent plotOutput reactiveTimer reactiveVal renderPlot req selectInput
#' @importFrom shiny shinyApp sidebarLayout sidebarPanel tabPanel tabsetPanel updateSelectInput
#' @importFrom stats heatmap median setNames
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @export
#'
run_board <- function(
                      answers_dir = "Answers", dest_url = NULL, panel_url = NULL,
                      numeric_range = c(0, 5)) {
  # Set default host/port, if not provided as `dest_url`.
  host <- getOption("shiny.host", "127.0.0.1")
  port <- getOption("shiny.port")
  if (!is.null(dest_url)) {
    dest_url <- strsplit(dest_url, ":")[[1]]
    if (length(dest_url) != 2) {
      stop("`dest_url` should follow the format HOST:PORT , for example, 192.168.100.7:4001 .")
    }
    host <- dest_url[[1]]
    port <- as.numeric(dest_url[[2]])
  }

  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        img(
          src = "https://inta.gob.ar/sites/default/files/inta_solo_0.jpg", height = 100, width = 100
        ),
        h6("LibreSense R package"),
        em(h6("https://github.com/ anibalacatania/LibreSense")),
        h6("catania.anibal@inta.gob.ar"),
        h5("INTA EEA Mendoza"),
        if (!is.null(panel_url)) plotOutput("qrcode"),
        width = 2
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Datos",
            h1("Datos"),
            br(),
            DT::dataTableOutput("data")
          ),
          tabPanel(
            "ACP",
            h1("ACP"),
            br(),
            plotOutput("answer_acp")
          ),
          tabPanel(
            "Cajas",
            h1("Cajas"),
            br(),
            selectInput("attribute_selector", "Atributo", NULL),
            plotOutput("answer_box")
          ),
          tabPanel(
            "Anova",
            h1("Anova"),
            br(),
            selectInput("attribute_selector_2", "Atributo", NULL),
            plotOutput("answer_anova")
          ),
          tabPanel(
            "Radar",
            h1("Radar"),
            br(),
            plotOutput("answers_radar")
          )
        )
      )
    )
  )

  # Load panelists answers.
  get_answers <- function() {
    names <- dir(glue("{answers_dir}/"))
    req(length(names) > 0)
    answers <- map(names, function(user) {
      map(
        dir(glue("{answers_dir}/{user}")),
        ~ tibble(
          Producto = sub("\\.csv$", "", .x),
          Valuador = user,
          read_csv(glue("{answers_dir}/{user}/{.x}"), col_types = cols())
        )
      )
    }) %>%
      bind_rows()
    req(nrow(answers) > 0)
    arrange(answers, Producto, Valuador)
  }

  # Get column types from a data frame.
  col_types <- function(dataset) sapply(dataset, class)

  # Render table as datatable, with default options.
  render_dtable <- function(table) {
    DT::datatable(
      table,
      options = list(paging = FALSE, searching = FALSE, info = FALSE), rownames = FALSE
    )
  }

  # Generates the QR code plot from the `text`.
  qr_gen <- function(text) {
    if (!require("qrcode") | is.null(text)) {
      return(invisible())
    }
    qr_matrix <- qrcode_gen(text, dataOutput = TRUE, plotQRcode = FALSE)
    qr_matrix <- as.data.frame.table(qr_matrix)
    qr_matrix[1:2] <- lapply(qr_matrix[1:2], as.numeric)
    qr_matrix <- qr_matrix[qr_matrix$Freq == 1, ]
    ggplot(qr_matrix, aes(Var1, Var2)) +
      geom_tile() +
      theme_void() +
      theme(aspect.ratio = 1)
  }

  server <- function(input, output, session) {
    answers <- reactiveVal()

    # Refresh answers every 5 seconds.
    timer <- reactiveTimer(1000 * 5)
    observeEvent(timer(), answers(get_answers()))

    # Show QR code.
    output$qrcode <- renderPlot(qr_gen(panel_url))

    # Show panel table.
    output$data <- DT::renderDataTable(render_dtable(answers()))

    # Create answers radar plot.
    output$answer_acp <- renderPlot({
      ans <- answers()
      req(nrow(ans) > 0 && sum(col_types(ans) == "numeric") > 0)
      ans <- select(ans, Producto, Valuador, where(is.numeric)) %>%
        as.data.frame()
      decat <- decat(ans, formul = "~Producto+Valuador", firstvar = 3, graph = FALSE)
      pca <- PCA(decat$adjmean, scale.unit = FALSE)
      wrap_plots(
        plot.PCA(pca, choix = "ind", axes = 1:2),
        plot.PCA(pca, choix = "var", axes = 1:2, shadowtext = TRUE, new.plot = TRUE),
        nrow = 1,
        widths = c(1, 1)
      )
    })

    # Update answers selector.
    observeEvent(answers(), {
      ans <- answers()
      req(nrow(ans) > 0)
      choices <- colnames(ans)[col_types(ans) == "numeric"]
      updateSelectInput(
        session, "attribute_selector",
        choices = choices,
        selected = if_else(
          nchar(input$attribute_selector) > 0, input$attribute_selector, choices[[1]]
        )
      )
      updateSelectInput(
        session, "attribute_selector_2",
        choices = choices, selected = input$attribute_selector
      )
    })

    # Update attribute selectors on change.
    observeEvent(
      input$attribute_selector,
      updateSelectInput(session, "attribute_selector_2", selected = input$attribute_selector)
    )
    observeEvent(
      input$attribute_selector_2,
      updateSelectInput(session, "attribute_selector", selected = input$attribute_selector_2)
    )

    # Selected answer boxplot.
    output$answer_box <- renderPlot({
      ans <- answers()
      req(nrow(ans) > 0 && sum(col_types(ans) == "numeric") > 0)
      ans$Valor <- pull(ans, input$attribute_selector)
      ans %>%
        ggplot(aes(x = Producto, y = Valor, label = Valuador)) +
        geom_boxplot() +
        geom_text(aes(color = Valuador)) +
        theme_bw() +
        xlab("") +
        ylab("") +
        coord_cartesian(ylim = numeric_range) +
        scale_y_continuous(breaks = seq(numeric_range[[1]], numeric_range[[2]])) +
        theme(legend.position = "none")
    })

    # Selected answer anova.
    output$answer_anova <- renderPlot({
      ans <- answers()
      req(nrow(ans) > 0 && sum(col_types(ans) == "numeric") > 0)
      aov <- aov(pull(ans, input$attribute_selector) ~ Producto, data = ans)
      lsd <- LSD.test(aov, "Producto", p.adj = "bonferroni")
      bar.group(lsd$groups, ylim = numeric_range, density = 4, border = "black", cex.names = 0.7)
    })

    # Create answers radar plot.
    output$answers_radar <- renderPlot({
      ans <- answers()
      req(nrow(ans) > 0 && sum(col_types(ans) == "numeric") > 0)
      select(ans, -Valuador) %>%
        group_by(Producto) %>%
        summarise_if(is.numeric, median) %>%
        ggradar(
          grid.min = numeric_range[[1]], grid.max = numeric_range[[2]], values.radar = c("", "", "")
        )
    })
  }

  # Run the app.
  shinyApp(ui, server, options = list(host = host, port = port))
}
