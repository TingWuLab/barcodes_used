require(shiny)
require(shinyjs)
require(dplyr)
require(readr)
require(tibble)
library(rclipboard)

os_street_barcodes <- data.frame(
  Name = c("3K - hg38 mm10 dm6 wuhCor1 loxafr3 ce11",
           "dm6",
           "dmel",
           "galGal4",
           "hg38",
           "hg38 mm10 dm6 galGal4 loxAfr3O imyakon",
           "loxAfr3",
           "mm10",
           "Oimyakon"),
  link = c("http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Streets_hg38mm10dm6wuhCor1loxafr3ce11_100120.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Streets_dm6.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Streets_dmel_scaffold2.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Streets_galGal4.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Streets_hg38.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Streets_hg38mm10dm6galGal4loxAfr3Oimyakon.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Streets_loxAfr3.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Streets_mm10.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Streets_Oimyakon.txt")
)

os_toes_barcodes <- data.frame(
  Name = c("3K - hg38 mm10 dm6 wuhCor1 loxafr3 ce11", 
           "dm6",
           "dmel",
           "galGal4",
           "hg38",
           "hg38 mm10 dm6 galGal4 loxAfr3O imyakon",
           "loxAfr3",
           "mm10",
           "Oimyakon"),
  link = c("http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Toes_hg38mm10dm6wuhCor1loxafr3ce11_100120.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Toes_dm6.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Toes_dmel_scaffold2.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Toes_galGal4.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Toes_hg38.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Toes_hg38mm10dm6galGal4loxAfr3Oimyakon.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Toes_loxAfr3.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Toes_mm10.txt",
           "http://hmsrsc-wulab-data.s3.amazonaws.com/barcodes/Toes_Oimyakon.txt")
)

ui <- fluidPage(
  useShinyjs(),  
  rclipboardSetup(),
  
  h1("Used barcodes"),
  helpText(".Upload your query file with a single column of sequences. Accepted formats: .txt, .csv, .tsv."),
  helpText(".Easily compare your barcodes against the selected list: copy a 'match' vector (1 for match, 0 for no match) or copy indexes of matched barcodes directly to your clipboard."),
  br(),
  
  fluidRow(
    column(2,
           radioButtons(inputId = 's_t', 
                        "Toeholds?",
                        choices = c("With toe" = "with_toe", "Toeless" = "no_toes"))
    ),
    column(6,
           selectInput(inputId = "barcodes",
                       label = "Select barcodes",
                       choices = os_street_barcodes$Name)
    ),
    column(4,
           fileInput("query_list", "Upload query sequences")
    )
  ),
  fluidRow(
    column(12,
           style = "background-color: #A8D5E2;",
           br(),
    column(4,
           tags$h4(style = "font-weight: bold; background-color: #A8D5E2;", 
                   textOutput("out_txt"))
           ),
           column(4, uiOutput("copy_idx_btn")),#actionButton("copy_idx_btn", icon = icon("copy"), "Copy match _index_ vector to clipboard")), # initially hidden in the server logic
           column(4, uiOutput("copy_btn")),#actionButton("copy_btn", icon = icon("copy"), "Copy match _0/1_ vector to clipboard")), # initially hidden in the server logic
           br(),
           br()),
           br(),
           dataTableOutput("out_df")
    
  )
)

server <- function(input, output, session) {
  
  # observe({
  #   shinyjs::toggle("copy_btn", condition = !is.null(input$query_list))
  #   shinyjs::toggle("copy_idx_btn", condition = !is.null(input$query_list))
  # })
  
  streets_query <- reactive({
    if(input$s_t == "with_toe"){
      df <- read_csv(os_toes_barcodes[os_toes_barcodes$Name == {input$barcodes}, 'link'], col_names = "toes")
      return(df$toes)
    } else if (input$s_t == "no_toes"){
      df <- read_csv(os_street_barcodes[os_street_barcodes$Name == {input$barcodes}, 'link'], col_names = "no_toes")
      return(df$no_toes)
    }
  }) %>% 
    bindEvent(input$barcodes, input$s_t)
  
  uploaded_sequences <- reactive({
    req(input$query_list)
    # Load the sequences from the input file
    df <- read.table(input$query_list$datapath, header = FALSE)
    query_seqs <- df[, 1]
    
    # Get barcodes using your streets_query function
    barcodes <- streets_query()
    
    # Initialize the output data frame
    out_df <- tibble(
      index = NA_character_,
      barcodes = barcodes,
      query = NA_character_,
      match = NA_real_
    )
    
    # Match each barcode with the query sequence
    for (i in seq_along(barcodes)) {
      matched_idx <- which(barcodes[i] == query_seqs)
      if (length(matched_idx) > 0) {
        # Get the first match for simplicity
        out_df$query[i] <- query_seqs[matched_idx[1]]
        out_df$match[i] <- 1
      } else {
        out_df$match[i] <- 0
      }
    }
    
    out_df$index <- 1:nrow(out_df)
    
    print(head(out_df, 50))
    return(out_df)
  })
  
  output$out_df <- renderDataTable({
    uploaded_sequences()
  })
  
  match_n <- reactive({
    req(input$query_list)
    sum(uploaded_sequences()$match)
  })
  
  total_n <- reactive({
    req(input$query_list)
    nrow(uploaded_sequences())
  })
  
  output$out_txt <- renderText({
    print(paste(match_n(), "matches out of", total_n()))
  })
  
  
  output$copy_btn <- renderUI({
    req(uploaded_sequences())
      copy_data <- uploaded_sequences()$match
      rclipButton("copy_btnn", icon = icon("clipboard"), label= "Copy match _0/1_ vector to clipboard", clipText = copy_data)
  })
  
  output$copy_idx_btn <- renderUI({
    req(uploaded_sequences())
    copy_data <- paste(uploaded_sequences()$index[uploaded_sequences()$match == 1], collapse = ",")
    rclipButton("copy_idx_btnn", icon = icon("clipboard"), label= "Copy match _index_ vector to clipboard", clipText = copy_data)
  })
  
  
  
  
  # observeEvent(input$copy_btn, {
  #   copy_data <- uploaded_sequences()$match
  #   rclipButton("goButton", label= "Go!", clipText = tibble(iris))
  #   clipr::write_clip(copy_data, allow_non_interactive = TRUE)
  # })
  # 
  # observeEvent(input$copy_idx_btn, {
  #   copy_data <- uploaded_sequences()$idx[uploaded_sequences()$match == 1]
  #   rclipButton("goButton", label= "Go!", clipText = tibble(iris))
  #   clipr::write_clip(copy_data, allow_non_interactive = TRUE)
  # })
  
}

# Run the application 
shinyApp(ui = ui, server = server)