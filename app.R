library(shiny)
library(shinyjs)
library(dplyr)
library(readr)
library(tibble)
library(clipr)

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
  
  h1("Used barcodes"),
  helpText("This app helps you query your barcodes to a list of available barcodes, and copy a 'match' vector to clipboard. (1 = match, 0 = no match)"),
  helpText("The input query file to upload should only contains a column with your query sequences. (accepted files: .txt .csv .tsv)"),
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
           dataTableOutput("out_df"),
           
           actionButton("copy_btn", icon = icon("copy"), "Copy match vector to clipboard") # initially hidden in the server logic
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    shinyjs::toggle("copy_btn", condition = !is.null(input$query_list))
  })
  
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
    df <- read.table(input$query_list$datapath, header = FALSE)
    
    query_output <- streets_query() %in% df[,1]
    
    out_df <- tibble(barcodes = streets_query() ,
                     query = NA,
                     match = as.numeric(query_output))
    
    out_df$query[query_output] <- df[,1]
    
    print(head(out_df, 50))
    return(out_df)
  })
  
  output$out_df <- renderDataTable({
    uploaded_sequences()
  })
  
  observeEvent(input$copy_btn, {
    copy_data <- uploaded_sequences()$match
    clipr::write_clip(copy_data)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)