#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


#Package to connect MySQL
library(RMySQL)


# Create a database connection
mydb= dbConnect(MySQL(),
                dbname='vulcano3',
                host='192.168.0.1',
                port=3306,
                user='rebeca',
                password='Rebeca1')



# Listing table and fields
dbListTables(mydb)
dbListTables(mydb, 'ejecucion')


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  tags$body(HTML("
                  <div id ='Nav'>
                  <H1>CONSULTA DE DATOS DE REPARACIÓN - VULCANO LUBRICACIÓN</H2>
                  </div>
                  <p style='text-align: right;border:2px solid black;'>
                  <img src='https://cdn.discordapp.com/attachments/880790281925050369/943880293507559424/unknown.png'style='border-color:''>
                  </p>
                   ")),
  
  # Create a new Row in the UI for selectInputs
  
  dateRangeInput('dateRange',
                 label = 'Ingrese la fecha de consulta:',
                 start = Sys.Date() - 2, end = Sys.Date() + 2
  ),
  
  
  hr(),
  fluidRow(column(2, verbatimTextOutput("value"))),
  tags$head(HTML("<style type='text/css'>
                #mybutton{
                background-color: red;
                
                }
                h1{
                background-color:#d7d7d7;
                border-radius:6px;
                }
                H2{
                background-color:  #ababb9;
                color:black;
                border-radius:6px;
                }
                
                img{
                text-align: right;
                border-color: red;
                }
                 
                 </style>")),
  
  tags$body(HTML("<H2>CLIENTES MÁS FRECUENTES</H2>
                  <div id ='sampleanimation'>
                  </div>
                  ")),
  DT::dataTableOutput("myTable"),
  tags$body(HTML("<H2>% REPARACIONES CON COSTO</H2>
                  <div id ='sampleanimation'>
                  </div>
                  ")),
  DT::dataTableOutput("myTable2"),
  tags$body(HTML("<H2>% REPARACIONES CON ATENCIÓN COMERCIAL</H2>
                  <div id ='sampleanimation'>
                  </div>
                  ")),
  DT::dataTableOutput("myTable3"),
  tags$body(HTML("<H2>% REPARACIONES CON GARANTÍA</H2>
                  <div id ='sampleanimation'>
                  </div>
                  ")),
  DT::dataTableOutput("myTable4"),
  tags$body(HTML("<H2>% EMAIL ENVIADOS DE AVISOS DE INFORMES</H2>
                  <div id ='sampleanimation'>
                  </div>
                  ")),
  DT::dataTableOutput("myTable5"),
  tags$body(HTML("<H2>% POR FAMILIA</H2>
                  <div id ='sampleanimation'>
                  </div>
                  ")),
  DT::dataTableOutput("myTable6"),
  tags$body(HTML("<H2>TIEMPO EN QUE TRANSCURRE ENTRE EL INGRESO DEL PRODUCTO HASTA AUTORIZACIÓN</H2>
                  <div id ='sampleanimation'>
                  </div>
                  ")),
  DT::dataTableOutput("myTable7")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$myTable = DT::renderDataTable({
    df <- dbGetQuery(mydb,
                     glue::glue("SELECT  ejecucion.cliente ,COUNT(ejecucion.cliente) AS value_frequent FROM ejecucion INNER JOIN fechas ON ejecucion.idOR = fechas.idOR WHERE fecha_reparacion between date'{as.character(input$dateRange[1])}' and date'{as.character(input$dateRange[2])}' GROUP BY ejecucion.cliente ORDER BY value_frequent DESC LIMIT 10"))
  })
  
  output$myTable2 = DT::renderDataTable({
    df2 <- dbGetQuery(mydb,
                      glue::glue("SET @total := (SELECT count(detalle_aut)  FROM ejecucion INNER JOIN fechas ON ejecucion.idOR = fechas.idOR where fecha_autorizacion between date'{as.character(input$dateRange[1])}' and date'{as.character(input$dateRange[2])}')"))
    df5 <- dbGetQuery(mydb,
                      glue::glue("SELECT detalle_aut, CONCAT(ROUND((COUNT(detalle_aut)/ @total  * 100), 2), '%') AS porcentaje FROM ejecucion INNER JOIN fechas ON ejecucion.idOR = fechas.idOR WHERE ejecucion.detalle_aut ='CON COSTO' and fecha_autorizacion  between date'{as.character(input$dateRange[1])}' and date'{as.character(input$dateRange[2])}'"))
    
  })
  
  output$myTable3 = DT::renderDataTable({
    df3 <- dbGetQuery(mydb,
                      glue::glue("SET @total := (SELECT count(detalle_aut)  FROM ejecucion INNER JOIN fechas ON ejecucion.idOR = fechas.idOR where fecha_autorizacion between date'{as.character(input$dateRange[1])}' and date'{as.character(input$dateRange[2])}')"))
    df6 <- dbGetQuery(mydb,
                      glue::glue("SELECT detalle_aut, CONCAT(ROUND((Count(detalle_aut)/ @total  * 100), 2), '%') AS porcentaje FROM ejecucion INNER JOIN fechas ON ejecucion.idOR = fechas.idOR WHERE ejecucion.detalle_aut ='ATENCIÓN COMERCIAL' and fecha_autorizacion between date'{as.character(input$dateRange[1])}' and date'{as.character(input$dateRange[2])}'"))
  })
  
  output$myTable4 = DT::renderDataTable({
    df4 <- dbGetQuery(mydb,
                      glue::glue("SET @total := (SELECT count(detalle_aut)  FROM ejecucion INNER JOIN fechas ON ejecucion.idOR = fechas.idOR where fecha_autorizacion between date'{as.character(input$dateRange[1])}' and date'{as.character(input$dateRange[2])}')"))
    df7 <- dbGetQuery(mydb,
                      glue::glue("SELECT detalle_aut, CONCAT(ROUND((Count(detalle_aut)/ @total  * 100), 2), '%') AS porcentaje FROM ejecucion INNER JOIN fechas ON ejecucion.idOR = fechas.idOR WHERE ejecucion.detalle_aut ='GARANTÍA' and fecha_autorizacion between date'{as.character(input$dateRange[1])}' and date'{as.character(input$dateRange[2])}'"))
  })
  
  output$myTable5 = DT::renderDataTable({
    df8 <- dbGetQuery(mydb,
                      glue::glue("SELECT concat(round((((SELECT COUNT(DISTINCT cliente) FROM vulcano3.ejecucion INNER JOIN clientes ON ejecucion.cliente = clientes.rz AND clientes.EMail != ' ')/count(DISTINCT cliente)) * 100), 2), '%') AS PORCENTAJE FROM ejecucion"))
  })
  
  output$myTable6 = DT::renderDataTable({
    df9 <- dbGetQuery(mydb,
                      glue::glue("SELECT grupo AS IdGrupo,nombre as PORFAMILIA,count(nombre) as Cantidad,CONCAT(ROUND((Count(grupo)/ (SELECT count(producto) FROM ejecucion INNER JOIN fechas ON ejecucion.idOR = fechas.idOR where fecha_autorizacion between date'{as.character(input$dateRange[1])}'and date'{as.character(input$dateRange[2])}') * 100), 2), '%') AS Porcentaje from grupos INNER JOIN productos ON grupos.id = productos.grupo inner join ejecucion on productos.cgo = SUBSTRING_INDEX(producto,' ',1) INNER JOIN fechas ON ejecucion.idOR = fechas.idOR WHERE fecha_reparacion between date'{as.character(input$dateRange[1])}'and date'{as.character(input$dateRange[2])}'group by nombre order by Cantidad desc")) 
  })
  
  output$myTable7 = DT::renderDataTable({
    df10 <- dbGetQuery(mydb,
                       glue::glue("SELECT  ejecucion.idProd,ejecucion.producto ,ejecucion.cliente,((DATEDIFF(fecha_autorizacion , fecha_ingreso)) - ((WEEK(fecha_autorizacion ) - WEEK(fecha_ingreso)) * 2) - (case when weekday(fecha_autorizacion ) = 6 then 1 else 0 end) - (case when weekday(fecha_ingreso) = 5 then 1 else 0 end))AS day_authoriza FROM ejecucion INNER JOIN fechas ON ejecucion.idOR = fechas.idOR WHERE fecha_autorizacion between date'{as.character(input$dateRange[1])}' and date'{as.character(input$dateRange[2])}' GROUP BY fecha_ingreso,fecha_autorizacion ORDER BY day_authoriza DESC")) 
  })
  
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)