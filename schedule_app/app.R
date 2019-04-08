library(shiny)
library(shinyjs)
library(DT)
library(tidyverse)
library(hms)
library(plotly)
library(htmlwidgets)
library(lubridate)

# TEST DATA --------------------------------------------------

sample_table <-
  tibble(
    `Patient ID` = round(runif(37, min=0, max=1000000)),
    `Time` = as.character(as.hms(seq(from = 1*3600, to = 10*3600, by = 3600/4))),
    `Probability` = round(runif(37, min=0, max=1),2)
  )

cal2 <-
  tibble(
    start = as_datetime(c("2019-04-05 18:36:00","2019-04-06 18:37:00")),
    start_hms = as.hms(start),
    start_date = as_date(start),
    start_int = as.integer(start_hms),
    end = as_datetime(c("2019-04-05 19:36:00","2019-04-06 19:37:00")),
    end_hms = as.hms(end),
    end_date = as_date(end),
    end_int = as.integer(end_hms),
    Patient = c("Brandon Gleason","Wendan Sun"),
    PatientID = c("1","2"),
    Probability = c(0.1,0.9)
  )

times2 <-
  ggplot(cal2) +
  geom_rect(
    aes(
      xmin = as_datetime(start_date) + lubridate::hours(11), 
      xmax = as_datetime(start_date) - lubridate::hours(11),
      ymin = start_int,
      ymax = end_int,
      fill = Probability
    )
  ) +
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%a\n%e",
    position = "top"
  ) +
  scale_y_reverse(
    limits = 3600*c(19,7),
    breaks = 3600*19:7,
    minor_breaks = NULL,
    labels = paste(c(7:1,12:7),c(rep("PM", 7), rep("AM", 6)))
  ) +
  scale_fill_distiller(
    palette = "RdYlGn",
    limits = c(0,1)
  ) +
  theme_minimal() +
  theme(
    # legend.position = "none",
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    fill = "No Show Probability"
  )

# UI --------------------------------------
ui <- navbarPage(
  
  "UnityPoint NoShows",
 
  tabPanel(
    title="Weekly",
    id="myID",
    column(
      value="table1",
      width=6,
      DT::dataTableOutput("day1")
    ),
    column(
      id="table2",
      width=6,
      DT::dataTableOutput("day2")
    )
  ),
  tabPanel(
    title = "Daily",
    DT::dataTableOutput("daily")
  ),
  tabPanel(
    title="Plotly",
    plotlyOutput("plot"),
    verbatimTextOutput("clickres")
  )
  
)

# SERVER --------------------------------------
server <- function(input, output) {
  output$daily = DT::renderDataTable({
    sample_table
  })
  
  output$day1 = DT::renderDataTable({
    sample_table
  })
  
  output$day2 = DT::renderDataTable({
    sample_table
  })
  
  output$plot <- renderPlotly({
    ggplotly(times2)
  })
  
  output$clickres <- renderPrint({
    event_data("plotly_click")
  })
  
}

# APP -----------------------------------------------
shinyApp(ui, server)

# TODO -----------------------------------
# 1. Build seven day view on calendar
#     a. Day separator vertical lines on ggplot
# X 2. Add color for probability
# 3. Add patient view on click from calendar view
#     a. Historical appointment schedule
#     b. Waitlist for replacement
#     c. Optional: Note recording tool
