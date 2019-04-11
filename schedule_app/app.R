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



# UI --------------------------------------
ui <- navbarPage(
  
  useShinyjs(),
  # HTML('<script>
  #   $(document).ready(function() {
  #     $("#donut").css("height","200px");
  #   })          
  # </script>'),
  
  title = "UnityPoint Scheduler",
 
  tabPanel(
    title="Schedule",
    id="ScheduleId",
    sidebarLayout(
      sidebarPanel(
        dateRangeInput("select_date", label = "Choose date range:")
      ),
      mainPanel(
        plotlyOutput("plot")
      )
    )
  ),
  
  # tabPanel(
  #   title = "Patient Profile",
  #   sidebarLayout(
  #     mainPanel(
  #       DT::dataTableOutput("appt_hist")
  #     ),
  #     sidebarPanel(
  #       position = "right",
  #       "George Washington",
  #       plotlyOutput("donut"),
  #       tableOutput("patientsummary")
  #     )
  #   )
  
    # fluidPage(
    #   column(9,
    #     DT::dataTableOutput("appt_hist")
    #   ),
    #   column(3,
    #     "George Washington",
    #     column(8,
    #       plotlyOutput("donut")
    #     ),
    #     column(4,
    #       tableOutput("patientsummary")
    #     )
    #   )
    # )
  tabPanel(
    title="Patient Profile",
    fluidPage(
      column(9,
        DT::dataTableOutput("appt_hist")
      ),
      column(3,
        "George Washington",
        fixedRow(height=200, plotlyOutput("donut")),
        fluidRow(tableOutput("patientsummary"))
      )
    )
  )
)

# SERVER --------------------------------------
server <- function(input, output) {
  
  output$appt_hist <- renderDataTable({
    
      tibble(
        `Appointment ID` = 1:10,
        `Date` = as_datetime(c(
          "2019-03-01 09:00:00",
          "2019-03-14 09:30:00",
          "2019-03-15 12:15:00",
          "2019-03-19 08:45:00",
          "2019-03-21 16:00:00",
          "2019-03-21 18:00:00",
          "2019-03-22 09:00:00",
          "2019-03-25 11:15:00",
          "2019-04-02 14:15:00",
          "2019-04-04 12:00:00"
        ), tz="US/Eastern"),
        `No-Show or Late Cancel` = c(F,F,F,T,F,F,F,F,F,F)
      )
  }, rownames = F)

cal2 <-
  tibble(
    start = as_datetime(
      c("2019-04-05 10:00:00","2019-04-06 17:37:00", "2019-04-07 16:00:00",
        "2019-04-08 15:30:00","2019-04-09 11:30:00", "2019-04-10 12:00:00",
        "2019-04-11 13:30:00"),
      tz="US/Eastern"
    ),
    start_hms = as.hms(start), start_date = as_date(start), start_int = as.integer(start_hms),
    end = as_datetime(
      c("2019-04-05 11:57:00","2019-04-06 18:37:00", "2019-04-07 18:45:00",
        "2019-04-08 17:30:00","2019-04-09 12:30:00", "2019-04-10 13:35:00",
        "2019-04-11 15:30:00"),
      tz="US/Eastern"
    ),
    end_hms = as.hms(end), end_date = as_date(end), end_int = as.integer(end_hms),
    Patient = c("George\nWashington", "Benjamin\nFranklin", "Thomas\nJefferson", 
                "John\nAdams", "Alexander\nHamilton", "James\nMadison", "John\nJay"),
    PatientID = as.character(1:7),
    Probability = c(0.93, 0.49, 0.15, 0.12, 0.29, 0.4, 0.34),
    x_i = as_datetime(start_date) + lubridate::hours(11),
    x_f = as_datetime(end_date) - lubridate::hours(11),
    vline = as_datetime(start_date) + lubridate::hours(12),
    text_color = case_when(Probability < 0.5 ~ "black", T ~ "white")
  )

times2 <-
ggplot(cal2, aes(x = x_i, y = start_int, xmin = x_i, xmax = x_f, ymin = start_int,
                 ymax = end_int, fill = Probability, label = Patient)) +
  geom_rect() +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a\n%e", position = "top") +
  scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0,1)) +
  theme_minimal() +
  theme(legend.position = "none", axis.ticks = element_blank(), 
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title = element_blank()) +
  labs(fill = "No Show Probability", title = "") +
  scale_y_reverse(limits = 3600*c(19,7), breaks = 3600*19:7, minor_breaks = NULL,
                  labels = paste(c(7:1,12:7),c(rep("PM", 8), rep("AM", 5)))) +
  geom_text(aes(color = I(text_color)),size = 2.3, nudge_y = -1800, nudge_x = -3600*10.5) 

  output$plot <- renderPlotly({
    ggplotly(times2) %>%
      config(displayModeBar = F)
  })
  
  output$donut <- renderPlotly({
    tibble(
      `Appointment ID` = 1:10,
      `Date` = as_datetime(c(
        "2019-03-01 09:00:00",
        "2019-03-14 09:30:00",
        "2019-03-15 12:15:00",
        "2019-03-19 08:45:00",
        "2019-03-21 16:00:00",
        "2019-03-21 18:00:00",
        "2019-03-22 09:00:00",
        "2019-03-25 11:15:00",
        "2019-04-02 14:15:00",
        "2019-04-04 12:00:00"
      ), tz="US/Eastern"),
      `No-Show or Late Cancel` = c(F,F,F,T,F,F,F,F,F,F)
    ) %>%
      summarize(`NoShow` = mean(`No-Show or Late Cancel`),
                `Show` = 1 - `NoShow`) %>%
      gather(`Show`,`NoShow`, key="key", value="value") %>%
      plot_ly(labels = ~key, values = ~value) %>%
      add_pie(hole = 0.6) %>%
      config(displayModeBar = F) %>%
      layout(
        showlegend = F,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        height = 200
      ) 
  })
  
  output$patientsummary <- renderTable({
    
    tibble(
      ` ` = c(
        "Patient ID",
        "Strikes",
        "Historic NSLC Rate",
        "Chronic Ailments",
        "Age",
        "Gender"
      ),
      `  ` = c(
        "1322018",
        "1",
        "0.5",
        "Diabetes",
        "287",
        "Male"
      )
    )
    
  })
    
  runjs('    
    $(document).ready(function() {
        $("#donut").css("height","200px");
    })
  ')
  
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
