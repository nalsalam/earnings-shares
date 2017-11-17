#
# Calculate and show trends in the share of earnings earned by earners throughout the distribution
#

library(shiny)
library(htmltab)
library(tidyverse)
library(stringr)
library(forcats)

# scrape SSA data
ssa_wage_data <- seq(1990, 2016, 1) %>%
  map(~ htmltab::htmltab(doc = paste0("https://www.ssa.gov/cgi-bin/netcomp.cgi?year=", .x), which = c(5)) %>%
        mutate(year = .x)) %>%
  bind_rows() %>%
  
  # clean up the interval variable
  rename(interval = `Net compensation interval`) %>%
  mutate(interval = str_replace_all(interval, "[^0-9,\\.$\\b]", " ")) %>%
  mutate(interval = fct_inorder(factor(str_replace_all(interval, "[ ]+", "-")))) %>% 
  
  # organize long and convert chr to numeric for analysis
  gather(key, value, -year, -interval) %>% 
  separate(key, into = c("variable", "statistic"), sep = " >> ") %>% 
  mutate(value = str_replace_all(value, "[,$]", "")) %>% 
  mutate(value = as.numeric(value)) %>%
  
  # calculate cumulative earnings shares 
  filter(statistic == "Number" | statistic == "Aggregate amount") %>%
  select(-variable) %>%
  spread(statistic, value) %>%
  group_by(year) %>%
  arrange(interval, .by_group = TRUE) %>%
  mutate(cum_share = cumsum(`Aggregate amount`)/sum(`Aggregate amount`),
         ptile = cumsum(`Number`)/sum(`Number`)) %>%
  arrange(year, interval) %>% select(year, interval, ptile, cum_share) 

# add interpolated cumulative shares at points of interest
earnings_shares <- ssa_wage_data %>% 
  select(-interval) %>% ungroup() %>%
  add_row(year = rep(1990:2016, each = 6), ptile = rep(c(.6, .7, .8, .9, .95, .99), times = (2016-1990+1))) %>%
  add_row(year = rep(1990:2016, each = 1), ptile = rep(c(0), times = (2016-1990+1)), cum_share = rep(c(0), times = (2016-1990+1))) %>%
  group_by(year) %>% arrange(ptile, .by_group = TRUE) %>% 
  mutate(cum_share = if_else(is.na(cum_share),
                             ((ptile-lag(ptile))/(lead(ptile) - lag(ptile))) * lead(cum_share) + 
                               ((lead(ptile) - ptile)/(lead(ptile) - lag(ptile))) * lag(cum_share),
                             cum_share)) 

# calculate shares at points of interest only
earnings_shares_subset <- earnings_shares %>%
  filter(ptile %in% c(0, .6, .7, .8, .9, .95, .99) | row_number() == n()) %>%  
  group_by(year) %>% arrange(ptile, .by_group = TRUE) %>% mutate(share = cum_share - lag(cum_share)) %>%
  mutate(xtile = cut(ptile, breaks = c(0.0, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 1.00))) %>%
  mutate(xtile = ordered(xtile, labels = c("0-60", "60-70", "70-80", "80-90", "90-95", "95-99", "99+"))) 

xtiles <- c("0-60", "60-70", "70-80", "80-90", "90-95", "95-99", "99+")

ui <- fluidPage(
   
   titlePanel("Earnings Shares Calculated from SSA W-2 Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         checkboxGroupInput("xtiles",
                     "Choose percentile range to plot:",
                     choices = xtiles,
                     selected = c("90-95", "95-99", "99+")
                     )
      ),
      
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     earnings_shares_subset %>% 
       filter(xtile %in% input$xtiles) %>%
       ggplot(aes(x = year, y = share, color = xtile)) +
       geom_line() + 
       labs(x = "Year", y = "Share", color = "Earner\nPercentile\nRange",
            title = "Share of Aggregate Earnings",
            caption = "Source: Based on data from https://www.ssa.gov/cgi-bin/netcomp.cgi?year=2016") +
       scale_x_continuous(breaks = c(seq(1990, 2010, 5), 2016)) + 
       theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10)))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

