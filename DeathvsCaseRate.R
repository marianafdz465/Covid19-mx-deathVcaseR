##************************************************************************
## Script Name: Cumulative Death rate vs Cumulative Case rate
## Purpose:
##
##
## Created:
## Authors: Mariana Fernandez
##
## GitHub: marianafdz465
##
##
##************************************************************************


# Libraries ---------------------------------------------------------------
library(ggplot2)
library(readr)
library(tidyverse)
library(ggpmisc)

# Data Set ----------------------------------------------------------------

urlfile="https://github.com/PADeCI/covid19-mx-data/blob/master/data/state/covid_ssa_state_2021-06-12.csv?raw=true"
covidData<-read_csv(url(urlfile))


# Filter DataSet ----------------------------------------------------------

conf_vs_death <- function(data = "CovidData",
                          type = "cum_cases",
                          types = "cum_cases",
                          select_date = "2021-06-12",
                          save_plot = FALSE) {


  # #General values
  # type <-  "cum_cases"
  # select_date = "2021-06-12"
  # types <- "cum_cases"
  #state <- covidData[covidData$var_resultado == 'Confirmados',]
  state <-  covidData
  n_time_stamp <- covidData$time_stamp[1]

  # Confirmed cases
  confirmed <- subset(state, var_resultado == "Confirmados") #We are focusing on confirmed cases
  confirmed <- confirmed %>%
    group_by(entidad) %>%
    rename(Confirmados = types) %>%
    mutate(last_case = select_date)
  confirmed <- subset(confirmed, date == last_case) #Keep only the last entry
  confirmed <- confirmed[c("entidad", "Confirmados", "population")]
  confirmed <- transform(confirmed, confirmed_rate = (Confirmados / population)* 100000)


  #Deaths
  #state <- covidData[covidData$var_resultado == 'Confirmados',]
  state2 <-  covidData

  # Confirmed cases
  deaths <- subset(state2, var_resultado == "Muertes") #We are focusing on confirmed cases
  deaths <- deaths %>%
    group_by(entidad) %>%
    rename(Muertes = types) %>%
    mutate(last_case = select_date)
  deaths <- subset(deaths, date == last_case) #Keep only the last entry
  deaths <- deaths[c("entidad", "Muertes", "population")]
  deaths <- transform(deaths,death_rate = (Muertes / population)* 100000)

  #Create just one data set
  total <- merge(deaths,confirmed,by="entidad")



  # Graph -------------------------------------------------------------------
  my.formula <-  total$confirmed_rate ~ total$death_rate

  # Add the regression line
  figure <- ggplot(total, aes(x=death_rate,y=confirmed_rate)) +
    geom_point() +
    geom_text(
      label=total$entidad,
      check_overlap = TRUE,
       vjust=3, hjust=0.5,
      nudge_x = 3) +
    geom_smooth(method=lm, se = FALSE, color = "black") +
    stat_poly_eq(formula = my.formula,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold",
                                    size = 16,
                                    family = "Arial"),
          plot.subtitle = element_text(size = 12,
                                       face = "plain",
                                       family = "Arial")) +
    labs(title = "Cumulative Death rate vs Cumulative Case rate",
         subtitle = "Mexico",
         x = "Cumulative death rate (per 100,000)",
         y = "Cumulative case rate (per 100,000) " ,
         caption = paste("\n @PADeCI1 ",
                         "Source: Dirección de Epidemiología de la",
                         "Secretaría de
                         Salud, ", select_date, ".")
         )
  print(figure)
  if (save_plot == TRUE) {
    ggsave(paste0("RL-",n_time_stamp,
                 ".jpeg"),
           width = 14, height = 6)}

}


