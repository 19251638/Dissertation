#Load packages
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(readxl)
library(lubridate)
library(scales)
library(plotly)
library(tidyverse)

#interactive dashboard packages
install.packages(c("flexdashboard", "plotly", "DT"))

library(flexdashboard)
library(plotly)
library(DT)




#Set working directory
setwd("C:/Users/User/Documents/Dissertation")

#read datasets
business_data<-read.csv('business-demographics.csv')
sectors<-read.csv('2024 borough by sector.csv')
job_density<-read.csv('jobs-and-job-density.csv')
workforce_jobs<-read.csv('workforce-jobs.csv')
workforce_sector<-read.csv('workforce-jobs-by-sector.csv')










#clean business data

business_data<-read.csv('business-demographics.csv')

head(business_data)

#filter years
business_london<-business_data %>%
  filter(year >= 2015 & year <= 2023) %>%
  filter(area=="London")


#total active enterprises
business_summary <- business_london %>%
  group_by(year) %>%
  summarise(
    total_active_enterprises = sum(active_enterprises, na.rm = TRUE),
    total_births = sum(births, na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE))

library(dplyr)
library(tibble)

#combining 2023 data with dataset
business_summary <- business_summary %>%
  bind_rows(
    tibble(
      year = 2023,
      total_active_enterprises = 592330,
      total_births = 74650,
      total_deaths = 65175
    )
  )

library(dplyr)
library(tibble)

#combining 2024 data with dataset
business_summary <- business_summary %>%
  bind_rows(
    tibble(
      year = 2024,
      total_active_enterprises = 595280,  
      total_births = 75550,               
      total_deaths = 61245                
    )
  )

# Total Active enterprise graph
library(scales)

max_y <- max(business_summary$total_active_enterprises, na.rm = TRUE)
label_y <- max_y * 1.003  

p_active <- ggplot(business_summary,
                   aes(x = year, y = total_active_enterprises)) +
  
  # ULEZ 2019 line
  geom_vline(xintercept = 2019, linetype = "dashed", linewidth = 0.9) +
  annotate("text", x = 2019, y = label_y,
           label = "ULEZ introduced",
           angle = 90, hjust = 0, size = 3.5) +
  
  # ULEZ 2021 line
  geom_vline(xintercept = 2021, linetype = "dashed",
             colour = "darkgreen", linewidth = 0.9) +
  annotate("text", x = 2021, y = label_y,
           label = "ULEZ Inner EXP",
           angle = 90, hjust = 0, size = 3.5, colour = "darkgreen") +
  
  # ULEZ 2023 line
  geom_vline(xintercept = 2023, linetype = "dashed",
             colour = "purple", linewidth = 0.9) +
  annotate("text", x = 2023, y = label_y,
           label = "ULEZ Greater EXP",
           angle = 90, hjust = 0, size = 3.5, colour = "purple") +
  
  # COVID 19 shaded period
  annotate(
    "text",
    x = 2020.5,
    y = label_y * 1.01,  
    label = "COVID-19 period",
    size = 3.5
  ) +
  
  geom_line(colour = "#1f78b4", linewidth = 1.2) +
  geom_point(colour = "#1f78b4", size = 2) +
  
  scale_x_continuous(breaks = seq(2015, 2024, 1)) +
  scale_y_continuous(
    limits = c(520000, 625000),
    breaks = seq(520000, 625000, by = 20000),
    labels = label_number(scale = 1e-3, suffix = "k")
  ) +
  
  labs(
    title = "Total Active Enterprises in London (2015–2024)",
    x = "Year",
    y = "Active enterprises (thousands)"
  ) +
  
  theme_minimal()

ggplotly(p_active)




#births/deaths graph
library(ggplot2)
library(plotly)
library(scales)

business_summary$year <- as.numeric(business_summary$year)

p_birth_death<- ggplot(business_summary, aes(x = year)) +
  
  # COVID shaded period
  geom_rect(
    aes(
      xmin = 2020,
      xmax = 2021,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey50",
    alpha = 0.25,
    inherit.aes = FALSE
  ) +
  
  # ULEZ vertical line
  geom_vline(
    xintercept = 2019,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.9
  ) +
  
  annotate(
    "text",
    x = 2019,
    y = max(business_summary$total_births, na.rm = TRUE),
    label = "ULEZ introduced",
    angle = 90,
    vjust = -0.5,
    size = 3.5
  ) +
  
  # ULEZ 2021 vertical line
  geom_vline(
    xintercept = 2021,
    linetype = "dashed",
    colour = "darkgreen",
    linewidth = 0.9
  ) +
  
  annotate(
    "text",
    x = 2021,
    y = max(business_summary$total_births, na.rm = TRUE),
    label = "ULEZ Inner EXP",
    angle = 90,
    vjust = -0.5,
    size = 3.5
  ) +
  
  # ULEZ 2023 vertical line
  geom_vline(
    xintercept = 2023,
    linetype = "dashed",
    colour = "purple",
    linewidth = 0.9
  ) +
  
  annotate(
    "text",
    x = 2023,
    y = max(business_summary$total_births, na.rm = TRUE),
    label = "ULEZ Greater EXP",
    angle = 90,
    vjust = -0.5,
    size = 3.5
  ) +
  
  
  annotate(
    "text",
    x = 2020.5,
    y = max(business_summary$total_births, na.rm = TRUE) * 0.95,
    label = "COVID-19 period",
    size = 3.5
  ) +
  
  geom_line(aes(y = total_births, colour = "Births"), linewidth = 1.1) +
  geom_line(aes(y = total_deaths, colour = "Deaths"), linewidth = 1.1) +
  
  scale_colour_manual(values = c(
    "Births" = "green",
    "Deaths" = "red"
  )) +
  
  scale_x_continuous(breaks = seq(2015, 2024, 1)) +
  scale_y_continuous(labels = comma) +
  
  labs(
    title = "Business Births and Deaths in London (2015–2024)",
    subtitle = "ULEZ introduction and COVID-19 period highlighted",
    x = "Year",
    y = "Number of Business Births / Deaths",
    colour = "Indicator"
  ) +
  
  theme_minimal()

ggplotly(p_birth_death)




#job density

# Clean Job density

# Check column names
colnames(job_density)

# Look at the first few rows
head(job_density)

# Get a summary of each column
summary(job_density)

# Check the structure (data types)
str(job_density)


job_density_long <- job_density %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    values_to = "job_density"
  ) %>%
  mutate(year = as.numeric(sub("X", "", year)))




job_density_long <- job_density %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    values_to = "job_density"
  ) %>%
  mutate(
    year = as.numeric(sub("X", "", year)),
    job_density = parse_number(job_density)
  )

#filter years

job_density_filtered <- job_density_long %>%
  filter(year >= 2016 & year <= 2023)


#filter area
city_data <- job_density_filtered %>%
  filter(Area == "London")

#Job density graph
library(plotly)

max_y <- max(city_data$job_density, na.rm = TRUE)

p_job_density <- ggplot(city_data, aes(x = year, y = job_density)) +
  
  # COVID shading
  annotate(
    "rect",
    xmin = 2019.8,
    xmax = 2021.2,
    ymin = -Inf,
    ymax = Inf,
    fill = "#bdbdbd",
    alpha = 0.35
  ) +
  
  # ULEZ 2019
  geom_vline(
    xintercept = 2019,
    linetype = "dashed",
    colour = "black",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2019,
    y = max_y,
    label = "ULEZ introduced",
    angle = 90,
    vjust = -0.4,
    size = 3.4
  ) +
  
  # ULEZ 2021 expansion
  geom_vline(
    xintercept = 2021,      
    linetype = "dashed",
    colour = "darkgreen",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2021,
    y = max_y,
    label = "ULEZ Inner EXP",
    angle = 90,
    vjust = -0.4,
    size = 3.4,
    colour = "darkgreen"
  ) +
  
  # ULEZ 2023 expansion
  geom_vline(
    xintercept = 2023,      
    linetype = "dashed",
    colour = "purple",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2023.1,                
    y = max_y * 1.005,         
    label = "ULEZ Greater EXP",
    angle = 90,
    vjust = -0.4,
    size = 3.4,
    colour = "purple"
  )+


  
  # COVID label
  annotate(
    "text",
    x = 2020.5,
    y = max_y * 0.9,
    label = "COVID-19\n(2020–2021)",
    size = 3.6
  ) +
  
  geom_line(colour = "blue", linewidth = 1.3) +
  geom_point(size = 2) +
  
  labs(
    title = "Job Density in London (2016–2023)",
    subtitle = "ULEZ introduction and COVID-19 period highlighted",
    x = "Year",
    y = "Jobs per working-age resident"
  ) +
  theme_minimal()

ggplotly(p_job_density)




#workforce graph


# Data cleaning


workforce_jobs <- workforce_jobs %>%
  mutate(
    year = year(ymd(paste0(date, "-01"))),
    quarter = quarter(ymd(paste0(date, "-01")))
  )

workforce_london <- workforce_jobs %>%
  filter(area == "London", year >= 2016 & year <= 2024)

workforce_summary <- workforce_london %>%
  group_by(year) %>%
  summarise(
    total_jobs = mean(total.workforce.jobs, na.rm = TRUE),
    employee_jobs = mean(employee.jobs, na.rm = TRUE),
    self_employment_jobs = mean(self.employment.jobs, na.rm = TRUE)
  )

# Max Y for annotations

max_y <- max(
  workforce_summary$total_jobs,
  workforce_summary$employee_jobs,
  workforce_summary$self_employment_jobs,
  na.rm = TRUE
)


# Interactive Plot for workforce


p_workforce <- ggplot(workforce_summary, aes(x = year)) +
  
  # COVID shaded period
  annotate(
    "rect",
    xmin = 2019.8,
    xmax = 2021.2,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey70",
    alpha = 0.35
  ) +
  
  # ULEZ introduction line
  geom_vline(
    xintercept = 2019,
    linetype = "dashed",
    colour = "black",
    linewidth = 1
  ) +
  
  # COVID label
  annotate(
    "text",
    x = 2020.5,          
    y = max_y * 0.68,   
    label = "COVID-19\n(2020–2021)",
    size = 4,
    fontface = "bold"
  ) +
  
  
  # ULEZ label
  annotate(
    "text",
    x = 2019,
    y = max_y,
    label = "ULEZ introduced",
    angle = 90,
    vjust = -0.4,
    size = 3.6
  ) +
  
  # ULEZ 2021 expansion
  geom_vline(
    xintercept = 2021,      
    linetype = "dashed",
    colour = "darkgreen",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2021,
    y = max_y,
    label = "ULEZ Inner London EXP",
    angle = 90,
    vjust = -0.4,
    size = 3.4,
    colour = "darkgreen"
  ) +
  
  # ULEZ 2023 expansion
  geom_vline(
    xintercept = 2023,      
    linetype = "dashed",
    colour = "purple",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2023.1,                 
    y = max_y * 1.005,          
    label = "ULEZ Greater EXP",
    angle = 90,
    vjust = -0.4,
    size = 3.4,
    colour = "purple"
  )+
  
  # Workforce job lines
  geom_line(aes(y = total_jobs, colour = "Total jobs"), linewidth = 1.2) +
  geom_line(aes(y = employee_jobs, colour = "Employee jobs"), linewidth = 1.2) +
  geom_line(aes(y = self_employment_jobs, colour = "Self-employed jobs"), linewidth = 1.2) +
  
  # Colours
  scale_colour_manual(
    values = c(
      "Total jobs" = "#1b9e77",
      "Employee jobs" = "#7570b3",
      "Self-employed jobs" = "#d95f02"
    )
  ) +
  
 
  scale_y_continuous(labels = comma) +
  
  labs(
    title = "Workforce Jobs in London (2016–2024)",
    subtitle = "Impact of ULEZ introduction and COVID-19 period",
    x = "Year",
    y = "Number of Jobs",
    colour = "Job Type"
  ) +
  
  theme_minimal()

ggplotly(p_workforce)



#workforce sector cleaning

# filter key sectors to keep it readable
workforce_sector_filtered <- workforce_sector %>%
  filter(year >= 2016 & year <= 2023)

#select key sectors
key_sectors <- c(
  "Construction",
  "Accommodation and food service activities",
  "Transportation and storage",
  "Wholesale and retail trade; repair of motor vehicles and motorcycles",
  "Professional, scientific and technical activities"
)

sector_plot_data <- workforce_sector_filtered %>%
  filter(industries %in% key_sectors)

#sector workforce graph

max_y <- max(sector_plot_data$jobs, na.rm = TRUE)

p_sector <- ggplot(
  sector_plot_data,
  aes(x = year, y = jobs, colour = industries)
) +
  
  # COVID shaded period
  annotate(
    "rect",
    xmin = 2019.8,
    xmax = 2021.2,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey70",
    alpha = 0.35
  ) +
  
  # ULEZ 2019
  geom_vline(
    xintercept = 2019,
    linetype = "dashed",
    colour = "black",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2019,
    y = max_y * 1.005,
    label = "ULEZ introduced",
    angle = 90,
    vjust = -0.4,
    size = 3.4
  ) +
  
  # ULEZ 2021
  geom_vline(
    xintercept = 2021,
    linetype = "dashed",
    colour = "darkgreen",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2021,
    y = max_y * 1.005,
    label = "ULEZ Inner London EXP",
    angle = 90,
    vjust = -0.4,
    size = 3.4,
    colour = "darkgreen"
  ) +
  
  # ULEZ 2023
  geom_vline(
    xintercept = 2023,
    linetype = "dashed",
    colour = "purple",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2023.1,
    y = max_y * 1.005,
    label = "ULEZ Greater London EXP",
    angle = 90,
    vjust = -0.4,
    size = 3.4,
    colour = "purple"
  ) +
  
  # COVID label 
  annotate(
    "text",
    x = 2020.5,          
    y = max_y * 0.92,    
    label = "COVID-19\n(2020–2021)",
    size = 4,
    fontface = "bold"
  ) +
  
  # Sector lines
  geom_line(linewidth = 1.1) +
  
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    title = "Sectoral Workforce Trends in London (2015–2023)",
    subtitle = "Selected industries",
    x = "Year",
    y = "Number of Jobs",
    colour = "Industry"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

ggplotly(p_sector)



#Environmental indicators

#no2 dataset
no2_data <- tibble(
  year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024),
  no2 = c(53,51,48,42,34,32,31,28,24)
)





#interactive dashboard

max_y <- max(no2_data$no2, na.rm = TRUE)

p_no2 <- ggplot(no2_data, aes(x = year, y = no2)) +
  
  # COVID shading
  annotate(
    "rect",
    xmin = 2019.8,
    xmax = 2021.2,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey70",
    alpha = 0.35
  ) +
  
  # ULEZ 2019
  geom_vline(
    xintercept = 2019,
    linetype = "dashed",
    linewidth = 1
  ) +
  
  # ULEZ 2021 expansion
  geom_vline(
    xintercept = 2021,
    linetype = "dashed",
    colour = "darkgreen",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2021,
    y = max_y * 1.005,
    label = "ULEZ Inner London EXP",
    angle = 90,
    vjust = -0.4,
    size = 3.4,
    colour = "darkgreen"
  ) +
  
  # ULEZ 2023 expansion
  geom_vline(
    xintercept = 2023,
    linetype = "dashed",
    colour = "purple",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2023.1,
    y = max_y * 1.005,
    label = "ULEZ Greater EXP",
    angle = 90,
    vjust = -0.4,
    size = 3.4,
    colour = "purple"
  ) +
  
  # NO2 line
  geom_line(colour = "#1f78b4", linewidth = 1.2) +
  geom_point(size = 3, colour = "#1f78b4") +
  
  # ULEZ 2019 label
  annotate(
    "text",
    x = 2019,
    y = max_y * 0.92,
    label = "ULEZ introduced",
    angle = 90,
    vjust = -0.5,
    size = 4
  ) +
  
  # COVID label (centred)
  annotate(
    "text",
    x = 2020.5,
    y = max_y * 0.85,
    label = "COVID-19 period",
    size = 4,
    fontface = "bold"
  ) +
  
  labs(
    title = "Annual Average NO₂ Concentration in London (2016–2024)",
    subtitle = "Measured concentrations (µg/m³)",
    x = "Year",
    y = "NO₂ concentration (µg/m³)",
    caption = "2024 data up to September"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggplotly(p_no2, tooltip = c("x", "y"))



# pm25 dataset
pm25_data <- tibble(
  year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024),
  pm25 = c(15.0, 13.3, 13.2, 12.6, 10.4, 10.2, 10.5, 9.2, 8.3)
)

#Pm25 graph

max_y <- max(pm25_data$pm25, na.rm = TRUE)

p_pm25 <- ggplot(pm25_data, aes(x = year, y = pm25)) +
  
  # COVID shading
  annotate(
    "rect",
    xmin = 2019.8,
    xmax = 2021.2,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey70",
    alpha = 0.35
  ) +
  
  # ULEZ 2019
  geom_vline(
    xintercept = 2019,
    linetype = "dashed",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2019,
    y = max_y * 1.03,
    label = "ULEZ introduced",
    angle = 90,
    vjust = -0.4,
    size = 3.6
  ) +
  
  # ULEZ 2021 expansion
  geom_vline(
    xintercept = 2021,
    linetype = "dashed",
    colour = "darkgreen",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2021,
    y = max_y * 1.03,
    label = "ULEZ Inner London EXP",
    angle = 90,
    vjust = -0.4,
    size = 3.4,
    colour = "darkgreen"
  ) +
  
  # ULEZ 2023 expansion
  geom_vline(
    xintercept = 2023,
    linetype = "dashed",
    colour = "purple",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2023.1,
    y = max_y * 1.03,
    label = "ULEZ Greater London EXP",
    angle = 90,
    vjust = -0.4,
    size = 3.4,
    colour = "purple"
  ) +
  
  # PM2.5 line
  geom_line(colour = "#1f78b4", linewidth = 1.2) +
  geom_point(size = 3, colour = "#1f78b4") +
  
  # COVID label (centred)
  annotate(
    "text",
    x = 2020.5,          # midpoint of shaded region
    y = max_y * 0.90,    # visually centred vertically
    label = "COVID-19 period",
    size = 4,
    fontface = "bold"
  ) +
  
  labs(
    title = "Annual Average PM2.5 Concentration in London (2016–2024)",
    subtitle = "Measured concentrations (µg/m³)",
    x = "Year",
    y = "PM2.5 concentration (µg/m³)",
    caption = "2024 data up to September"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggplotly(p_pm25, tooltip = c("x", "y"))







#3d graph (workforce)
sector_wide <- workforce_sector_filtered %>%
  select(year, industries, jobs) %>%
  pivot_wider(names_from = industries, values_from = jobs)

z_matrix <- as.matrix(sector_wide[ , -1])

p_surface3d <- plot_ly(
  x = sector_wide$year,
  y = seq_len(ncol(z_matrix)),   # FIXED
  z = z_matrix,
  type = "surface",
  colorscale = "Viridis"
  
) %>%
  layout(
    title = "3D Surface Model: Workforce Jobs by Sector (2016–2023)",
    scene = list(
      xaxis = list(title = "Year"),
      yaxis = list(
        title = "Sector",
        tickvals = seq_len(ncol(z_matrix)),
        ticktext = colnames(z_matrix)
      ),
      zaxis = list(title = "Jobs")
    )
  )


#3d graph(business birth/death)
library(plotly)

p_birthdeath3d <-plot_ly(
  business_summary,
  x = ~year,
  y = ~total_births,
  z = ~total_deaths,
  type = "scatter3d",
  mode = "lines+markers",
  marker = list(size = 4)
) %>%
  layout(
    title = "3D Model: Year vs Business Births vs Business Deaths",
    scene = list(
      xaxis = list(title = "Year"),
      yaxis = list(title = "Business Births"),
      zaxis = list(title = "Business Deaths")
    ))
    
