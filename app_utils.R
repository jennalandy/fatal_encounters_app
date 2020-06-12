# Constants ####

teal <- '#008080'
light_grey <- '#808080'

## Legislation http://campaign-zero-cartogram.s3.amazonaws.com/index.html ####
legislation <- rbind(
  # Washington
  data.frame(
    "bill" = c("HB2908","I-940","HB2362","SB5311"),
    "state" = rep("WA", 4),
    "category" = c(
      "Limit Use of Force",
      "Independently Investigate & Prosecute",
      "Body Cams/ Film the Police",
      "Training"
    )
  ),
  # Oregon
  data.frame(
    "bill" = c("HB2002","HB4003","SB5701","SB2704"),
    "state" = rep("OR", 4),
    "category" = c(
      "End Broken Windows Policing",
      "End Broken Windows Policing",
      "Limit Use of Force",
      "Body Cams/ Film the Police"
    )
  ),
  # California
  data.frame(
    "bill" = c("AB853","SB54","AB71","SB1421","AB392","SB277","AB256","SB89","AB748","SB11","SB29"),
    "state" = rep("CA", 11),
    "category" = c(
      "End Broken Windows Policing",
      "End Broken Windows Policing",
      "Limit Use of Force",
      "Limit Use of Force",
      "Limit Use of Force",
      "Independently Investigate & Prosecute",
      "Body Cams/ Film the Police",
      "Body Cams/ Film the Police",
      "Body Cams/ Film the Police",
      "Training",
      "Training"
    )
  )
)


## Racial Distribution from https://www.census.gov/quickfacts/fact/table/US/IPE120218 ####
race_props_list <- list(
  "African-American/Black" = 0.134,
  "European-American/White" = 0.604,
  "Hispanic/Latino" = 0.183,
  "Asian/Pacific Islander" = 0.059 + 0.002,
  "Native American/Alaskan" = 0.013
)
race_props <- data.frame(t(data.frame(race_props_list))) %>%
  rownames_to_column()
colnames(race_props) <- c("race", "prop_race")
race_props$race <- names(race_props_list)

## Gender Distribution from https://www.census.gov/quickfacts/fact/table/US/IPE120218 ####
# Transgender proportion from https://en.wikipedia.org/wiki/LGBT_demographics_of_the_United_States#:~:text=A%20different%20survey%20in%202016,adult%20population%20identifying%20as%20LGBT.
gender_props_list <- list(
  "Female" = 0.508,
  "Male" = 0.492,
  "Transgender" = 0.068
)
gender_props <- data.frame(t(data.frame(gender_props_list))) %>%
  rownames_to_column()
colnames(gender_props) <- c("gender", "prop_gender")

# Functionality Functions ####

load_data <- function() {
  filename <- "FATAL ENCOUNTERS DOT ORG SPREADSHEET (See Read me tab).xlsx"
  data <- read_excel(filename, sheet = 1) %>%
    clean_names() %>%
    dplyr::select(
      -subjects_race,
      -imputation_probability,
      -full_address,
      -video,
      -unique_id_formula,
      -unique_identifier_redundant
    )
  names(data)[names(data) == 'date_of_injury_resulting_in_death_month_day_year'] <- 'date'
  data$day <- data$date
  data$month <- as.Date(paste('01-', format(data$date, '%m-%Y'), sep = ''), format = '%d-%m-%Y')
  data$year <- as.Date(paste('01-01-', year(data$date), sep = ''), format = '%d-%m-%Y')
  
  names(data)[names(data) == 'subjects_race_with_imputations'] <- 'race'
  data$race[data$race == 'NA'] <- 'Race unspecified'
  data$race[data$race == 'HIspanic/Latino'] <- 'Hispanic/Latino'
  data$race[data$race == 'European American/White'] <- 'European-American/White'
  data$race[data$race == 'Other Race' | 
            data$race == 'Race unspecified' |
            data$race == 'Middle Eastern'] <- 'Other or Unknown'
  
  names(data)[names(data) == 'subjects_gender'] <- 'gender'
  data$gender[is.na(data$gender)] <- 'Unknown'
  data$gender[data$gender == 'White'] <- 'Unknown'
  data$gender[data$gender == 'Transexual'] <- 'Transgender'
  #“Transsexual” is a more specific term that fits under the transgender umbrella.
  
  data$cause_of_death[data$cause_of_death == 'Undetermined'] <- 'Unknown'
  data$cause_of_death[is.na(data$cause_of_death)] <- 'Unknown'
  
  stop_idx <- which(startsWith(data$subjects_name, prefix = 'Items below this row'))
  data <- data[1:(stop_idx-1),]
  
  # filter out suicides and criminal acts
  data <- data %>%
    filter(
      !grepl(
        "suicide|drug overdose|criminal|medical emergency|drowned",
        tolower(dispositions_exclusions_internal_use_not_for_analysis)
      ) &
      !grepl(
        "suicide|drug overdose|criminal|medical emergency|drowned",
        tolower(cause_of_death)
      )
    )
  
  data
} 

load_pop_data <- function() {
  filename <- "FATAL ENCOUNTERS DOT ORG SPREADSHEET (See Read me tab).xlsx"
  pop_data <- read_excel(filename, sheet = 2) %>%
    clean_names() %>%
    dplyr::select(
      -x3, -x8, -x9
    )
  pop_data$name[pop_data$name == 'Washington (State)'] <- 'Washington'
  pop_data
}

totitle <- function(string) {
  paste(
    toupper(str_sub(string, 1, 1)), 
    str_sub(string, 2, nchar(string)), 
    sep = ''
  )
}


# Display Functions ####
plot_over_time <- function(
  data, pop_data, time_unit, by,
  min_date = min(data$date),
  max_date = max(data$date)
) {
  total_pop <- pop_data %>%
    select(population_2015_est, population_2010, population_2005, population_2000) %>%
    colSums()
  
  if (by != 'none') {
    if (by == 'race') {
      over_time <- data %>%
        filter(date > min_date, date < max_date) %>%
        group_by(!!sym(time_unit), !!sym(by)) %>%
        summarize(n = length(unique_id)) %>%
        merge(race_props, by = 'race') %>%
        mutate(
          prop = case_when(
            year(!!sym(time_unit)) < 2002 ~ n/(prop_race*total_pop['population_2000']),
            year(!!sym(time_unit)) >= 2002 & year(!!sym(time_unit)) < 2007 ~ n/(prop_race*total_pop['population_2005']),
            year(!!sym(time_unit)) >= 2007 & year(!!sym(time_unit)) < 2012 ~ n/(prop_race*total_pop['population_2010']),
            year(!!sym(time_unit)) >= 2012 ~ n/(prop_race*total_pop['population_2015_est'])
          ),
          n_per_100k = prop*100000
        ) %>% 
        complete(!!sym(time_unit), race, fill = list(n_per_100k = 0))
      
    } else if (by == 'gender') {
      over_time <- data %>%
        filter(date > min_date, date < max_date) %>%
        group_by(!!sym(time_unit), !!sym(by)) %>%
        summarize(n = length(unique_id)) %>%
        merge(gender_props, by = 'gender') %>%
        mutate(
          prop = case_when(
            year(!!sym(time_unit)) < 2002 ~ n/(prop_gender*total_pop['population_2000']),
            year(!!sym(time_unit)) >= 2002 & year(!!sym(time_unit)) < 2007 ~ n/(prop_gender*total_pop['population_2005']),
            year(!!sym(time_unit)) >= 2007 & year(!!sym(time_unit)) < 2012 ~ n/(prop_gender*total_pop['population_2010']),
            year(!!sym(time_unit)) >= 2012 ~ n/(prop_gender*total_pop['population_2015_est'])
          ),
          n_per_100k = prop*100000
        ) %>% 
        complete(!!sym(time_unit), gender, fill = list(n_per_100k = 0))
    }
    
    p <- ggplot(over_time, aes(x = !!sym(time_unit), y = n_per_100k, color = !!sym(by))) +
      geom_line(size = 1, alpha = 0.8) +
      labs(color = totitle(by))
    
  } else {
    over_time <- data %>%
      filter(date > min_date, date < max_date) %>%
      group_by(!!sym(time_unit)) %>%
      summarize(n = length(unique_id)) %>%
      mutate(
        prop = case_when(
          year(!!sym(time_unit)) < 2002 ~ n/(total_pop['population_2000']),
          year(!!sym(time_unit)) >= 2002 & year(!!sym(time_unit)) < 2007 ~ n/(total_pop['population_2005']),
          year(!!sym(time_unit)) >= 2007 & year(!!sym(time_unit)) < 2012 ~ n/(total_pop['population_2010']),
          year(!!sym(time_unit)) >= 2012 ~ n/(total_pop['population_2015_est'])
        ),
        n_per_100k = prop*100000
      ) %>% 
      complete(!!sym(time_unit), fill = list(n_per_100k = 0))
    
    p <- ggplot(over_time, aes(x = !!sym(time_unit), y = n_per_100k)) +
      geom_line(
        color = teal,
        size = 1,
        alpha = 0.8
      ) 
  }
  
  p +
    theme_classic() +
    labs(
      x = time_unit %>% 
        substitute() %>% 
        deparse() %>% 
        totitle(),
      y = "Number of Deaths per 100k",
      title = "Deaths Through Police Interactions Over Time"
    ) +
    theme(
      plot.title.position = 'plot'
    )
}

write_over_time <- function(data, by) {
  all_lm_data <- data %>%
    group_by(date_year) %>%
    summarize(n = length(unique_id)) %>%
    filter(date_year < 2020)
  
  all_CI <- lm(n~date_year, all_lm_data) %>% confint()
  
  paste(
    "We are 95% confident that the number of deaths through police interactions",
    "increases by between", round(all_CI[2,1], 1), "and", round(all_CI[2,2], 1), "each year." 
  )
}

write_props <- function(data, by) {
  counts <- data %>%
    group_by(!!sym(by)) %>%
    summarize(n = length(unique_id))
  
  total <- data %>% nrow()
  
  counts <- counts %>%
    mutate(
      prop = n/total
    )
  
  if (by == 'race') {
    test = prop.test(
      x = counts$n[counts$race == 'African-American/Black'][1],
      n = total,
      p = race_props$prop_race[race_props == 'African-American/Black'][1]
    )
    
    prop_text <- paste(
      "African-American/Black individuals make up ",
      round(100*race_props$prop_race[race_props == 'African-American/Black'][1], 1),
      "% of the US population, but this racial group makes up ",
      round(100*counts$prop[counts$race == 'African-American/Black'][1], 1),
      "% of individuals killed in police interactions.", sep = ''
    )
  } else if (by == 'gender') {
    test = prop.test(
      x = counts$n[counts$gender == 'Male'][1],
      n = total,
      p = gender_props$prop_gender[gender_props == 'Male'][1]
    )
    
    prop_text <- paste(
      "Male-identifying individuals mae up ",
      round(100*gender_props$prop_gender[gender_props == 'Male'][1], 1),
      "% of the US population, but this group makes up ",
      round(100*counts$prop[counts$gender == 'Male'][1], 1),
      "% of individuals killed in police interactions.", sep = ''
    )
  }
  
  if (test$p.value < 0.05) {
    test_text <- paste(
      "This difference is significant at the 5% significance level (p-value = ",
      round(test$p.value, 3), ").", sep = ''
    )
  } else {
    test_text <- paste(
      "This difference is not significant at the 5% significance level (p-value = ",
      round(test$p.value, 3), ").", sep = ''
    )
  }
  
  paste(prop_text, test_text)
}

plot_props <- function(data, by) {
  counts <- data %>%
    group_by(!!sym(by)) %>%
    summarize(n = length(unique_id))
  
  total <- data %>% nrow()
  
  counts <- counts %>%
    mutate(
      prop = n/total
    )
  
  plot_data <- list(
    "by" = c(),
    "prop_of_pop" = c(),
    "prop_of_killed" = c(),
    "n_killed" = c()
  )
  if (by == 'gender') {
    by_props <- gender_props
    bys <- gender_props$gender
  } else if (by == 'race') {
    by_props <- race_props
    bys <- race_props$race
  }
  
  prop_col <- names(by_props)[grepl('^prop_', names(by_props))]
  
  for (b in bys) {
    plot_data[['by']] <- c(plot_data[['by']], b)
    plot_data[['prop_of_pop']] <- c(plot_data[['prop_of_pop']], by_props[[prop_col]][by_props == b])
    plot_data[['prop_of_killed']] <- c(plot_data[['prop_of_killed']], counts$prop[counts[[by]] == b])
    plot_data[['n_killed']] <- c(plot_data[['n_killed']], counts$n[counts[[by]] == b])
  }
  
  if (by == 'gender') {
    titlex <- 0.3
    titley <- 1.03
    t1 <- textGrob(expression(
      "Distribution of Gender in " * 
        phantom(bold("US Population")) * " and " * 
        phantom(bold("Individuals Killed"))
    ), x = titlex, y = titley, gp = gpar(col = "black")
    )
    t2 <- textGrob(expression(
      phantom("Distribution of Gender in ") * 
        bold("US Population") * phantom(" and ") * 
        phantom(bold("Individuals Killed"))
    ), x = titlex, y = titley, gp = gpar(col = "#d6afb3")
    )
    t3 <- textGrob(expression(
      phantom("Distribution of Gender in ") * 
        phantom(bold("US Population")) * phantom(" and ") * 
        bold("Individuals Killed")
    ), x = titlex, y = titley, gp = gpar(col = "#82222d")
    )
  } else if (by == 'race') {
    titlex <- 0.27
    titley <- 1.03
    t1 <- textGrob(expression(
      "Distribution of Racial Groups in " * 
        phantom(bold("US Population")) * " and " * 
        phantom(bold("Individuals Killed"))
    ), x = titlex, y = titley, gp = gpar(col = "black")
    )
    t2 <- textGrob(expression(
      phantom("Distribution of Racial Groups in ") * 
        bold("US Population") * phantom(" and ") * 
        phantom(bold("Individuals Killed"))
    ), x = titlex, y = titley, gp = gpar(col = "#d6afb3")
    )
    t3 <- textGrob(expression(
      phantom("Distribution of Racial Groups in ") * 
        phantom(bold("US Population")) * phantom(" and ") * 
        bold("Individuals Killed")
    ), x = titlex, y = titley, gp = gpar(col = "#82222d")
    )
  }
  
  data.frame(plot_data) %>%
    pivot_longer(cols = c('prop_of_pop','prop_of_killed')) %>%
    ggplot(aes(by, x = value, fill = name, group = name)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    annotation_custom(grobTree(t1, t2, t3)) +
    coord_cartesian(clip = "off") +
    theme_classic() +
    labs(
      x = "",
      y = "",
      title = "",
      fill = ''
    ) +
    scale_fill_manual(
      breaks = c('prop_of_pop', 'prop_of_killed'),
      values = c('#d6afb3', '#82222d')
    ) +
    theme(
      plot.title.position = 'plot',
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.line.y = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(
        color = light_grey
      ),
      axis.text.y = element_text(
        hjust = 1,
        vjust = 0,
        face = 'bold',
        size = 9,
        margin = margin(r = -10)
      ),
      panel.grid.major.x = element_line(
        color = light_grey,
        size = 0.1
      ),
      legend.position = 'none'
    ) 
}

plot_states <- function(data, pop_data, in_year) {
  all_data <- data
  abbr_to_name <- read.csv('abbr-name.csv', header = FALSE)
  names(abbr_to_name) <- c('abbr','name')
  
  
  data <- data %>%
    filter(year(date) == in_year)
  
  states <- geojson_read( 
    x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json",
    what = "sp"
  )
  
  data_by_state <- data %>%
    group_by(location_of_death_state) %>%
    summarize(
      n = length(unique_id)
    ) %>%
    merge(abbr_to_name, by.x = 'location_of_death_state', by.y = 'abbr') %>%
    merge(pop_data, by = 'name') %>%
    mutate(
      prop = case_when(
        in_year < 2003 ~ n/population_2000,
        (in_year >= 2003 & in_year < 2008) ~ n/population_2005,
        (in_year >= 2008 & in_year < 2013) ~ n/population_2010,
        in_year >= 2013 ~ n/population_2015_est
      ),
      n_per_100k = prop*100000
    ) %>% 
    complete(name, fill = list(n_per_100k = 0))
  
  data_by_state <- data_by_state[data_by_state$name %in% states$name,]
  data_by_state <- data_by_state[order(data_by_state$name),]
  
  states <- states[states$name %in% data_by_state$name,]
  states <- states[order(states$name),]
  
  labels <- paste(data_by_state$name, ": ", round(data_by_state$n_per_100k,3), " deaths per 100k", sep = '')
  
  states$n_per_100k <- data_by_state$n_per_100k
  
  
  max_n <- all_data %>%
    group_by(location_of_death_state, date_year) %>%
    summarize(
      n = length(unique_id)
    ) %>%
    merge(abbr_to_name, by.x = 'location_of_death_state', by.y = 'abbr') %>%
    merge(pop_data, by = 'name') %>%
    mutate(
      prop = case_when(
        in_year < 2003 ~ n/population_2000,
        (in_year >= 2003 & in_year < 2008) ~ n/population_2005,
        (in_year >= 2008 & in_year < 2013) ~ n/population_2010,
        in_year >= 2013 ~ n/population_2015_est
      ),
      n_per_100k = prop*100000
    ) %>%
    pull(n_per_100k) %>%
    max(na.rm = TRUE)
  
  bins <- seq(0, ceiling(max_n*100)/100, length.out = 10)
  pal <- colorBin("YlOrRd", domain = states$n_per_100k, bins = bins)
  
  if (in_year == 2020) {
    rr <- tags$div(
      HTML(paste('<h3>2020 (so far)</h3>', sep = ''))
    )  
  } else {
    rr <- tags$div(
      HTML(paste('<h3>', in_year, '</h3>', sep = ''))
    )  
  }
  
  
  map <- leaflet(states) %>%
    addTiles() %>%
    setView(-98, 37.8, 3) %>%
    addPolygons(
      fillColor = ~pal(n_per_100k),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal", 
          padding = "3px 8px"
        ),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addControl(rr, position = "bottomleft")
  
  
  max_all_n <- all_data %>%
    group_by(date_year) %>%
    summarize(n = length(unique_id)) %>%
    pull(n) %>%
    max()
  
  min_all_n <- all_data %>%
    group_by(date_year) %>%
    filter(date_year != 2020) %>%
    summarize(n = length(unique_id)) %>%
    pull(n) %>%
    min()
  
  line <- all_data %>%
    filter(date_year <= in_year) %>%
    group_by(date_year) %>%
    summarize(n = length(unique_id)) %>%
    ggplot(aes(x = date_year, y = n)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    labs(
      x = '',
      y = ''
    ) +
    xlim(c(
      min(all_data$date_year, na.rm = T), 
      max(all_data$date_year, na.rm = T)
    )) + 
    ylim(c(
      min_all_n-10, max_all_n+10
    ))
  
  out <- list()
  out[['map']] = map
  out[['line']] = line
  out
}





