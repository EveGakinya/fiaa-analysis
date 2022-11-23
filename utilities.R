#---------------------------------------------
# Renaming / to .
#---------------------------------------------

rename_vars <- function(df){
  names(df) <- c(gsub("/",".",names(df)))
  
  return(df)
  
}
#---------------------------------------------
# calculate the average of integer question
#---------------------------------------------
integer_questions_mean <- function(question.name, group.name, calculation.type) {

  result <- data.all %>% 
    dplyr::select(Community = Q1_6_Select_community, !!sym(question.name)) %>% 
    dplyr::group_by(Community) %>% 
    dplyr::summarise_at(vars(!!sym(question.name)),# variable to be evaluated. 
                 list(Value = ~ifelse(sum(!is.na(as.numeric(.)))==0, NA, mean(as.numeric(.), na.rm=T)))) %>%
    dplyr::mutate(`Group` = group.name, .before = Community,
           `Indicator` = question.name,
           `Calculation` = calculation.type) %>%
    dplyr::mutate( `Choice` = "",
            `Function` = "", .after = Community) %>%
    dplyr::mutate_at(c("Value", "Choice"), as.character)
  
  return(result)
}

#---------------------------------------------
# calculate the median of integer question
#---------------------------------------------
integer_questions_median <- function(question.name, group.name, calculation.type) {
  
  result <- data.all %>% 
    dplyr::select(Community = Q1_6_Select_community, !!sym(question.name)) %>% 
    dplyr::group_by(Community) %>% 
    dplyr::summarise_at(vars(!!sym(question.name)), 
                 list(Value = ~ifelse(sum(!is.na(as.numeric(.)))==0, NA, median(as.numeric(.), na.rm=T)))) %>%
    dplyr::mutate(`Group` = group.name, .before = Community,
           `Indicator` = question.name,
           `Calculation` = calculation.type) %>%
    dplyr::mutate( `Choice` = "",
            `Function` = "", .after = Community) %>%
    dplyr::mutate_at(c("Value", "Choice"), as.character)
  
  return(result)
}
#---------------------------------------------
# calculate the median of integer question
#---------------------------------------------
integer_questions_median_no_agg <- function(question.name) {
  
  result <- data.all %>% 
    dplyr::select(Choice = !!sym(question.name)) %>% 
    dplyr::summarise("Value" = median(Choice, na.rm = TRUE)) %>%
    dplyr::mutate(`Indicator` = question.name) %>%
    dplyr::mutate( `Choice` = "",
                   `Function` = "") %>%
    dplyr::mutate_at(c("Value", "Choice"), as.character)
  
  return(result)
}
#---------------------------------------------
# select_one function to get the number and the percentages of choices of select_one questions
#---------------------------------------------
select_one_questions <- function(question.name, group.name, calculation.type) {

  result <- data.all  %>% 
    dplyr::mutate(Community = Q1_6_Select_community,
           Choice = !!sym(question.name)) %>% 
    dplyr::group_by(Community, Choice) %>%
    dplyr::summarise(counts = n()) %>% 
    na.omit() %>% 
    dplyr::mutate(percentage = round(counts/sum(counts)*100, 2)) %>%
    dplyr::mutate(`Group` = group.name, .before = Community,
           `Indicator` = question.name,
           `Calculation` = calculation.type) %>% 
    pivot_longer(cols = c("counts", "percentage"), names_to = "Function", values_to = "Value") %>% 
    dplyr::mutate_at(c("Value", "Choice"), as.character)
  
  return(result)
}
# select_one function to get the number and the percentages of choices of select_one questions
#---------------------------------------------
select_one_questions_agg <- function(question.name, group.name, calculation.type) {
  
  result <- data.all  %>% 
    dplyr::mutate(Sub_disctrict = Q1_5_select_sub_district,
                  Choice = !!sym(question.name)) %>% 
    dplyr::group_by(Sub_disctrict, Choice) %>%
    dplyr::summarise(counts = n()) %>% 
    na.omit() %>% 
    dplyr::mutate(percentage = round(counts/sum(counts)*100, 2)) %>%
    dplyr::mutate(`Group` = group.name, .before = Sub_disctrict,
                  `Indicator` = question.name,
                  `Calculation` = calculation.type) %>% 
    pivot_longer(cols = c("counts", "percentage"), names_to = "Function", values_to = "Value") %>% 
    dplyr::mutate_at(c("Value", "Choice"), as.character)
  
  return(result)
}
#---------------------------------------------
# select_one function without aggregation
#---------------------------------------------
select_one_questions_no_agg <- function(question.name) {
  
  result <- data.all  %>% 
    dplyr::mutate(Choice = !!sym(question.name)) %>% 
    dplyr::group_by(Choice) %>%
    dplyr::summarise(counts = n()) %>% 
    na.omit() %>% 
    dplyr::mutate(percentage = round(counts/sum(counts)*100, 2)) %>%
    dplyr::mutate(`Indicator` = question.name,) %>% 
    pivot_longer(cols = c("counts", "percentage"), names_to = "Function", values_to = "Value") %>% 
    dplyr::mutate_at(c("Value", "Choice"), as.character)
  
  return(result)
}

#---------------------------------------------
# select_multiple function to get the number and the percentages of choices of select_multiple questions
#---------------------------------------------
select_multiple_questions <- function(question.name, group.name, calculation.type) {

  table.name <- data.all %>% 
    dplyr::select(c(Community = Q1_6_Select_community, colnames(data.all)[grepl(question.name, colnames(data.all), fixed=T)])) %>%
    dplyr::filter(!is.na(!!sym(question.name))) %>% 
    dplyr::select_if(~all(!is.na(.))) %>%
    dplyr::select(-question.name)
  
  colnames(table.name)[2:ncol(table.name)] <- lapply(colnames(table.name)[2:ncol(table.name)], function(x) str_split(x, "[.]")[[1]][2])
  
  result <- table.name %>% 
    pivot_longer(!Community, names_to="Choice", values_to="Val") %>% 
    dplyr::group_by(Community, Choice, .drop=F) %>% 
    dplyr::summarise(counts = sum(as.numeric(Val), na.rm = T),
              percentage= round(sum(as.numeric(Val), na.rm = T)/n()*100, 2)) %>%
    dplyr::ungroup() %>% 
    dplyr::filter(counts>0) %>% 
    dplyr::mutate(`Group` = group.name, .before = Community,
           `Indicator` = question.name,
           `Calculation` = calculation.type) %>% 
    pivot_longer(cols = c("counts", "percentage"), names_to = "Function", values_to = "Value") %>%
    dplyr::mutate_at(c("Value", "Choice"), as.character)
  
  return(result)
}

#---------------------------------------------
# select_one function without aggregation
#---------------------------------------------
select_multiple_questions_no_agg <- function(question.name) {
  
  table.name <- data.all %>% 
    dplyr::select(c(Community = Q1_6_Select_community, colnames(data.all)[grepl(question.name, colnames(data.all), fixed=T)])) %>%
    dplyr::filter(!is.na(!!sym(question.name))) %>% 
    dplyr::select_if(~all(!is.na(.))) %>%
    dplyr::select(-question.name)
  
  colnames(table.name)[2:ncol(table.name)] <- lapply(colnames(table.name)[2:ncol(table.name)], function(x) str_split(x, "[.]")[[1]][2])
  
  result <- table.name %>% 
    pivot_longer(!Community, names_to="Choice", values_to="Val") %>% 
    dplyr::group_by(Choice, .drop=F) %>% 
    dplyr::summarise(counts = sum(as.numeric(Val), na.rm = T),
              percentage= round(sum(as.numeric(Val), na.rm = T)/n()*100, 2)) %>%
    dplyr::ungroup() %>% 
    dplyr::filter(counts>0) %>% 
    dplyr::mutate(`Indicator` = question.name) %>% 
    pivot_longer(cols = c("counts", "percentage"), names_to = "Function", values_to = "Value") %>%
    dplyr::mutate_at(c("Value", "Choice"), as.character)
  
  return(result)
}

#---------------------------------------------
#column.is.empty function used when question has no answers
#---------------------------------------------
column.is.empty <- function(question.name, group.name, calculation.type){
  result.test <- as_tibble (data.frame(
    `Group` = group.name,
    `Indicator` = question.name,
    `Calculation` = calculation.type,
    `Community` = "",
    `Choice` = "",
    `Function` = "",
    `Value` = "Question had 0 Entries, No Data to use for Analysis"))
}

#---------------------------------------------
# Pie Chart With Aggregation
#---------------------------------------------
pie.agg <- function(question.name, graph.data, label){
  chart.table <- graph.data %>%
    dplyr::filter(Indicator == question.name & Function == "percentage")
  
  chart.table$Value <- as.numeric(as.vector(chart.table$Value))
  legend.label <- as.character(label)
  
  pie <- ggplot(chart.table, aes(x="", y=Value, fill= Choice)) +
    geom_bar(stat = "identity") +
    coord_polar("y") +
    facet_grid(.~Community) +
    labs(title= label, x = "", y = "") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid  = element_blank()) +
    geom_label(aes(label = label),
               color = "white",
               position = position_stack(vjust = 0.25),
               show.legend = FALSE) +
    theme_classic() +
    scale_fill_manual(legend.label, values =  c("#58585A", "#EE5859", "#F69E61", "#FFF67A", "#D2CBB8", "#A5C9A1", "#56B3CD", "#56B3CD", "#95A0A9"))
    
}

#---------------------------------------------
# Pie Chart Without Aggregation
#---------------------------------------------
pie.no.agg <- function(question.name, graph.data, label){
  chart.table <- graph.data %>%
    dplyr::filter(Indicator == question.name & Function == "percentage")
  
  chart.table$Value <- as.numeric(as.vector(chart.table$Value))
  legend.label <- as.character(label)
  
  pie <- ggplot(chart.table, aes(x="", y=Value, fill= Choice)) +
    geom_bar(stat = "identity") +
    coord_polar("y") +
    labs(title= label, x = "", y = "") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid  = element_blank()) +
    geom_label(aes(label = Value),
               color = "white",
               position = position_stack(vjust = 0.25),
               show.legend = FALSE) +
    theme_classic() +
    scale_fill_manual(legend.label, values =  c("#58585A", "#EE5859", "#F69E61", "#FFF67A", "#D2CBB8", "#A5C9A1", "#56B3CD", "#56B3CD", "#95A0A9"))
  
}

#---------------------------------------------
# Bar Chart With Aggregation
#---------------------------------------------
bar.agg <- function(question.name, graph.data, label){
  chart.table <- graph.data %>%
    dplyr::filter(Indicator == question.name & Function == "percentage")
  
  # chart.table <- graph.data %>%
  #   dplyr::filter(Indicator == question.name & Function == "counts")
  # 
  chart.table$Value <- as.numeric(as.vector(chart.table$Value))
  legend.label <- as.character(label)
  
  bar <- ggplot(chart.table, aes(x="", y=Value, fill= Choice)) +
    geom_bar(stat = "identity", position=position_dodge()) +
    facet_grid(.~Community) +
    labs(title= label, x = "", y = "") +
    geom_label(aes(label = Value),
               color = "white",
               position = position_dodge(0.9),
               show.legend = FALSE) +
    theme_classic() +
    scale_fill_manual(legend.label, values =  c("#58585A", "#EE5859", "#F69E61", "#FFF67A", "#D2CBB8", "#A5C9A1", "#56B3CD", "#56B3CD", "#95A0A9"))
}

#---------------------------------------------
# Bar Chart Without Aggregation
#---------------------------------------------
bar.no.agg <- function(question.name, graph.data, label){
  
  chart.table <- graph.data %>%
    dplyr::filter(Indicator == question.name & Function == "percentage")
  
  chart.table$Value <- as.numeric(as.vector(chart.table$Value))
  legend.label <- as.character(label)
  
  bar <- ggplot(chart.table, aes(x="", y=Value, fill= Choice)) +
    geom_bar(stat = "identity", position=position_dodge()) +
    labs(title= label, x = "", y = "") +
    geom_label(aes(label = Value),
               color = "white",
               position = position_dodge(0.9),
               show.legend = FALSE) +
    theme_classic() +
    scale_fill_manual(legend.label, values =  c("#58585A", "#EE5859", "#F69E61", "#FFF67A", "#D2CBB8", "#A5C9A1", "#56B3CD", "#56B3CD", "#95A0A9"))
  
}

#---------------------------------------------
# Column Chart with Aggregation
#---------------------------------------------
col.agg <- function(question.name, graph.data, label){
  
  chart.table <- graph.data %>%
    dplyr::filter(Indicator == question.name & Function == "percentage")
  
  chart.table <- graph.data %>%
    dplyr::filter(Indicator == question.name & Function == "counts")
  
  chart.table$Value <- as.numeric(as.vector(chart.table$Value))
  legend.label <- as.character(label)
  col <- ggplot(chart.table, aes(x="", y=Value, fill= Choice)) +
    geom_col(position=position_dodge()) +
    facet_grid(.~Community) +
    labs(title= label, x = "", y = "") +
    geom_label(aes(label = Value),
               color = "white",
               position = position_dodge(0.9),
               show.legend = FALSE) + 
    theme_classic() +
    scale_fill_manual(legend.label, values =  c("#58585A", "#EE5859", "#F69E61", "#FFF67A", "#D2CBB8", "#A5C9A1", "#56B3CD", "#56B3CD", "#95A0A9"))
  
  
}

#---------------------------------------------
# Column Chart without Aggregation
#---------------------------------------------
col.no.agg <- function(question.name, graph.data, label){
  
  chart.table <- graph.data %>%
    dplyr::filter(Indicator == question.name & Function == "counts")
  
  chart.table$Value <- as.numeric(as.vector(chart.table$Value))
  legend.label <- as.character(label)
  col <- ggplot(chart.table, aes(x="", y=Value, fill= Choice)) +
    geom_col( position=position_dodge()) +
    labs(title= label, x = "", y = "") +
    geom_label(aes(label = Value),
               color = "white",
               position = position_dodge(0.9),
               show.legend = FALSE,
               legend = legend.label) + 
    theme_classic() +
    scale_fill_manual(legend.label, values =  c("#58585A", "#EE5859", "#F69E61", "#FFF67A", "#D2CBB8", "#A5C9A1", "#56B3CD", "#56B3CD", "#95A0A9"))
  
}

#End