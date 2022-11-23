##############################################
#######Food Items Assessment Analysis#######
##############################################
#---------------------------------------------
# Prepare Environment
#---------------------------------------------

rm(list=ls())

this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, writexl, openxlsx, excel.link, ggplot2)

#Load functions & items groups ###################################
source("./utilities.R")
source("./items_groups.R")

#Read output of Cleaning Script ###################################
#ATTENTION!!!!
#use first line if the file is password protected, if not use the second line. (ALWAYS COMMENT ONE OF THE LINES!)
#ALWAYS REMOVE PASSWORD FROM THE CODE AFTER READING THE DATA!

# data.all <- xl.read.file("input/full_data_cleaned.xlsx", password = "SCA_KI_2022_Syria")
data.all <- read.xlsx("input/final_data_latest.xlsx")


# rename columns - change "/" to "." ###################################

data.all <- rename_vars(data.all)

#Read Analysis plan ###################################
analysis.plan <- read.xlsx("input/FIAA_Analysis Plan.xlsx", sheet = "Sheet1")
analysis.for.charts <- read.xlsx("input/FIAA_Analysis Plan.xlsx", sheet = "no.agg")
analysis.graphs.agg <- read.xlsx("input/FIAA_Analysis Plan.xlsx", sheet = "Sheet2")
graphs <- read.xlsx("input/FIAA_Analysis Plan.xlsx", sheet = "Graphs")


# Prices and Items Analysis ###################################

# PART 1:  Price analysis (Medians, Min, Max) of Items ###################################

#ATTENTION!!!! 
#update Exchange rate file based on numbers from MM Round.
exchange.rates <- read.xlsx("input/Exchange_rates.xlsx")

items.list <- names(data.all %>% dplyr::select(starts_with("Calc_")))

items.list <- items.list[3:41]
# calculate medians of exchange rates: TRY-SYP & USD-SYP for each community
currency.calc <- data.all %>% 
  dplyr::select(uuid, Q1_6_Select_community, Q5_10_What_is_the_main_currency_used_at_your_shop, items.list) %>%
  dplyr::mutate(exchange_rate_medians_sell = as.numeric(0))

#Check that all communities in the data have exchange rates listed in the Exchange rates file.
if(!all(currency.calc$Q1_6_Select_community %in% exchange.rates$q_town))stop("some communites are not in the exchange rate list. check Exchange rate file and/or data file.")

for (i in 1:nrow(exchange.rates)){
  
  community <- as.character(exchange.rates[i, "q_town"])
  currency <- as.character(exchange.rates[i, "sell_rate_of"])
  value <- exchange.rates[i, "value"]
  
  currency.calc$exchange_rate_medians_sell[community ==currency.calc$Q1_6_Select_community & 
                                             currency == currency.calc$Q5_10_What_is_the_main_currency_used_at_your_shop] <-  value
}
#Check that all exchange_rate_medians_sell are != 0
if(any(currency.calc$exchange_rate_medians_sell == 0))stop("Exchange rate is 0!. check Exchange rate file and/or data file.")

# apply exchange rate to all prices columns 
prices.edited <- lapply(items.list, function(col.price){
  # convert prices
  output <- currency.calc %>%
    dplyr::mutate(!!(col.price) := ifelse(Q5_10_What_is_the_main_currency_used_at_your_shop %in% c("Turkish_lira", "US_dollar"),
                                          as.numeric(!!sym(col.price)) * exchange_rate_medians_sell,
                                          !!sym(col.price))) %>%
    dplyr::select(uuid, Q1_6_Select_community, Q5_10_What_is_the_main_currency_used_at_your_shop, !!sym(col.price))
  return(output)
}) 
list.prices.syp <- prices.edited %>% 
  reduce(full_join, by=c("uuid", "Q1_6_Select_community", "Q5_10_What_is_the_main_currency_used_at_your_shop")) %>% 
  dplyr::select(-Q5_10_What_is_the_main_currency_used_at_your_shop)

summary.community.level <- as.data.frame(list.prices.syp) %>% 
                          dplyr::group_by(Q1_6_Select_community) %>%
                          summarise(across(
                            .cols = where(is.numeric),
                            .fns = list("median" = median, "max" = max, "min" = min),
                            na.rm = T))
  
summary.overall.level <- as.data.frame(list.prices.syp) %>% 
  summarise(across(
    .cols = where(is.numeric),
    .fns = list("median" = median, "max" = max, "min" = min),
    na.rm = T)) %>% 
  add_column(Q1_6_Select_community = "Overall", .before = "Calc_Flour_median")


community.level.price.analysis <- rbind(summary.overall.level,summary.community.level)


write.csv(list.prices.syp, paste0("./output/tables/prices and medians/food_items_list_prices_syp.csv"), row.names=FALSE)
write.csv(community.level.price.analysis, paste0("./output/tables/prices and medians/food_items_items_price_analysis.csv"), row.names=FALSE)


# PART 2: Items  core questions analysis ###################################

tokeep <- c("Q6_1","Q6_5","Q6_7", "Q6_8", "Q6_9", "Q6_10","Q6_11", "Q6_12", "Q6_13", "Q6_14", "Q6_15", "Q6_16", "Q6_17")

data.prep <- data.all[,colSums(is.na(data.all))<nrow(data.all)]
core.items.list <- as.data.frame(colnames(data.prep)[grepl(paste(tokeep, collapse="|"),colnames(data.prep))])

colnames(core.items.list) <- "Col.name"
core.items.list <- core.items.list %>%
  mutate(Group = "Core Questions - repeate for each item", .before = Col.name) %>%
  mutate(Function = case_when(startsWith(Col.name, "Q6_7_") ~ "integer",
                              startsWith(Col.name, "Q6_9_") ~ "integer",
                              startsWith(Col.name, "Q6_11_") ~ "select_multiple",
                              TRUE ~ "select_one"),
         Calculation = case_when(Function == "integer" ~ "Median",
                                 TRUE ~ "count and percentage"))

core.items.list <- core.items.list %>% 
                    filter(!str_detect(Col.name, "Turkey|Egypt|Iraq|Jordan|Morocco|Lebanon|UAE|Saudi_Arabia|other"))
  
# PART 2A: Items  core questions analysis at community level ###################################  
result.items.community <- lapply(1:nrow(core.items.list), function(r){
  
  # core.items.list input parameters
  function.name <- as.character(core.items.list[r, "Function"])
  question.name <- as.character(core.items.list[r, "Col.name"])
  calculation.type <- as.character(core.items.list[r, "Calculation"])
  group.name <- as.character(core.items.list[r, "Group"])
  
  if(all(is.na(data.all[,question.name]))) {
    
    output <- column.is.empty(question.name, group.name, calculation.type)
    
  }else if(function.name == "select_one"){
    
    output <- select_one_questions(question.name, group.name, calculation.type)
    
  }else if(function.name == "select_multiple"){
    
    output <- select_multiple_questions(question.name, group.name, calculation.type)
    
  }else if(function.name == "integer" & calculation.type == "Mean"){
    
    output <- integer_questions_mean(question.name, group.name, calculation.type)
    
  }else if(function.name == "integer" & calculation.type == "Median"){
    
    output <- integer_questions_median(question.name, group.name, calculation.type)
  } 
  return(output)
})

result.items.community <- result.items.community %>% reduce(rbind)

result.items.community <- result.items.community %>% 
                          add_column(Research.Question = "RQ1")
# PART 2B: Items  core questions analysis at overall level ################################### 
result.items <- lapply(1:nrow(core.items.list), function(r){
  
  # core.items.list input parameters
  function.name <- as.character(core.items.list[r, "Function"])
  question.name <- as.character(core.items.list[r, "Col.name"])
  calculation.type <- as.character(core.items.list[r, "Calculation"])
  group.name <- as.character(core.items.list[r, "Group"])
  
  if(all(is.na(data.all[,question.name]))) {
    
    output <- column.is.empty(question.name)
    
  }else if(function.name == "select_one"){
    
    output <- select_one_questions_no_agg(question.name)
    
  }else if(function.name == "select_multiple"){
    
    output <- select_multiple_questions_no_agg(question.name)
    
  }else if(function.name == "integer" & calculation.type == "Median"){
    
    output <- integer_questions_median_no_agg(question.name)
  } 
  return(output)
})

result.items <- result.items %>% reduce(rbind)
result.items <- result.items %>% 
  add_column( Group = "Core Questions - repeat for each item (for dry food and for fresh food)",
              Community = "Overall",
              Research.Question = "RQ1") %>% 
  mutate(Calculation = case_when(Function %in% c("counts", "percentage") ~ "count and percentage",
                                 TRUE ~ "Median"))
result.core.items.analysis <- rbind(result.items,result.items.community)

# Part 3A Analysis of all other indicators at community level ################################### 

analysis.plan <- read.xlsx("input/FIAA_Analysis Plan.xlsx", sheet = "Sheet1")
dap.all <- analysis.plan %>%
  filter(Group != "Core Questions - repeat for each item (for dry food and for fresh food)" & 
           !is.na(Function)& !is.na(Calculation))

result.analysis <- lapply(1:nrow(dap.all), function(r){
  # dap.all input parameters
  function.name <- as.character(dap.all[r, "Function"])
  question.name <- as.character(dap.all[r, "Col.name"])
  calculation.type <- as.character(dap.all[r, "Calculation"])
  group.name <- as.character(dap.all[r, "Group"])
  
  if(all(is.na(data.all[,question.name]))) {
    
    output <- column.is.empty(question.name, group.name, calculation.type)
    
  }else if(function.name == "select_one"){
    
    output <- select_one_questions(question.name, group.name, calculation.type)
    
  }else if(function.name == "select_multiple"){
    
    output <- select_multiple_questions(question.name, group.name, calculation.type)
    
  }else if(function.name == "integer" & calculation.type == "Mean"){
    
    output <- integer_questions_mean(question.name, group.name, calculation.type)
    
  }else if(function.name == "integer" & calculation.type == "Median"){
    
    output <- integer_questions_median(question.name, group.name, calculation.type)
    
  } else{
    output <- print("something's missing")
  }
  
  return(output)
})

result.analysis <- result.analysis %>% reduce(rbind)

indicators <- read.xlsx("input/FIAA_Analysis Plan.xlsx", sheet = "Sheet4") %>% 
              select(Col.name, Research.Question)
result.analysis <- left_join(result.analysis,indicators,by = c("Indicator" = "Col.name"))
# Part 3B Analysis of all other indicators at sub  district level ################################### 

analysis.graphs.agg <- read.xlsx("input/FIAA_Analysis Plan.xlsx", sheet = "Sheet2")

result.analysis.district <- lapply(1:nrow(analysis.graphs.agg), function(r){
  # dap.all input parameters
  function.name <- as.character(analysis.graphs.agg[r, "Function"])
  question.name <- as.character(analysis.graphs.agg[r, "Col.name"])
  calculation.type <- as.character(analysis.graphs.agg[r, "Calculation"])
  group.name <- as.character(analysis.graphs.agg[r, "Group"])
  
  if(all(is.na(data.all[,question.name]))) {
    
    output <- column.is.empty(question.name, group.name, calculation.type)
    
  }else if(function.name == "select_one"){
    
    output <- select_one_questions_agg(question.name, group.name, calculation.type)
    
  } else{
    output <- print("something's missing")
  }
  
  return(output)
})
result.analysis.district <- result.analysis.district %>% reduce(rbind)
result.analysis.district<- result.analysis.district %>% 
                           rename( Community = Sub_disctrict)
result.analysis.district <- left_join(result.analysis.district, indicators, by = c("Indicator" = "Col.name"))
# Part 3 Analysis of all other indicators Overall ################################### 
analysis.for.charts <- read.xlsx("input/FIAA_Analysis Plan.xlsx", sheet = "no.agg")

result.items.no.agg <- lapply(1:nrow(analysis.for.charts), function(r){
  # analysis.for.charts input parameters
  function.name <- as.character(analysis.for.charts[r, "Function"])
  question.name <- as.character(analysis.for.charts[r, "Col.name"])
  calculation.type <- as.character(analysis.for.charts[r, "Calculation"])
  group.name <- as.character(analysis.for.charts[r, "Group"])
  
  if(function.name == "select_one"){
    
    output <- select_one_questions_no_agg(question.name)
    
  }else if(function.name == "select_multiple"){
    
    output <- select_multiple_questions_no_agg(question.name)
    
  }else if(function.name == "integer" & calculation.type == "Median"){
    
    output <- integer_questions_median_no_agg(question.name)
  } 
  
  return(output)
})
result.items.no.agg <- result.items.no.agg %>% reduce(rbind)

indicators_A <- read.xlsx("input/FIAA_Analysis Plan.xlsx", sheet = "Sheet4")

result.items.no.agg_b <- left_join(result.items.no.agg,indicators_A, by = c("Indicator" = "Col.name")) %>% 
                       add_column(Community = "Overall")
#merge results
result.final <- rbind(result.core.items.analysis, result.analysis,result.analysis.district,result.items.no.agg_b)

#split by Research.Question and Save separately 
split.by.group <-split(result.final, f = result.final$Research.Question)
for(i in names(split.by.group)){
  write.csv(split.by.group[[i]], paste0("./output/tables/groups analysis/",i,".csv"), row.names=FALSE)
}

# Graphs #######################################################################

 

graphs <- read.xlsx("input/FIAA_Analysis Plan.xlsx", sheet = "Graphs")

for (i in 1:nrow(graphs)) {
  
  chart.type <- as.character(graphs[i, "Graph.needed"])
  question.name <- as.character(graphs[i, "Question"])
  label <- as.character(graphs[i, "label"])
  aggregation <- as.character(graphs[i, "aggregation"])
  
  if( chart.type == "Pie chart"){
    if(is.na(aggregation)){
      graph.data <- result.items.no.agg
      pie <- pie.no.agg(question.name, graph.data, label)
      ggsave(pie, filename = paste0("./output/graphs/pie charts/", question.name, ".pdf"), device = "pdf")
    } else if(aggregation == "Community"){
      graph.data <- result.analysis.district
      pie <- pie.agg(question.name, graph.data, label)
      ggsave(pie, filename = paste0("./output/graphs/pie charts/", question.name, "_per_Sub_district.pdf"), device = "pdf")
    }
  } else if(chart.type == "Bar chart"){
    if(is.na(aggregation)){
      graph.data <- result.items.no.agg
      bar <- bar.no.agg(question.name, graph.data, label)
      ggsave(bar, filename = paste0("./output/graphs/bar charts/", question.name, ".pdf"), device = "pdf")
    } else if(aggregation == "Community"){
      graph.data <- result.analysis.district
      bar <- bar.agg(question.name, graph.data, label)
      ggsave(bar, filename = paste0("./output/graphs/bar charts/", question.name, "_per_Sub_district.pdf"), device = "pdf")
    }
  } else if (chart.type == "Column chart"){
    if(is.na(aggregation)){
      graph.data <- result.items.no.agg
      col <- col.no.agg(question.name, graph.data, label)
      ggsave(col, filename = paste0("./output/graphs/Column charts/", question.name, ".pdf"), device = "pdf")
    } else if(aggregation == "Community"){
      graph.data <- result.analysis.district
      col <- col.agg(question.name, graph.data, label)
      ggsave(col, filename = paste0("./output/graphs/Column charts/", question.name, "_per_Sub_district.pdf"), device = "pdf")
    }
  }
}
