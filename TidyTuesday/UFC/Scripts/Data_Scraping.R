#모든 내용은 Andrew Couch의 자료 및 영상을 참고하였습니다.
#All of Data from http://ufcstats.com/statistics/events/completed
library(rvest) #web scraping
library(tidyverse) #Data preprocessing
library(progress) #show progres bar for check progress status
library(here) #path 지정

scrape_cards <- function(link){
  link %>% 
    read_html() %>% 
    html_nodes(".b-link_style_black") %>%  #매칭되는 요소 반환
    html_attr("href") %>% #attribute 추출
    tibble("cards"=.)
}
scrape_dates <- function(link){
  link %>% 
    read_html() %>% 
    html_nodes(".b-list__box_list_item:nth-child(1)") %>% 
    html_text() %>% 
    tibble("fight_data"=.) %>% 
    separate(fight_date,info = c("key","value"),sep = ":") %>% 
    select(date = value) %>% 
    mutate(date = str_replace_all(date,"\n","")) %>% 
    mutate(date =str_trim(date)) #공백제거
}

scrape_fights <- function(link){
  link %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    tibble("fights" = .) %>% 
    filter(str_detect(fights,"fight_details"))
}

scrape_fight_summary_data <- function(link){
  link <- link %>% read_html()
  
  table_df <- link %>% html_nodes("table")
  
  summary_data <- table_df[1] %>% 
    html_table(trim = T, fill = T) %>% 
    do.call("rbind",.) %>% 
    as_tibble() %>% 
    rename("Fighter" = 1, "KD" = 2, "Sig_Strike" = 3, "Sig_Strike_Percent" = 4,
           "Total_Strikes" = 5, "TD" = 6, "TD_Percent"= 7, "Sub_Attempts" = 8, "Pass" = 9, "Rev" = 10) %>% 
    gather() %>% 
    separate(value, into = c("fighter_1","fighter_2"), sep = '  ', extra = 'merge') %>% 
    mutate_all(~str_replace_all(.x,"\n",'')) %>% 
    mutate_all(str_trim) %>% 
    pivot_wider(names_from = key,values_from = c(fighter_1,fighter_2)) %>%
    separate(fighter_1_Sig_Strike, into = c('fighter_1_Sig_Strike_Landed','fighter_1_Sig_Stirke_Attempts'),
             sep = " of ",extra = "merge") %>% 
    separate(fighter_2_Sig_Strike, into = c("fighter_2_Sig_Strike_Landed",'fighter_2_Sig_Strike_Attempts'),
             sep = " of ",extra = "merge") %>%
    separate(fighter_1_Total_Strikes, into = c("fighter_1_Strike_Landed","fighter_1_Strike_Attempts"),
             sep = " of ",extra = "merge") %>% 
    separate(fighter_2_Total_Strikes, into = c("fighter_2_Strike_Landed","fighter_2_Strike_Attempts"),
             sep = " of ",extra = "merge") %>% 
    separate(fighter_1_TD, into = c("fighter_1_TD_Landed","fighter_1_TD_Attempts"),
             seo = " of ",extra = "merge") %>% 
    separate(fighter_2_TD, into = c("fighter_2_TD_Landed","fighter_2_TD_Attempts"),
             seo = " of ",extra = "merge") %>% 
    mutate_at(vars(contains("Percnet")),~.01*str_replace(.x,"%","") %>%  as.numeric()) %>% 
    mutate_at(vars(-contains("Fighter",ignore.case = F)),as.numeric)
  
    fight_details <- link %>% 
      html_nodes(xpath = '//*[contains(concat( " ",@class, " " ),concat( " ", "b-fight-details__text", " ")) and (((count(preceding-sibling::*) +1) = 1) and parent::*)]//i') %>% 
      html_text() %>% 
      as_tibble() %>% 
      mutate(value = str_replace_all(value,"\n","")) %>% 
      mutate(value = str_trim(value, side = "both")) %>% 
      separate(value, into = c("feature","value"), sep = ":", extra = "merge") %>% 
      mutate(value = str_trim(value)) %>% 
      replace_na(list(value = "")) %>% 
      group_by(feature) %>% 
      filter(value != "") %>% 
      ungroup() %>% 
      pivot_wider(names_from = feature, values_from = value) %>% 
      rename_all(.funs = ~str_replace(.x,"\\s|/","_") %>% tolower()) %>% 
      cbind(
        link %>% 
          html_node(".b-fight-details__persons") %>% 
          html_text() %>% 
          str_extract("[:upper:]{1}") %>% 
          tibble("fighter_1_res" = .)) %>% 
      mutate(fighter_2_res = case_when(
        fighter_1_res == "L" ~"W",
        fighter_2_res == "W" ~"L",
        TRUE ~ "D"
      )) %>% 
      rename("round_finished" = "round") %>% 
      cbind(
        link %>% 
          html_nodes(".b-fight-details__fight-title") %>% 
          html_text() %>% 
          str_replace_all('\n',"") %>% 
          str_trim() %>% 
          tibble(weight_class=.))
    
    summary_data <- cbind(summary_data,fight_details)
    pb$tick()
    Sys.sleep(1/100)
    summary_data %>% as_tibble()
    
}

#scrape_round_data

#==========================----------------------------------


