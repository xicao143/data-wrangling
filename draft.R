write.csv(scraped, "2020 Tuition and Living Costs Summary By States")
university_num = unique(diversity_school$name)
t1 <- diversity_school %>%
  drop_na() %>%
  group_by(state, category) %>%
  summarise(count = sum(enrollment)) %>%
  arrange(desc(count))
grid.arrange(
  ggplot(t1) + 
    geom_bar(aes(x = count, y = state),stat = "identity") + 
    facet_wrap(~ category) + 
    ggtitle("Total Enrollment by State and Category"),
  diversity_school %>% 
    drop_na() %>% 
    group_by(state) %>%
    summarise(value = sum(enrollment)) %>%
    mutate(region = tolower(state)) %>%
    inner_join(df_state_demographics, by = 'region') %>%
    state_choropleth(title = "Total Enrollment by State") + 
    theme(legend.position = "bottom"),
  ncol = 1
)


historical_tuition %>% 
  mutate(type2 = str_detect(`tuition_type`, "\\bConstant")) %>%
  filter(type2 == TRUE & type == "All Institutions") %>% 
  ggplot() + 
  geom_bar(aes(x = tuition_cost, y =year , fill = tuition_type),stat = "identity")

grid.arrange(
  historical_tuition %>% 
    mutate(type2 = str_detect(`tuition_type`, "\\bConstant")) %>%
    filter(type2 == TRUE & type == "All Institutions") %>% 
    ggplot() + 
    geom_bar(aes(x = tuition_cost, y =year , fill = tuition_type),stat = "identity", position = "dodge") + 
    ggtitle("1985-2017 All Institutions Tuition Costs"),
  historical_tuition %>% 
    mutate(type2 = str_detect(`tuition_type`, "\\bConstant")) %>%
    filter(type2 == TRUE & type == "Public") %>% 
    ggplot() + 
    geom_bar(aes(x = tuition_cost, y =year , fill = tuition_type),stat = "identity", position = "dodge") + 
    ggtitle("1985-2017 Public Tuition Costs"),
  historical_tuition %>% 
    mutate(type2 = str_detect(`tuition_type`, "\\bConstant")) %>%
    filter(type2 == TRUE & type == "Private") %>% 
    ggplot() + 
    geom_bar(aes(x = tuition_cost, y = year , fill = tuition_type),stat = "identity", position = "dodge") + 
    ggtitle("1985-2017 Private Tuition Costs"),
  ncol = 2
)



t2 <- salary_potential %>% 
  drop_na() %>%
  group_by(state_name) %>%
  summarise(avg_early_career_pay = round(mean(early_career_pay),2),
            avg_mid_career_pay = round(mean(mid_career_pay),2),
            avg_make_world_better_perc = round(mean(make_world_better_percent),2),
            avg_stem = round(mean(stem_percent),2))

grid.arrange(
  t2 %>%
    melt() %>%
    filter(variable == "avg_early_career_pay"|variable == "avg_mid_career_pay") %>%
    ggplot() + 
    geom_bar(aes(x = value, y = state_name, fill = variable), stat = "identity", position = "dodge") +
    xlab("average career pay"),
  
  t2 %>%
    melt() %>%
    filter(variable == "avg_make_world_better_perc"|variable == "avg_stem") %>%
    ggplot() + 
    geom_bar(aes(x = value, y = state_name, fill = variable), stat = "identity", position = "dodge") +
    xlab("university quality"),
  ncol=2
)


t2 %>%
  rename(value = avg_mid_career_pay) %>%
  mutate(region = tolower(state_name) %>% str_replace_all("-", " ")) %>%
  inner_join(df_state_demographics, by = 'region') %>%
  state_choropleth(title = "Average Mid-Career Pay by State") + 
  theme(legend.position = "bottom")


tuition_cost <- tuition_cost %>% 
  replace_na(list(state="District of Columbia", room_and_board = 0)) %>%
  mutate(total = in_state_total + out_of_state_total)

  
tuition_cost_public <- tuition_cost %>% 
  filter(type == "Public") %>%
  group_by(state, degree_length) %>%
  summarise(avg_in_state_total = round(mean(in_state_total),2),
            avg_out_of_state_total = round(mean(out_of_state_total),2),
            avg_total = round(mean(total),2)) %>%
  arrange(desc(avg_total))
head(tuition_cost_public, 5)

tuition_cost_private <- tuition_cost %>% 
  filter(type == "Private") %>%
  group_by(state, degree_length) %>%
  summarise(avg_in_state_total = round(mean(in_state_total),2),
            avg_out_of_state_total = round(mean(out_of_state_total),2),
            avg_total = round(mean(total),2)) %>%
  arrange(desc(avg_total))
head(tuition_cost_private, 5)

tuition_cost_profit <- tuition_cost %>% 
  filter(type == "For Profit") %>%
  group_by(state, degree_length) %>%
  summarise(avg_in_state_total = round(mean(in_state_total),2),
            avg_out_of_state_total = round(mean(out_of_state_total),2),
            avg_total = round(mean(total),2)) %>%
  arrange(desc(avg_total))
head(tuition_cost_profit, 5)



grid.arrange(
  ggplot(tuition_cost_private %>% melt() %>% filter(degree_length == "4 Year")) +
    geom_bar(aes(x = value, y = state, fill = variable), stat = "identity", position = "dodge") + 
    ggtitle("4 Year Tuition Cost Private"),
  ggplot(tuition_cost_private %>% melt() %>% filter(degree_length == "2 Year")) +
    geom_bar(aes(x = value, y = state, fill = variable), stat = "identity", position = "dodge") + 
    ggtitle("2 Year Tuition Cost Private"),
  ncol = 2
)

grid.arrange(
  ggplot(tuition_cost_public %>% melt() %>% filter(degree_length == "4 Year")) +
    geom_bar(aes(x = value, y = state, fill = variable), stat = "identity", position = "dodge") + 
    ggtitle("4 Year Tuition Cost Public"),
  ggplot(tuition_cost_public %>% melt() %>% filter(degree_length == "2 Year")) +
    geom_bar(aes(x = value, y = state, fill = variable), stat = "identity", position = "dodge") + 
    ggtitle("2 Year Tuition Cost Public"),
  ncol = 2
)


grid.arrange(
  ggplot(tuition_cost_profit %>% melt() %>% filter(degree_length == "4 Year")) +
    geom_bar(aes(x = value, y = state, fill = variable), stat = "identity", position = "dodge") + 
    ggtitle("4 Year Tuition Cost For Profit"),
  ggplot(tuition_cost_profit %>% melt() %>% filter(degree_length == "2 Year")) +
    geom_bar(aes(x = value, y = state, fill = variable), stat = "identity", position = "dodge") + 
    ggtitle("2 Year Tuition Cost For Profit"),
  ncol = 2
)



join1 <- left_join(tuition_cost, diversity_school, by = c("name", "state")) %>%
  drop_na() %>%
  group_by(state, category) %>%
  summarise(avg_total = round(mean(total),2),
            avg_total_enrollment = round(mean(enrollment),2)) 

ggplot(join1) + 
  geom_bar(aes(x = avg_total, y = state, fill = variable), stat = "identity", position = "dodge") +
  facet_wrap(~ category) + 
  ggtitle("Tuition Cost on Category")
  




state_books <- state_books[,-c(1:3)]
state_books$State <- str_replace_all(state_books$State, "-", " ") 
state_books[state_books == "-"] <- 0
colnames(state_books) <- c("Undergraduate-books", "Graduate-books", "State")

state_room <- state_room[,-c(1:3)]
state_room$State <- str_replace_all(state_room$State, "-", " ") 
state_room[state_room == "-"] <- 0
colnames(state_room) <- c("Undergraduate-room", "Graduate-room", "State")

state_tuition <- state_tuition[,-1]
state_tuition$State <- str_replace_all(state_tuition$State, "-", " ") 
state_tuition[state_tuition == "-"] <- 0
colnames(state_tuition) <- c("In-state Undergraduate Tuition Fee", "Out-state Undergraduate Tuition Fee", "In-state Graduate Tuition Fee", "Out-state Graduate Tuition Fee", "State")

state_total <- state_total[,-1]
state_total$State <- str_replace_all(state_total$State, "-", " ") 
state_total[state_total == "-"] <- 0
colnames(state_total) <- c("In-state Undergraduate Total", "Out-state Undergraduate Total", "In-state Graduate Total", "Out-state Graduate Total", "State")


scraped2 <- left_join(state_total, state_tuition) %>% 
  left_join(state_room) %>% 
  left_join(state_books) %>%
  .[, c(5, 1:4, 6:13)]
write.csv(scraped2, "2020 Tuition and Living Costs Summary By States 2")


scraped$`In-State Public Tuition Fees` <- as.numeric(str_replace(substr(scraped$`In-State Public Tuition Fees`, 2,10), ",",""))
scraped$`Out-State Public Tuition Fees` <- as.numeric(str_replace(substr(scraped$`Out-State Public Tuition Fees`, 2,10), ",",""))
scraped$`Private Tuition Fees` <- as.numeric(str_replace(substr(scraped$`Private Tuition Fees`, 2,10), ",",""))
scraped$`On-Campus Living Costs` <- as.numeric(str_replace(substr(scraped$`On-Campus Living Costs`, 2,10), ",",""))
scraped$`Off-Campus Living Costs` <- as.numeric(str_replace(substr(scraped$`Off-Campus Living Costs`, 2,10), ",",""))

for(i in 2:13){
  scraped2[,i] <- as.numeric(str_replace(substr(scraped2[,i], 2,10), ",",""))
  scraped2[,i] <- replace_na(scraped2[,i], 0)
}

for(i in 3:6){
  scraped[,i] <- replace_na(scraped[,i], 0)
}


scraped <- scraped %>%
  .[-62,] %>%
  mutate(`Public Tuition Fees` = `In-State Public Tuition Fees` + `Out-State Public Tuition Fees`)


grid.arrange(
  scraped %>% 
    melt() %>%
    filter(variable == "public tuition fees"|variable == "Private Tuition Fees") %>%
    ggplot() + 
    geom_bar(aes(x = value, y = State, fill = variable), stat = "identity", position = "dodge") +
    ggtitle("2020 Tuition Fees By University Type"),
  
  scraped %>% 
    melt() %>%
    filter(variable == "On-Campus Living Costs"|variable == "Off-Campus Living Costs") %>%
    ggplot() + 
    geom_bar(aes(x = value, y = State, fill = variable), stat = "identity", position = "dodge") +
    ggtitle("2020 Living Costs By States"),
  ncol = 2
)


scraped2 %>% 
  select("State", "In-state Undergraduate Total", "Out-state Undergraduate Total", "In-state Graduate Total", "Out-state Graduate Total") %>%
  melt() %>%
  ggplot() + 
  geom_bar(aes(x = value, y = State), stat = "identity", position = "dodge") +
  facet_wrap(~ variable) + 
  ggtitle("2020 Costs of Attendance By States")

scraped2 %>% 
  rename(region = State, value = `Out-state Graduate Total`) %>%
  inner_join(df_state_demographics, by = 'region') %>%
  state_choropleth(title = "2020 Costs of Attendance on Graduate By States") + 
  theme(legend.position = "bottom")



scraped %>% 
  melt() %>%
  filter(variable == "Public Tuition Fees"|variable == "Private Tuition Fees") %>%
  ggplot() + 
  geom_bar(aes(x = value, y = State, fill = variable), stat = "identity", position = "dodge") +
  ggtitle("2020 Tuition Fees By University Type") + 
  theme(legend.position="bottom")

