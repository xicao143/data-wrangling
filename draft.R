write.csv(scraped, "2020 Tuition and Living Costs Summary By States")
university_num = unique(diversity_school$name)
t1 <- diversity_school %>%
  drop_na() %>%
  group_by(state, category) %>%
  summarise(count = sum(enrollment)) %>%
  arrange(desc(count))
grid.arrange(
  ggplot(t1) + 
    geom_bar(aes(x = count, y = state, fill = category),stat = "identity") + 
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
    geom_bar(aes(x = tuition_cost, y =year , fill = tuition_type),stat = "identity") + 
    ggtitle("1985-2017 All Institutions Tuition Costs"),
  historical_tuition %>% 
    mutate(type2 = str_detect(`tuition_type`, "\\bConstant")) %>%
    filter(type2 == TRUE & type == "Public") %>% 
    ggplot() + 
    geom_bar(aes(x = tuition_cost, y =year , fill = tuition_type),stat = "identity") + 
    ggtitle("1985-2017 Public Tuition Costs"),
  historical_tuition %>% 
    mutate(type2 = str_detect(`tuition_type`, "\\bConstant")) %>%
    filter(type2 == TRUE & type == "Private") %>% 
    ggplot() + 
    geom_bar(aes(x = tuition_cost, y =year , fill = tuition_type),stat = "identity") + 
    ggtitle("1985-2017 Private Tuition Costs"),
  ncol = 2
)













