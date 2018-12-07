# Date: 2018-12-07
# S Ogletree
# Description: Explore import and EDA

library(tidyverse)
library(googlesheets4)
library(janitor)
library(alluvial)

# Get Google Sheet data
df <- sheets_read("1LlBr-9sJYWZ88SJugOr9nM9I0wRMk0FwGO4oWstXsdE")
str(df)

# look at the positions
pt <- df %>% tabyl(Current_Institution, Current_Postion) %>% adorn_percentages()
pt %>% gather(position, num, -Current_Institution) %>% ggplot(aes(position, num,  fill=Current_Institution)) + geom_col(position = "dodge") +coord_flip()

# the years people recieved degree
df %>% ggplot(aes(Year_Highest_Degree)) + geom_density()

# average year of degree by department
df %>% group_by(Current_Institution) %>% summarise(meanyr = mean(Year_Highest_Degree, na.rm = T))
# the most recent
df %>% group_by(Current_Institution) %>% summarise(meanyr = max(Year_Highest_Degree, na.rm = T))

# Years since degree
df %>% filter(!is.na(Year_Highest_Degree)) %>% mutate(yrsince = 2018 - Year_Highest_Degree) %>% group_by(Current_Institution) %>% summarise(meanyrs = mean(yrsince))

# Using the year bachelors was obtained as a rough proxy for when person was 21
df %>% filter(!is.na(Year_Bachelors)) %>% mutate(approxage = (2018-Year_Bachelors)+21) %>% group_by(Current_Institution) %>% summarise(meanage = mean(approxage))
# ages of faculty on the whole
df %>% filter(!is.na(Year_Bachelors)) %>% mutate(approxage = (2018-Year_Bachelors)+21) %>% ggplot(aes(approxage)) + geom_histogram()
# age by position
df %>% filter(!is.na(Year_Bachelors)) %>% mutate(approxage = (2018-Year_Bachelors)+21) %>% group_by(Current_Postion) %>% summarise(meanage = mean(approxage))

# Sankey Diagram ----------------------------------------------------------

d1 <- df %>% select(Current_Institution, Degree_Intstitution) %>% filter(!is.na(Degree_Intstitution))
d2 <- d1 %>% group_by(Current_Institution, Degree_Intstitution) %>% summarise(Freq = n())
d2 <- d2[, c(2, 1, 3)]
alluvial(d2[, 1:2], freq = d2$Freq, col = ifelse( d2$Degree_Intstitution == "Clemson University", "orange", "grey" ),  cex = 0.7)


# Network Diagrams --------------------------------------------------------
# https://www.jessesadler.com/post/network-analysis-with-r/
# https://www.datacamp.com/community/tutorials/centrality-network-analysis-R
library(ggraph)
library(igraph)
gr <- graph_from_data_frame(d2)
ggraph(gr, layout = 'fr') + geom_node_point() + geom_edge_link(aes(width = Freq)) + geom_edge_loop(aes(width = Freq))

degree.cent <- centr_degree(gr, mode = "all")
degree.cent$centralization
degree.cent$res
data.frame(closeness(gr, mode="all")) %>% rownames_to_column() %>% arrange(desc(closeness.gr..mode....all..))
