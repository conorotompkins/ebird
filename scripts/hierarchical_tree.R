library(tidyverse)
library(rebird)
library(janitor)
library(ggraph)
library(tidygraph)
library(grapher)

rebird::ebirdfreq("counties", "US-PA-03")

species_code('sula variegata')

tax_data_all <- ebirdtaxonomy() %>% 
  clean_names() %>% 
  mutate(class = "Aves")

glimpse(tax_data_all)

tax_data_all %>% 
  count(order, sort = T)

tax_data <- tax_data_all

tax_data %>% 
  select(class, order, family_com_name, com_name)

tax_data %>% 
  select(class, order, family_com_name, com_name) %>% 
  filter(is.na(family_com_name)) %>% 
  View()

edges_1 <- tax_data %>% 
  select(class, order) %>% 
  distinct() %>% 
  rename(from = 1,
         to = 2)

edges_2 <- tax_data %>% 
  select(order, family_com_name) %>% 
  distinct() %>% 
  rename(from = 1,
         to = 2)

edges_3 <- tax_data %>% 
  select(family_com_name, com_name) %>%
  distinct() %>% 
  rename(from = 1,
         to = 2)

edges_combined <- bind_rows(edges_1, edges_2)

nodes <- tax_data %>% 
  select(class, order, family_com_name) %>% 
  pivot_longer(cols = everything(), names_to = "type", values_to = "value") %>% 
  select(value, type) %>% 
  distinct() %>% 
  mutate(type = fct_relevel(type, c("class", "order", "family_com_name")))

nodes %>% 
  mutate(node_id = row_number()) %>% 
  filter(value == "Passeriformes")

nodes %>% 
  mutate(node_id = row_number()) %>% 
  View()

bird_family_network <- tbl_graph(nodes = nodes, edges = edges_combined, directed = T) %>%
  activate(nodes) %>% 
  mutate(is_root = node_is_root()) %>%
  filter(node_is_adjacent(c(1, 152)) | node_is_root())

bird_family_network %>% 
  as_tibble() %>% 
  View()

bird_family_tree <- bird_family_network %>% 
  ggraph(layout = "dendrogram", circular = T) +
  geom_edge_diagonal(edge_alpha = .5, edge_width = .5) +
  geom_node_point(aes(filter = type != "com_name",
                      color = type)) +
  geom_node_point(aes(filter = type == "comm_name"),
                  size = .1) +
  geom_node_label(aes(size = type, 
                      label = value,
                      color = type)) +
  scale_size_manual(values = c(1, 5, 8)) +
  theme_void()

bird_family_tree %>% 
  ggsave(filename = "output/taxonomy/bird_family_tree.png",
         width = 40,
         height = 40,
         dpi = 300)


## https://grapher.network/
edges_new <- tbl_graph(nodes = nodes, edges = edges_combined, directed = T) %>% 
  activate(edges) %>% 
  as_tibble()

nodes_new <- nodes %>% 
  mutate(id = row_number()) %>% 
  select(id, name = value, type) 


list(nodes = nodes_new, links = edges_new) %>% 
  graph(directed = T) %>% 
  scale_node_color(type)

