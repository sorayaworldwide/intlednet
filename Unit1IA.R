library(tidyverse)
library(tidytext)
library(readxl)
library(writexl)
library(rtweet)
library(tweetrmd)
library(knitr)
library(tidygraph)
library(ggraph)
library(igraph)

##store api keys
app_name <-"sentimentcovidintled"
api_key <-"NrLRwiMnpknvbMEwYXxB2Gw4A"
api_secret_key <-"meLIhMf9skmIbRUx0UssNtrYszWgKPU5pvCRxKEqBWKSMCYEPR"
access_token <-"293694037-bMnCGVK8wuEWsnksAaB7wpIYIdJAoNKmwWwJsP2q"
access_token_secret <-"MgqZXwR3RYTzU7kRL2Jipozy4d4hHcf7kswiEdwmxa2YW"
##authenticate
token<-create_token(
  app =app_name,
  consumer_key = api_key,
  consumer_secret=api_secret_key,
  access_token = access_token, 
  access_secret = access_token_secret)

intled_net <-search_tweets("#intled", n=500, include_rts = FALSE)
view(intled_net)

highered_net <-search_tweets("#highered", n=500, include_rts = FALSE)
view(highered_net)

#save intled_net to excel
write_xlsx(intled_net, "intl_net.xlsx")

#regex
regex <- "@([A-Za-z]+[A-Za-z0-9_]+)(?![A-Za-z0-9_]*\\.)"

intled_net_usernames <-
  intled_net %>%
  # Use regular expression to identify all the usernames in a tweet
  mutate(all_mentions = str_extract_all(text, regex)) %>%
  unnest(all_mentions)

mentions <-intled_net_usernames%>%
  mutate(all_mentions = str_trim(all_mentions))%>%
  select(sender=screen_name, all_mentions)

#remove @
edgelist <- 
  mentions %>% 
  # remove "@" from all_mentions column
  mutate(all_mentions = str_sub(all_mentions, start = 2)) %>% 
  # rename all_mentions to receiver
  select(sender, receiver = all_mentions)

#count interactions

interactions_sent <- edgelist %>% 
  # this counts how many times each sender appears in the data frame, effectively counting how many interactions each individual sent 
  count(sender) %>% 
  # arranges the data frame in descending order of the number of interactions sent
  arrange(desc(n))

interactions_sent
interactions_sent


edgelist <- edgelist %>% 
  # the first of the two lines below filters to include only senders in the interactions_sent data frame
  # the second line does the same, for receivers
  filter(sender %in% interactions_sent$sender,
         receiver %in% interactions_sent$sender)

g<-as_tbl_graph(edgelist)
g

  g %>%
  # we chose the kk layout as it created a graph which was easy-to-interpret, but others are available; see ?ggraph
  ggraph(layout = "kk") +
  # this adds the points to the graph
  geom_node_point() +
  # this adds the links, or the edges; alpha = .2 makes it so that the lines are partially transparent
  geom_edge_link(alpha = .2) +
  # this last line of code adds a ggplot2 theme suitable for network graphs
  theme_graph()
  
  
  g %>% 
    # this calculates the centrality of each individual using the built-in centrality_authority() function
    mutate(centrality = centrality_authority()) %>% 
    ggraph(layout = "kk") + 
    geom_node_point(aes(size = centrality, color = centrality)) +
    geom_node_label(aes(label=name), repel=TRUE)+
    # this line colors the points based upon their centrality
    scale_color_continuous(guide = 'legend') + 
    geom_edge_link(alpha = .2) +
    theme_graph()
  
  ##try with highered 
  highered_net_tweets <-
    highered_net%>%
    mutate(all_mentions = str_extract_all(text, regex)) %>%
    unnest(all_mentions)
  
  mentions_highered <-
    highered_net_tweets %>%
    mutate(all_mentions = str_trim(all_mentions)) %>%
    select(sender = screen_name, all_mentions)
  
  edgelist_highered <-
    mentions_highered%>%
    mutate(all_mentions = str_sub(all_mentions, start = 2)) %>% 
    select(sender, receiver = all_mentions)
  
  interactions_sent_highered <- edgelist_highered %>% 
    count(sender) %>% 
    arrange(desc(n))
  
  interactions_sent_highered 
  
  edgelist_highered <- edgelist_highered %>% 
    filter(sender %in% interactions_sent$sender,
           receiver %in% interactions_sent$sender)
  
  #rtweet with igraph
  fds<-get_friends(c("DukeU", "UNC", "NCState"))
  
  #frequency count
  tbl <-table(fds$user_id)

  #100 or more
  fds3<-subset(fds, user_id %in% names(tbl[tbl>2L]))

  #convert to matrix
  mat <-as.matrix(fds3)

  #graph object
  mat<-igraph::graph_from_edgelist(mat)

  #plot network
  plot(mat, edge.arrow.size=.5, vertex.color="gold", vertex.size=15, 
       vertex.frame.color="gray", vertex.label.color="black", 
       vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)
  
  
  plot(mat, edge.arrow.size=.2, edge.color="orange",
       vertex.color="orange", vertex.frame.color="#ffffff",
       vertex.label=V(mat)$user, vertex.label.color="black") 
 
  transitivity(g) 
  reciprocity(g)
  