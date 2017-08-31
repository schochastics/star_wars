library(rvest)
library(tidyverse)
library(stringr)
library(stringi)
library(hrbrthemes)


################################################################################
## scrape-extract-clean data
################################################################################
#imdb list of screen times
url <- "http://www.imdb.com/list/ls031379663/"
# scrape relevant info ----
doc <- read_html(url) %>% 
  html_nodes(".description")
doc <- doc[-1]

char <- read_html(url) %>% 
  html_nodes(".info") %>% 
  html_node("a") %>% 
  html_text()

# extract screen times ----
screen_regex <- "([A-Z ]+: )*[A-Z ]+ <([0-9]+)*(:[0-9]+)*>"
screen_times <- stri_extract_all_regex(html_text(doc),screen_regex)


#build data.frame ----
length_list <- map(screen_times,length) %>% unlist()
movie <- map(screen_times,function(x) word(x,1,sep="<")) %>% 
  unlist %>% str_trim()
time   <- map(screen_times,function(x) stri_extract_last_regex(x,"(?<=<)(.*?)(?=>)") )  %>% unlist

times_df <- tibble(character=rep(char,length_list),movie=movie,time=time)

times_df <- times_df %>% 
  mutate(time = str_replace_all(time,"^:","0:")) %>% 
  mutate(time = ifelse(str_detect(time,":"),time,paste0(time,":00"))) %>% 
  mutate(time = as.numeric(word(time,1,sep=":"))+as.numeric(word(time,2,sep=":"))/60) %>% 
  group_by(character,movie) %>% 
  dplyr::summarise(time=sum(time))

times_df <- times_df %>% dplyr::filter(!is.na(character))
movies <- unique(movie)
times_df$character[times_df$character=="Darth Vader" & 
                   times_df$movie%in%movies[1:3]] <- "Anakin Skywalker"

write_csv(times_df,"data.csv")
#clean up ----
rm(char,doc,length_list,screen_regex,screen_times,time,url,movie)

################################################################################
## analyse and plot
################################################################################
#screen time per character ----
star_wars_col <- c("#27408B", "#3A5FCD", "#6CA6CD", "#8B6914", "#CD9B1D", "#FFC125", "#CD2626", "#5C5C5C")
times_df %>% 
  group_by(character) %>% 
  mutate(sum=sum(time)) %>% 
  ungroup() %>% 
  dplyr::filter(dense_rank(-sum)<=20) %>% 
  ggplot(aes(x = reorder(character,sum), y = time,fill = factor(movie,levels=rev(movies)))) +
  geom_col()+
  coord_flip()+
  # scale_fill_manual(values=ipsum_pal()(8),name="Movie")+
  scale_fill_manual(values=rev(star_wars_col),name="")+
  theme_ipsum_rc(grid=F)+
  theme(legend.position = "bottom",
        plot.caption = element_text(color="grey"))+
  guides(fill = guide_legend(reverse=T))+
  labs(x="",y="Screen time (Minutes)",
       title="Screen time of Star Wars characters",caption="schochastics.net")

ggsave("time_character.png",height=12,width=5)

#most screen time per season ----
times_df %>% 
  group_by(movie) %>%
  top_n(10,time) %>% 
  ggplot(aes(x = reorder(character,time), y = time))+
  geom_col()+
  coord_flip()+
  facet_wrap(~movie,scales = "free")

#create list of plots
times_df_fil <- times_df %>% 
  group_by(movie) %>%
  top_n(10,time) %>% 
  ungroup()

myplots <- lapply(split(times_df_fil,times_df_fil$movie), function(x){
  
  x$character <- factor(x$character, levels=x$character[order(x$time,decreasing=F)])
  
  #make the plot
  p <-   ggplot(x,aes(x = character, y = time))+
    geom_col()+
    coord_flip()+
    theme_ipsum_rc(grid=F)+
    labs(x="",y="")#title=x$movie[1])
})
gridExtra::grid.arrange(c(myplots))
p <- do.call(gridExtra::arrangeGrob,(c(myplots, ncol=3)))
ggsave("char_per_season.png",plot=p,height=10,width=10)
