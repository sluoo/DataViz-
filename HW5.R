library(tidyverse)
library(forcats)
library(plotly)
library(wordcloud)
library(ggrepel)
library(countrycode)

df <-  read_csv("ramen-ratings_edit.csv") 
df <- (df %>% select(-1,-7))

#unique values in each column create factors
df %>% summarise_all(funs(n_distinct)) 

#File says 3 ramen unrated, remove these 3 rows. 
df1 <- (df 
  %>% filter(Stars != "Unrated")
  %>% mutate(Country = factor(Country),Brand=factor(Brand),Style = factor(Style))
  %>% mutate(Stars = as.integer(Stars)))

### PLOT 1 ### 

SumVariety <- (df1 
               %>% group_by(Brand)
               %>%count(Variety)
               %>% mutate(TotalVariety=sum(n))
               %>% select(Brand,TotalVariety)
               %>% distinct(Brand,TotalVariety))

#top producers of ramen based on variety
set.seed(15)
wc <- wordcloud(words = SumVariety$Brand, freq = SumVariety$TotalVariety, 
          min.freq = 8,
          max.words=300, 
          scale = c(6,0.5),
          random.order=FALSE, rot.per=0.30, 
          colors=brewer.pal(9 ,"Dark2"), res=500)


### PLOT 2 ###

SumBrand <- (df1 
               %>% group_by(Country)
               %>%count(Brand)
               %>% mutate(TotalBrand=sum(n))
               %>% select(Country,TotalBrand)
               %>% distinct(Country,TotalBrand))  #remove duplicates

#avg rating of ramen based on country
ratingBrand <- (df1 
                  %>% group_by(Country)
                  %>% summarise(avgRating = mean(Stars)))


#combine for new data frame 
df3 <- (data.frame(SumBrand,AvgRateCountry = ratingBrand$avgRating) 
        %>% tbl_df())

#recode countries
CountryCode <- countrycode(df3$Country, "country.name", "iso3c")
df4 <- tbl_df(data.frame(df3,CountryCode))

p1 <- df4 %>% 
  ggplot(aes(x=TotalBrand,y=AvgRateCountry)) + 
  geom_point(aes(size=TotalBrand, color=Country, 
                 text=paste("country:",Country)),alpha=0.5) + 
  geom_text(aes(size=TotalBrand, label=CountryCode), color="black", alpha=1, nudge_y = 0.1) + 
  geom_hline(yintercept = mean(df4$AvgRateCountry),alpha=0.3,size=1) + 
  labs(title = "Rating of Ramen",
       x = "Total Number of Brands in Each Country",
       y = "Average Rating of Ramen by Country (Scale: 1 to 5)") + 
  theme_minimal() 


p2 <- p1 + scale_size_area(max_size = 8) + theme(legend.title = element_blank())

ggplotly(p2)















               




