library(tidyverse)
library(forcats)
library(plotly)
library(wordcloud)
library(ggrepel)
library(countrycode)

## BMB: can combine these lines into a single line
##  (that's the point of piping).
## Also, you should almost always use names rather than numbers
##  (positional indices) to select - easier to read and more robust
df <-  (read_csv("ramen-ratings_edit.csv")
    %>% select(-c("Review #","Top Ten"))
)    
## df <- (df %>% select(-1,-7))



#unique values in each column create factors
df %>% summarise_all(funs(n_distinct))
## BMB: I get "funs() is soft deprecated as of dplyr 0.8.0 ..."

#File says 3 ramen unrated, remove these 3 rows. 
df1 <- (df 
    %>% filter(Stars != "Unrated")
    ##  %>% mutate(Country = factor(Country),Brand=factor(Brand),Style = factor(Style))
    ## BMB: slightly more compact.
    ##  You can also do this upstream, when reading the data
    %>% mutate_at(c("Country","Brand","Style"), factor)
    %>% mutate_at("Stars",as.integer)
)

### PLOT 1 ### 

SumVariety <- (df1 
               %>% group_by(Brand)
               %>% count(Variety)
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

gg1 <-( SumVariety
    %>% filter(TotalVariety>=8)
    %>% ungroup()
    %>% mutate(Brand=fct_reorder(Brand,TotalVariety))
    %>% ggplot(aes(TotalVariety,Brand))+
        geom_point() +
        scale_x_log10()
)
print(gg1)  ## this isn't TERRIBLE, but I agree that the wordcloud is prettier
## (also lots of things to improve here)

## BMB: I get lots of warnings.  These probably aren't your fault
## ('res' is not a graphical parameter)

## BMB: wordcloud() is a base R function - it draws the plot
##  directly, doesn't return a value
wc  ## BMB: this is null.

### PLOT 2 ###

SumBrand <- (df1 
               %>% group_by(Country)
               %>% count(Brand)
               %>% mutate(TotalBrand=sum(n))  ## BMB: do you want summarise() here?
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


ggplotly(p2) %>% hide_legend()

## BMB: not bad.  You have some extra tooltips there ... I got rid of the legend for you


######## 
# This data was retrived from Kaggle: https://www.kaggle.com/residentmario/ramen-ratings

# NOTE: Dubai, Holland and Sarawak are not countries so I combined them with their respected 
# countries in the data. So please refer to ramen-ratings_edit.csv file in my respository. 

#Plot 1: 
# I wanted to show which brands produced the most variety of ramen. It was challenging with 
#355 brands so I chose a word cloud (I also wanted to experiment with this package). It 
#doesn’t convey much but it highlights the main brands. Originally it was very cluttered and messy so I 
#removed brands that had less than 8 types of ramen.  

## BMB: OK. How many brands 

#Plot 2: 
#The word cloud above lacks information, it doesn't tell us much about the country or overall rating. 
#In this plot, it shows that Asian countries along with the United States dominate the ramen industry.
#They are the largest producers 
#and rated the most highly with most being above the average line. Initially I plotted a regular ggplot and 
#labeled it using ggrepel but it was messy even after abbreviating the country names. An interactive plot 
#was a much better choice. The legend should be organized by size but after many attempts, I can’t 
#seem to figure it out. Also, I wanted each country to be represented by a different color. I'm 
#not sure why the colors are repeated. Is there a restriction to the palette I chose?
#For another improvements, I could represent countries in the same region with the same color and 
#indicate it in the legend. Rather than having the countries listed in the legend. 

## BMB: I think you just can't see the difference in the colours (if you look e.g. at the differences
##  between the Taiwan and Thailand tooltips, you can see that there is actually a difference).
## I agree that representing regions by colour would be better; I got rid of the legend for you.

## score: 2.25










               




