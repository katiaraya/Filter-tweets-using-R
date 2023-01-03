library (rtweet)
library (httpuv)
livetweets<- stream_tweets("")
dim(livetweets)

#Extract 100 tweets on "marketing"
auth_setup_default()
tweets_all <- search_tweets("marketing", n = 100,include_rts = TRUE, lang = "en")
tweets_all

# Check for count of replies
library(plyr)
count(tweets_all$in_reply_to_screen_name)

# Check for count of quotes
count(tweets_all$is_quote_status)

# Check for count of retweets
count(tweets_all$retweeted)
count(tweets_all$retweet_count)
#tweets_all$retweeted_status

#Extract tweets on "marketing" applying the -filter
# Apply the '-filter'
tweets_org <- search_tweets("marketing
                            -filter:retweets
                            -filter:quote
                            -filter:replies",
                             n = 100)
tweets_org

# Check for count of replies
count(tweets_org$in_reply_to_screen_name)

# Check for count of quotes
count(tweets_org$is_quote_status)

# Check for count of retweets
count(tweets_org$retweeted)

# Filter and extract tweets posted in Spanish
tweets_lang<- search_tweets("brand marketing", lang = "es")
View(tweets_lang)

head(tweets_lang$lang)

# Extract tweets with minimum 100 favorites and retweets
tweets_pop <- search_tweets("bitcoin min_faves:100 AND min_retweets:100")
tweets_pop

# Create a data frame to check retweet and favorite counts
counts <- tweets_pop[c("retweet_count","favorite_count")]
counts
head(counts)

# View the tweets
head(tweets_pop$text)

# Search for 1000 tweets on #fitness
tweet_fit <- search_tweets("#fitness", n = 1000)
tweet_fit

# Extract user information
user_fit<-users_data(tweet_fit)
user_fit

# View column names of the user data
names(user_fit)

# Aggregate screen_name, followers_count & friends_count
library(dplyr)
counts_df <- user_fit %>%
  group_by(screen_name) %>%
  summarize(follower = mean(followers_count),
            friend = mean(friends_count))
counts_df
head(counts_df)

# Create a column to calculate the golden ratio
counts_df$ratio<-((counts_df$follower)/(counts_df$friend))
counts_df$ratio 
head(counts_df$ratio)

# Sort the data frame in decreasing order of follower count
counts_sort <- arrange(counts_df, desc(follower))
counts_sort

# Select rows where the follower count is greater than 30000
counts_sort[counts_sort$follower>30000,]

# Select rows where the follower count is less than 2000
counts_sort[counts_sort$follower<2000,]

lst_playstation <- lists_users("PlayStation")
lst_playstation[,1:4]

# Extract 100 subscribers of the "gaming" list owned by "Playstation"
list_PS_sub <- lists_subscribers(slug = "gaming", owner_user = "PlayStation", n = 100)
list_PS_sub

# View screen names of the subscribers
list_PS_sub$screen_name

# Create a list of four screen names
users <- c("Morten83032201","ndugumr","WOLF210_Warrior","souransb")
# Extract user information
users_PS_gaming <- lookup_users(users)
users_PS_gaming

# Get overall current trending topics
trend_topics <- get_trends()
head(trend_topics$trend, 10)

# Extract locations of available twitter trends
trends_avail <- trends_available()
head(trends_avail)

# Get trending topics in the US
gt_US <- get_trends("United States")
gt_US
View(gt_US)

# Get trending topics in New York
gt_city <- get_trends("New York")
gt_city
head(gt_city)

# Aggregate trends and tweet volumes
library(dplyr)
trend_df <- gt_city %>%
  group_by(trend) %>%
  summarize(tweet_vol = mean(tweet_volume))
head(trend_df)

# Sort data frame on descending order of tweet volumes
trend_df_sort <- arrange(trend_df, desc(tweet_vol))
# View the most tweeted trends
head(trend_df_sort)

library(rtweet)
# Extract tweets on "#google" using search
search_tweets("#google", n = 18000, include_rts = FALSE)

# Extract tweets on "#camry" using search_tweets()
camry_st <- search_tweets("#camry", n = 18000, include_rts = FALSE)
camry_st

# Create a time series plot
ts_plot(camry_st, by = "hours", color = "blue")

# Convert tweet data into a time series object
camry_ts <- ts_data(camry_st, by = 'hours')
head(camry_ts)

# Rename the two columns in the time series object
names(camry_ts) <- c("time","camry_n")
head(camry_ts)

tesla_st <- search_tweets("#tesla", n = 18000, include_rts = FALSE)
tesla_ts <- ts_data(tesla_st, by = 'hours')
names(tesla_ts) <- c("time","tesla_n")
head(tesla_ts)

# Merge the two time series objects and retain "time" column
merged_df <- merge(tesla_ts, camry_ts, by = "time", all = TRUE)
head(merged_df)

# Stack the tweet frequency columns using melt() function
install.packages("reshape")
library(reshape)
melt_df <- melt(merged_df, na.rm = TRUE, id.vars = "time")
head(melt_df)

library(ggplot2)
# Plot frequency of tweets on Camry and Tesla
ggplot(data = melt_df,
       aes(x = time, y = value, col = variable)) +
  geom_line(lwd = 0.8)
