Team Members: Jonathan Phan, Ishika Manghnani, Sakina Naqvi, Kuilin Jiang, Josh Ong

library(tidyverse)
library(moderndive)
spotify <- Spotify_SP23

# it seems that the more “acoustic” a song is, the less popular it will be
spotify %>% 
  ggplot(aes(x = acousticness, y = popularity)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

# it seems that the higher the "danceability" of a song, the more popular it is
spotify %>% 
  ggplot(aes(x = danceability, y = popularity)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

# it seems that the higher the energy of a song, the more popular it is
spotify %>% 
  ggplot(aes(x = energy, y = popularity)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

# it seems that the higher the "instrumentalness" of a song, the less popular it is
spotify %>% 
  ggplot(aes(x = instrumentalness, y = popularity)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

# it seems that the higher the "liveness" of a song, the less popular it is
spotify %>% 
  ggplot(aes(x = liveness, y = popularity)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

# it seems that the louder the song is, the more popular the song is
spotify %>% 
  ggplot(aes(x = loudness, y = popularity)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

# it seems that the higher the "speechiness" a song is, the lower the popularity is
spotify %>% 
  ggplot(aes(x = speechiness, y = popularity)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

# it seems that the higher the tempo is, the higher the popularity is
spotify %>% 
  ggplot(aes(x = tempo, y = popularity)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

# it seems that as the years progressed, "acousticness" lowered
spotify %>% 
  ggplot(aes(x = year, y = acousticness)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

# it seems that as the years progressed, "danceability" increased
spotify %>% 
  ggplot(aes(x = year, y = danceability)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

# it seems that as the years progressed, "energy" increased
spotify %>% 
  ggplot(aes(x = year, y = energy)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

# it seems that as the years progressed, "instrumentalness" lowered
spotify %>% 
  ggplot(aes(x = year, y = instrumentalness)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

# it seems that as the years progressed, "liveness" lowered
spotify %>% 
  ggplot(aes(x = year, y = liveness)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

# it seems that as the years progressed, "loudness" increased
spotify %>% 
  ggplot(aes(x = year, y = loudness)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

# it seems that as the years progressed, tempo increased
spotify %>% 
  ggplot(aes(x = year, y = tempo)) +
  geom_point(alpha=0.01, color = "#b9bbf7") +
  geom_smooth(method="lm", se=FALSE, color = "red")

summary(Spotify_23)
# removing the outliers
Spotify_23 <- Spotify_23 %>%
  mutate(duration_IQR = ifelse(duration_ms > 172513 & duration_ms < 263984, duration_ms, NA))
# observing the IQR without outliers
summary(Spotify_23$duration_IQR)
# short if duration of the song is shorter than 25th percentile (190s)
# long if duration of the song is longer than 75th percentile (233s)
# middle if duration is close to median (212s)
Spotify_23 <- Spotify_23 %>% 
  mutate(duration_cat=ifelse(duration_ms<190827, "Short",
           ifelse(duration_ms<233600, "Middle", "Long")))

Spotify_23 %>% 
  ggplot(aes(x = year, y=popularity, color=duration_cat)) +
  geom_point(alpha=0.7, size=2) +
  geom_smooth(method="lm", se=FALSE) 
# This graph visualizes the duration of songs chronologically
# Songs with shorter duration started to before 1975, longer songs began to take 
# over. After 2015, the duration for popular songs became shorter and shorter again.
# There is definitely a decreasing trend of producing songs that are longer than 263s

spotify.numeric <- select_if(Spotify_SP23, is.numeric)

# Generate correlation matrix, round result to two decimal places
spotify.cor <- round(cor(spotify.numeric), 2)
spotify.cor

# Melt correlation matrix for better visualization
spotify.cor.melt <- melt(spotify.cor)

# Basic heat map
# Focus on the popularity column, top 3 positive and negative correlations
# Year correlation of 0/88 is interesting, need to explore this more
spotify.cor.melt %>% 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "light blue", 
                       mid = "white", midpoint = 0) +
  geom_text(aes(x = Var1, y = Var2, label = value), size = 3) +
  theme(axis.text.x = element_text(size = 8, angle = 30, hjust = 0.8))

# Checking the IQR of "danceability"
summary(spotify)
# Create a new column that stores values that are within the IQR of "danceability".
Spotify_SP23 <- Spotify_SP23 %>%
  mutate(danceability_IQR = ifelse(danceability > 0.4150 & danceability < 0.6640, danceability, NA))

lm_int_yr_pop_energy = lm(popularity~year*energy, spotify)
summary(lm_int_yr_pop_energy)
#Created a interactive regression model between popularity, year and energy
#For low energy, Population_hat = -1541 + 0.79*year
#For high energy, Population_hat = -1223.6 + 0.64*year
#R^2: 77.4% of the model is explained 
#All variables are significant to the model and interacting with each other and so this is the best types of model

#Create a subset for energy
#Any values which are above the mean energy = 0.4928
spotify_ener_mean = spotify %>% 
  filter(energy > 0.4928)
#There are 82060 observations for this subset as opposed to 166330 originally.
spotify_ener_mean %>% 
  ggplot(aes(x = energy, y = population)) +
  geom_point(alpha=0.1) +
  geom_smooth(method="lm", se=FALSE)
#Created another subset for energy
#Any values which are above the upper quartile energy
spotify_ener_q3 = spotify %>% 
  filter(energy>0.7130)
#41549 observations
spotify_ener_q3 %>% 
  ggplot(aes(x=energy, y=popularity))+
  geom_point(alpha=0.1, color = "lightblue")+
  geom_smooth(method="lm", se = FALSE)
#After the upper quartile, the most popular songs actually have slightly lower energy.

###DATA VISUALIZATION###
#Created new subset for roughly top 4 songs (most popular) 
spotify_mostpop = spotify %>% 
  filter(popularity>95)
spotify_num_pop = select_if(spotify_mostpop, is.numeric)
variable.cor1 = round(cor(na.omit(spotify_num_pop)), digits = 2)
variable.cor1
#All 4 most popular songs were produced in 2020 and 75% of them had a high energy
#These top 4 most popular songs have a very close correlation with duration_ms (0.95), energy (0.85) and 
#mode (0.95)
#For businesses their goal should be to try and be in the top 5 most popular songs and so I believe this data will be useful.

spotify_mostpop %>% 
  ggplot(aes(y=popularity, x = duration_ms))+
  geom_point(color = "blue")+
  geom_smooth(method="lm", se = FALSE)+
  ggtitle("Relationship with Popularity and Duration of the Song")+
  labs(x="Duration of Song (milliseconds)", y="Popularity")
#This graph shows the strong positive correlation between popularity and duration_ms
#The most popular songs tend to be over about 173,000 milliseconds long (2.88 minutes)
#The most popular song is 200,000 milliseconds long (3.33 minutes)
#A recommendation to businesses would be to keep the duration of a song from 2.88-3.33 minutes.
#Data also shows that very long songs do not tend to be as popular as well.

spotify_mostpop %>% 
  ggplot(aes(y=popularity, x = energy, color = mode))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE)+
  ggtitle("Relationship with Popularity and Energy")+
  labs(x="Energy", y="Popularity")
#Although mode has a high correlation with popularity, we can see from the plots that 50% of top 4 popular songs 
#are in a major key whilst the other 50% are in minor key.
#Therefore we can conclude that mode has no effect on the popularity of the song.
#In regards to energy, there is a strong positive correlation between popularity and energy of songs. 
#The data shows that songs with energy around 0.7 or between 0.35 and 0.55 are the most popular.

table_pop_energy = table(spotify_mostpop$energy, spotify_mostpop$popularity)
table_pop_energy
proportions(table_pop_energy, margin = 2)

lm_mostpop1 = lm(popularity~duration_ms, spotify_mostpop)
summary(lm_mostpop1)
#Created a basic regression model for popularity and duration
#R^2 value: 90.74% of the model is explained, adjusted R^2 value:86.1%
#Both variables and the model are significant as the p-value is <0.05 
#Popularity_hat = 76.62 + 0.00012*duration_ms

lm_mostpop2 = lm(popularity~duration_ms+energy, spotify_mostpop)
summary(lm_mostpop2)
#Created a multiple regression model for popularity, duration and energy of the song
#R^2 value:93.54% of the model is explained, adjusted R^2 value: 80.63%
#None of the variables or the model are significant because the p-value is >0.05
#For no energy, Popularity_hat = 158.5 + 9.226e-05 * duration_ms
#For energy, Popularity_hat = 82.807 + 9.226e-05 * duration_ms

lm_mostpop3 = lm(popularity~duration_ms*energy, spotify_mostpop)
summary(lm_mostpop3)
#Created a multiple regression interactive model for popularity, duration and energy of the song
#R^2 value: 100%
#There are no residuals 
#For no energy, Popularity_hat = -12 + 0.00062*duration*ms
#For energy, Popularity_hat = 125.5 - 0.00015*duration*ms

#From these findings, I would make the recommendations for the business to focus on producing song with a high
#energy level and with a duration within the recommended range

# Generate residuals and Normal QQ plot
lm_mostpop1 = lm(popularity~duration_ms, spotify_mostpop)
summary(lm_mostpop1)

lm_mostpop1_points <- get_regression_points(lm_mostpop1)
lm_mostpop1_points <- lm_mostpop1_points %>% 
  mutate(stdres = rstandard(lm_mostpop1))

# Generate the Normal QQ plot
lm_mostpop1_points %>% 
  ggplot(aes(sample = stdres)) +
  geom_qq(color = "blue") +
  geom_qq_line(color = "red") +
  ggtitle("Normal QQ Plot") +
  labs(x = "Theoretical Quantile", y = "Stdres") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot the residual plot using the standardized residuals
lm_mostpop1_points %>% 
  ggplot(aes(x = duration_ms, y = stdres)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Residual Plot") +
  theme(plot.title = element_text(hjust = 0.5))
