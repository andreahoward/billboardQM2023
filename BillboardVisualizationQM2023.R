
library(tidyr)
library(stringr)
library(ggplot2)
library(viridis)

data(billboard)
# Song rankings for Billboard Top 100 in the year 2000
# The data() function loads these data from the tidyr package

# Use the pivot_longer function to convert these data to
# long form (currently, each week on the chart is a separate variable)
# We can also use the str_extract() function from the stringr package
# to grab the week number from what started as the column name.
# The new variable "week" is converted to numeric format
# and "track" is formatted as a factor. 
# For this example, I've deleted all rows with no value for "position"
# (i.e., weeks when a song wasn't on the chart) and all weeks
# after the 20th. 
songs <- billboard %>% 
  pivot_longer(cols = c(wk1:wk76), names_to = "week", values_to = "position") %>% 
  mutate(week = as.numeric(str_extract(week, "\\d{1,2}")),
         track = as.factor(track)) %>% 
  filter(!is.na(position) & week <=20) 

# The FIRST plot (Figure 1) is a simple graph of all songs by their week 
# on the Billboard Top 100 chart. 

ggplot(songs, aes(x = week, y = position)) +
  geom_line(aes(group = track),   # the group argument draws a separate line for each song
            colour = "black") +
  scale_y_continuous(name = "Chart Position",
                     trans = "reverse",   # this argument places lower numbers at the top of the scale
                     breaks = c(1,seq(10,100,10))) +
  scale_x_continuous(name = "Number of Weeks on the Chart",
                     breaks = seq(1,20,1)) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 16))

# The SECOND plot (Figure 2) highlights songs by Destiny's Child.
# First I need to make a second dataset containingy ONLY
# the data for these songs:

destiny <- songs %>% 
  filter(artist == "Destiny's Child")

# Now I can alter the above plot code to highlight the secondary data

ggplot(songs, aes(x = week, y = position)) +
  geom_line(aes(group = track),
            colour = "grey85") +  # choose a light colour to make the main data fade to the back
  geom_line(data = destiny,       # use the geom_line function again, but specify the secondary data
            aes(colour = track),  # I decided to choose a different colour for each song
            size=1.5) +
  scale_colour_viridis_d(name = NULL,          # using the viridis palettes, I assign colours
                         option = "plasma",    # for the individual song trajectories.
                         begin=.2, end=.9) +
  scale_y_continuous(name = "Chart Position",
                     trans = "reverse",
                     breaks = c(1,seq(10,100,10))) +
  scale_x_continuous(name = "Number of Weeks on the Chart",
                     breaks = seq(1,20,1)) +
  theme_classic() +
  theme(legend.position = "top",
        axis.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 12))

# The THIRD plot (Figure 3) highlights all songs that made it to Number One
# on the Billboard chart. Again, a secondary dataset is required.
# Using the group_by() function, I create a new variable that indicates, 
# for each track, whether the song ever made it to Number One. 
# Then, I retain ONLY the rows containing Number One songs.
 
numberones <- songs %>% 
  group_by(track) %>% 
  mutate(numone = ifelse(min(position)==1,1,0)) %>% 
  ungroup() %>% 
  filter(numone == 1)

# Now I re-make the plot, highlighting different secondary data:

ggplot(songs, aes(x = week, y = position)) +
  geom_line(aes(group = track),
            colour = "grey85") +
  geom_line(data = numberones,
            aes(group = track),
            colour= "royalblue4",  # I decided to make these tracks all one colour
            size=1) +
  scale_y_continuous(name = "Chart Position",
                     trans = "reverse",
                     breaks = c(1,seq(10,100,10))) +
  scale_x_continuous(name = "Number of Weeks on the Chart",
                     breaks = seq(1,20,1)) +
  theme_classic() +
  theme(legend.position = "top",
        axis.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 12))

# Don't forget to save your image! 
# If you save your image as a PDF, you will 
# preserve lossless resolution
# (your figure won't get blurry as you zoom in).

ggsave(filename = "Number1s.pdf", device = "pdf")
