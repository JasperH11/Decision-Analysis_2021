# Seminar 2 - Data visualisation

# Participants data ----

# Load data
library(readr)

urlfile = "https://raw.githubusercontent.com/CWWhitney/teaching_R/master/participants_data.csv"

participants_data <- read_csv(url(urlfile))

# Bar plot
plot(participants_data$academic_parents)

# Box plot
plot(participants_data$academic_parents, participants_data$days_to_email_response)


# Load ggplot
library(ggplot2)

# Participants data
ggplot(data = participants_data,
       aes(x = letters_in_first_name,
           y = days_to_email_response,
           color = academic_parents,
           size = working_hours_per_day)) + 
  geom_point()


# Iris data ----

ggplot(data = iris,
       aes(x = Sepal.Length,
           y = Petal.Length,
           color = Species,
           size = Petal.Width)) +
  geom_point()


# Diamonds data ----  
ggplot(data = diamonds,
       aes(x = carat,
           y = price,
           alpha = 0.2)) + 
  geom_point()

ggplot(data = diamonds,
       aes(x = log(carat),  # log() added
           y = log(price),
           alpha = 0.2)) + 
  geom_point()


library(dplyr)
dsmall <- top_n(diamonds, n = 100)

ggplot(data = dsmall,
       aes(x = carat, 
           y = price, 
           color = color)) +  # Mark colour of the diamonds by colours in the plot
  geom_point()

ggplot(data = dsmall,
       aes(x = carat, 
           y = price, 
           shape = cut)) +  # Mark cut of the diamonds by shape in the plot
  geom_point()


ggplot(data = diamonds,
       aes(x = carat, 
           y = price, 
           alpha = I(0.1),  # Inhibit interpretation of "0.1", treat as constant argument
           color = I("blue"))) +  # Inhibit interpretation of "blue", treat as constant argument
  geom_point()


# geom options 
ggplot(data = dsmall,
       aes(x = carat, 
           y = price)) +
  geom_point() + 
  geom_smooth()

ggplot(data = dsmall,
       aes(x = carat, 
           y = price)) +
  geom_point() + 
  geom_smooth(method = 'glm') # Generalised linear model 


# Boxplots
ggplot(data = dsmall,
       aes(x = color, 
           y = price/carat,
           alpha = I(0.2))) + 
  geom_boxplot() + 
  geom_jitter() # Adding jitter points


# Histograms
ggplot(data = diamonds,
       aes(x = carat,
           fill = color,  # colour of the diamonds
           alpha = I(0.3))) +  # set opacity
  geom_density()



# mpg data ---- 

# subset
ggplot(data = mpg,
       aes(x = displ,
           y = hwy,
           color = cyl)) + 
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = mpg,
       aes(x = displ,
           y = hwy,
           color = factor(cyl))) +  # cylinders are now regarded as a numeric factor
  geom_point() +
  geom_smooth(method = "lm")


# mtcars data ---- 

# Set title and labels
ggplot(mtcars, aes(mpg, hp,
                   col = gear)) + 
  geom_point() + 
  ggtitle("My Title") + 
  labs(x = "The x label",
       y = "The y label",
       col = "Legend title")


# Using dplyr, ggplot2 and reshape2 ----
library(reshape2)

part_data <- select_if(participants_data, is.numeric) # Select only numeric categories  

cormat <- round(cor(part_data), 1)   # Round correlation matrix of part_data to 1 digit 
melted_cormat <- melt(cormat)   # Makes data more usable 

# Visualise as tile plot 
ggplot(data = melted_cormat,
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45,  # Tilt text by 45 degrees
                                   hjust = 1))  # Add tick marks 


# Export figures ---- 
png(file = "cortile.png",
    width = 7,
    height = 6,
    units = "in",
    res = 300)

ggplot(data = melted_cormat,
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45,  
                                   hjust = 1))  
dev.off()


# gganimate and datasauRus ---- 
library(datasauRus)
library(gganimate)

ggplot(datasaurus_dozen,
       aes(x = x, 
           y = y)) + 
  geom_point() + 
  theme_minimal() + 
  transition_states(dataset, 3, 1) + 
  ease_aes('cubic-in-out')             # Gives a 100 images instead of animation 


ggplot(data = dsmall,
       aes(x = carat,
           y = price,
           cplor = color)) + 
  geom_line() + 
  transition_reveal(carat) + 
  ease_aes("linear") + 
  labs(title = 'Diamond carat: {frame_along}')
