# Needed libraries
library(agridat)
library(ggplot2)
library(dplyr)

# First model ----
# Loading the dataset from agridat
apples <- agridat::archbold.apple
head(apples)
summary(apples)

# Defining a ggplot2 theme for consistent and nice graphs.
theme.clean <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.position = "right")
}

# Turn the spacing data into a factor
apples$spacing2 <- as.factor(apples$spacing)

# Boxplot of yield due to spacing
(apples.p <- ggplot(apples, aes(x = spacing2, y = yield))+
  geom_boxplot(fill = '#CD3333', alpha = 0.8, colour = '#8B2323')+
  theme.clean()+
  theme(axis.text.x = element_text(size = 12, angle = 0))+
  labs(
    x = 'Spacing [m]',
    y = 'Yield [kg]'
  )
)

# From the plot, it is not clear that spacing makes a difference on yield.
# Therefore, let's try a model now
apples.m <- lm(yield ~ spacing2, data = apples)
summary(apples.m)

# More pratice, another model ----
sheep <- agridat::ilri.sheep
sheep <- filter(sheep, ewegen == 'R')   # there are confounding variables in this dataset that we don't want to take into account. We'll only consider lambs that come from mothers belonging to the breed "R".

head(sheep) # The model focuses on weanwt (wean weight) and weanage.

sheep.m1 <- lm(weanwt ~ weanage, data = sheep)
summary(sheep.m1)

# Introducing sex as a predictor variable.
sheep.m2 <- lm(weanwt ~ weanage*sex, data = sheep)
summary(sheep.m2)