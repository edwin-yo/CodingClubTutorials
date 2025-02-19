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

# Visualizing the relationship
sheep.p <- ggplot(data = sheep, mapping = aes(x = weanage , y = weanwt))+
  geom_point(aes(colour = sex))+
  labs(
    x = 'Age at weaning [days]',
    y = 'Wean weight [kg]'
  )+
  stat_smooth(method = 'lm', aes(fill = sex, colour = sex))+
  scale_colour_manual(values = c('#FFC125', '#36648B'))+
  scale_fill_manual(values = c('#FFC125', '#36648B'))+
  theme.clean()
sheep.p

# Checking that the residuals are normally distributed.
apples.resid <- resid(apples.m)   # Extracting the residuals.
shapiro.test(apples.resid)   # Using the Shapiro-Wilk test.
# The null hypothesis of normal distribution is accepted: there is no significant difference (p > 0.05) from a normal distribution.

# Checking for homoscedasticity.
bartlett.test(apples$yield, apples$spacing2)
bartlett.test(yield ~ spacing2, data = apples) # Note that these two ways of writing the code give the same results.
# The null hypothesis of homoscedasticity is accepted.

# Examination of the model through a plot.
while (FALSE) {
plot(apples.m) #   Need to hit Enter 4 times. It will produce 4 plots.
}
# Produced plots:
# 1. Residuals versus fitted values.
# 2. a Q-Q plot of standardized residuals.
# 3. a scale-location plot (square roots of standardized residuals versus fitted values).
# 4. a plot of residuals versus leverage that adds bands corresponding to Cook's distances of 0.5 to 1.

# Practicing generalised linear models ----

