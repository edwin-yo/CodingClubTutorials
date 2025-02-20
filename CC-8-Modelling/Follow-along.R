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

# Practicing generalised linear models
# Poisson distribution ----
shag <- read.csv('shagLPI.csv', header = TRUE)

# Making a histogram to assess data distribution
shag.hist <- ggplot(shag, aes(pop))+
  geom_histogram()+
  theme.clean()
shag.hist

# Try a poisson distribution
shag.m <- glm(pop ~ year, family = poisson, data = shag)
summary(shag.m)
# From the summary, it can be seen that Shag abundance varies significantly based on the predictor year.

# Visualization of how European Shag abundance has changed over the years with a linear model fit with 95% confidence intervals.
shag.p <- ggplot(shag, aes(x = year, y = pop))+
  geom_point(colour = '#483D8B')+
  geom_smooth(method = glm, colour = '#483D8B', fill = '#483D8B', alpha = 0.6)+
  scale_x_continuous(breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005))+
  theme.clean()+
  labs(
    x = ' ',
    y = 'European Shag abundance')
shag.p

# Try the visualization again but with a poisson fit.
shag.p2 <- ggplot(shag, aes(x = year, y = pop)) +
    geom_point(colour = "#483D8B") +
    geom_smooth(
      method = "glm",
      method.args = list(family = poisson),  # Force Poisson GLM
      colour = "#483D8B", 
      fill = "#483D8B", 
      alpha = 0.6
    ) +
    scale_x_continuous(breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005)) +
    theme.clean() +
    labs(
      x = " ",
      y = "European Shag abundance"
    )
shag.p2
# Binomial distribution ----
Weevil_damage <- read.csv('Weevil_damage.csv')

# Making block a factor (a categorical variable)
Weevil_damage$block <- as.factor(Weevil_damage$block)

# Running the model
weevil.m <- glm(damage_T_F ~ block, family = binomial, data = Weevil_damage)
summary(weevil.m)

# Challenge! ----
ToothGrowth <- datasets::ToothGrowth
str(ToothGrowth)

## Are higher doses of vitamin C beneficial for tooth growth?----
# The response (tooth growth) is continous while the predictor (vitamin C dose) is discrete. This suggests an ANOVA analysis. Just like before, I will do this through the built-in linear model.
# But first, I need to make sure that the dose column is a factor.
ToothGrowth$dose2 <- factor(ToothGrowth$dose)
ToothGrowth.m <- lm(len ~ dose2, data = ToothGrowth)
summary(ToothGrowth.m)
ToothGrowth.m
# The p value is very small (<0.05).
# The adjusted R-squared is 0.69 which means that dose explains 69% of the variance.
# We can also visualize this in a boxplot.
(
  ToothGrowth.p <- ggplot(ToothGrowth, aes(dose2, len))+
    geom_boxplot(fill = '#CD3333', alpha = 0.8, colour = '#8D2323')+
    theme.clean()+
    theme(
      axis.text.x = element_text(size = 12, angle = 0)
    )+
    labs(
      x = 'Vitamin C dose [mg/day]',
      y = 'Growth [length]'
    )
)
# To answer the question, yes, higher doses of vitamin C result in higher tooth growth.

## Does the method of administration (orange juice, OJ, or ascorbic acid, VC) influence the effect of the dose? ----
# This question involves exploring whether introducing another predictor variable has an influence on the model.
# The introduced variable is the method of administration. The new linear model involves multiplying both predictor variables.
# The variable supp is already a factor so no need to change it.
ToothGrowth.m2 <- lm(len ~ dose2 * supp, data = ToothGrowth)
summary(ToothGrowth.m2)
# The coefficients suggest that orange juice (OJ) has a positive effect on tooth growth while ascorbic acid (VC) has a negative effect.
# The p-value is still below 0.05 and the adjusted R-squared is 0.77 which means that the predictor variables now explain 77% of the variance.
# Let's visualize this.
(
  ToothGrowth.p2 <- ggplot(ToothGrowth, aes(x = dose2, y = len))+
    geom_boxplot(alpha = 0.5, aes(colour = supp, fill = supp))+
    labs(
      x = 'Vitamin C dose [mg/day]',
      y = 'Length [length]',
    )+
    theme.clean()
)

## What would be the predicted tooth length of a guinea pig given 1 mg of vitamin C as ascorbic acid? ----
# From the linear model in the previous question, the result would be the intercept plus the appropriate interaction.
# The intercept (reference) represents OJ @ 0.5 mg/day is reported as 13.230.
# Then, OJ @ 1 mg/day is reported as 9.470.
# Then, the reference for ascorbic acid is suppVC (VC @ 0.5 mg/day) is reported as -5.250.
# Then, VC @ 1 mg/day is reported as -.680.
# Therefore, the predicted tooth length of a ginea pig given VC @ 1 mg/day is
# (13.230 + 9.470) + (-5.25 -.680) = 16.77.