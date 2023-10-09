#install and load gapminder package
install.packages("gapminder")
library('gapminder')
data('gapminder')

#display gapminder data
View(gapminder)

#install and load dplyr package
library('dplyr')

#comparing the variance in south africa and ireland
View(gapminder)
df1 <- gapminder %>%
  select(country, lifeExp) %>%
  filter(country == "South Africa" | country == "Ireland")

#perform t-test
t.test(data = df1, lifeExp ~ country)

#plotting a gdpPercap vs LifeExp graph for each continent
library(ggplot2)

gapminder%>%
  filter(gdpPercap<50000)%>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp , col = continent, size = pop))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = lm)+
  facet_wrap(~continent)
  