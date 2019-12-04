library(gapminder)
library(ggplot2)
library(gganimate)
data(gapminder)
p <- ggplot(
  gapminder,
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p + transition_time(year)



df <- rbind(
  data.frame(group = c("A","B","C"), values = c(3,2,4), frame = rep('a',3)),
  data.frame(group = c("A","B","C"), values = c(5,3,7), frame = rep('b',3))
)
ggplot(df, aes(group, values, fill = group)) + 
  geom_col(position = "identity") + 
  transition_states(frame, .02, .001) + 
  ease_aes('cubic-in-out')

file_renderer(dir = ".", prefix = "gganim_plot", overwrite = FALSE)


animate(ani_tst, fps = 2,renderer = ffmpeg_renderer)


