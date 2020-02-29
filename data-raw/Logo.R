## code to prepare `Logo` goes here

library(hexSticker)
library(ggplot2)
library(cowplot)
require(magick)
map<-ggplot()+geom_sf(data=Delta, color="white", fill="white")+ theme_map()

p<-ggdraw() +
  draw_image("data-raw/Logo/Space.jpg") +
  draw_plot(map, x=0.13, y=0.1, scale=0.3)

ggsave(plot=p, filename="data-raw/Logo/Sticker_background.png", device="png", width = 18.56, height=12.3, units="in")

p<-image_read("data-raw/Logo/Sticker_background.png")
p2<-image_crop(p, "5068x3690+100")
image_write(p2, "data-raw/Logo/Sticker_background.png")

logo<-sticker("data-raw/Logo/Sticker_background.png",
        package="spacetools", p_size=20, s_width=0.9, s_y = 0.88, s_x = 1.0, asp=1.373442,
        h_fil="black", h_color="dodgerblue1", h_size = 1.5,
        white_around_sticker = TRUE,
        filename="data-raw/Logo/Sticker.png")
usethis::use_logo("data-raw/Logo/Sticker.png")
