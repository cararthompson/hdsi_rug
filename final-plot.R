# Final plot

# Libraries
library(tidyverse)
library(ggtext)

# Line up the data
penguins <- palmerpenguins::penguins %>%
  mutate(banana_quantity = case_when(species == "Adelie" & island == "Biscoe" ~ 1,
                                     species == "Adelie" & island == "Dream" ~ 0.6,
                                     species == "Adelie" & island == "Torgersen" ~ 0,
                                     TRUE ~ 1))

penguin_summaries <- palmerpenguins::penguins %>%
  group_by(species) %>%
  summarise(bill_depth_mm = mean(bill_depth_mm, na.rm = TRUE),
            bill_length_mm = mean(bill_length_mm, na.rm = TRUE)) %>%
  mutate(commentary = case_when(species == "Adelie" ~
                                  "The Adelie penguins tried varying the amount of banana in the mix. Turns out, even a hint of green banana is detrimental to yumminess!",
                                species == "Gentoo" ~
                                  "Over-ripe bananas and typically shorter baking times.",
                                TRUE ~ "Ripe bananas and slightly longer cooking times."))

# lining up variable names so can keep same aesthetics
penguin_highlights <- palmerpenguins::penguins_raw %>%
  janitor::clean_names() %>%
  rename(bill_depth_mm = culmen_depth_mm,
         bill_length_mm = culmen_length_mm) %>%
  filter(bill_length_mm %in% c(max(bill_length_mm, na.rm = TRUE),
                               sort(bill_length_mm, decreasing = TRUE)[2],
                                 min(bill_length_mm, na.rm = TRUE))) %>%
  arrange(bill_length_mm) %>%
  mutate(species = gsub("(.) (.*)", "\\1", species),
         commentary = case_when(bill_length_mm == max(bill_length_mm) ~
                                  paste0("Our star baker is **", individual_id,
                                         "**, a ", species, " from ", island,
                                         ". Congratulations, ", individual_id, "!"),
                                bill_length_mm == sort(bill_length_mm, decreasing = TRUE)[2] ~
                                  paste0("Our runner up is a ", species,
                                         " from ", island, ": **", individual_id,
                                         "**, proving that ripe and over-ripe bananas are both good options!"),
                                TRUE ~ paste0("**", individual_id,
                                              "**, did not have a good baking day. The combination of short cooking time and green bananas probably didn't help!")),
         label_x = c(15, 18.15, 16.45),
         label_y = c(34, 57, 59),
         h_align = case_when(label_x < bill_depth_mm ~ 1,
                             TRUE ~ 0),
         arrow_x_end = case_when(label_x < bill_depth_mm ~ bill_depth_mm - 0.1,
                                 TRUE ~ bill_depth_mm + 0.1),
         arrow_y_end = case_when(label_y < bill_length_mm ~ bill_length_mm - 0.1,
                                 TRUE ~ bill_length_mm + 0.1))

# Set colour scheme
banana_colours <- list("Adelie" = "#89973d",
                            "Chinstrap" = "#e8b92f",
                            "Gentoo" = "#a45e41")

# To build choose the grey/text colours
dark_text <- monochromeR::generate_palette(
  banana_colours$Chinstrap, "go_darker",
  n_colours = 2)[2]

light_text <-  monochromeR::generate_palette(
  dark_text, "go_lighter",
  n_colours = 3)[2]

banana_colours <- list("Adelie" = "#89973d",
                       "Chinstrap" = "#e8b92f",
                       "Gentoo" = "#a45e41",
                       "dark_text" = dark_text,
                       "light_text" = light_text)
# Plot

# aesthetics that apply to all
ggplot(penguins,
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           colour = species)) +
  # visualising data
  geom_point(aes(alpha = banana_quantity),
             show.legend = FALSE) +
  geom_textbox(data = penguin_summaries,
               aes(label = paste0("**Team ", species, "**",
                                  "<br><span style = \"color:",
                                  banana_colours$light_text,
                                  "\"</span>", commentary)),
               family = "DM Sans",
               size = 4,
               width = unit(12, "line"),
               alpha = 0.9,
               box.colour = NA) +
  geom_textbox(data = penguin_highlights,
               aes(label = commentary,
                   x = label_x,
                   y = label_y,
                   halign = h_align,
                   hjust = h_align),
               show.legend = FALSE,
               family = "DM Sans",
               size = 3.2,
               fill = NA,
               box.colour = NA) +
  geom_curve(data = penguin_highlights,
             aes(x = label_x, xend = arrow_x_end,
                 y = label_y, yend = arrow_y_end),
             arrow = arrow(length = unit(0.1, "cm")),
             curvature = list(0.15),
             alpha = 0.5) +
  # colours
  scale_colour_manual(values = banana_colours) +
  scale_alpha(range = c(0.2, 0.9)) +
  # labels
  labs(title = paste0("Banana loaf tastes best when baked with <span style=\"color:",
                      banana_colours$Chinstrap, "\">ripe</span> or <span style=\"color:",
                      banana_colours$Gentoo, "\">over-ripe</span> bananas"),
       subtitle = "The Palmer Penguins carried out an experiment using bananas of different ripeness.<br>
       The Adelie penguins were given unripe bananas, Gentoos were given over-ripe bananas<br>
       and Chinstraps were given yellow bananas.
       <br>Each penguin was left to choose their own cooking time.",
       x = "Baking time",
       y = "Yumminess",
       caption = "Data from {palmerpenguins}; misused for illustration purposes.") +
  guides("none") +
  # theming
  theme_minimal(base_size = 12) +
  theme(text = element_text(family = "DM Sans", colour = banana_colours$light_text,
                            lineheight = 1.2),
        plot.title = element_markdown(size = 18, family = "Poppins", colour = banana_colours$dark_text, face = "bold"),
        panel.grid = element_line(colour = "#F6F6F5"),
        axis.text = element_text(size = 6),
        plot.caption = element_text(size = 6),
        legend.position="none")

# Peer review; caption typically in text under graph, so focussing on in-graph text

