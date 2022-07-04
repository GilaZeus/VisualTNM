# VisualTNM
Tools for visualising different cancer stage data.

## Grading
```
# install.packages("tidyverse")
# library(tidyverse)
coords <-
  dataset %>% 
  combine_diff(D) %>%                               # leave only grading of 1, 2, 3 and 4
  create_age_groups(Alter, Altersgruppe, 70) %>%    # Create age groups separated by age of 70
  group_by(Altersgruppe) %>%                        # Group by age
  calc_freq(D) %>%                                  # Calculate frequencies
  new_size() %>%                                    # Resize the dataset saving the grading's distribution
  create_diff_dataset(D, base_noise = 0.3)          # create dataset for plotting
```

`ggforce` is recomended for grading plot.
```
# install.packages("ggforce")
# library(ggforce)
ggplot(coords, aes(x, y, fill = D, group = -1L)) + 
  geom_voronoi_tile(expand = unit(-.3, 'mm'), radius = unit(1, 'mm')) + 
  facet_wrap(~ Altersgruppe, nrow = 2) + 
  geom_point(aes(size = nucl_r), alpha = 0.4)
```
![Grading plot](pics/Grading.png?raw=true "Title")

## Missing values

```
# install.packages("tidyverse")
# library(tidyverse)
plot_data <-
  dataset %>% 
  calculate_UICC(T, N, M) %>%         # Calculate UICC stages
  check_integrity(T, N, M, D, UICC)
```
And the graph's code:
```
ggplot(plot_data, aes(x = x, y = n, fill = full, label = paste(full, "\n", n, sep = ""))) +
  geom_col(position = position_stack(), width = 1) +     
  geom_text(size = 4, position = position_stack(vjust = 0.5)) +     
  theme_void() +     
  theme(legend.position = "none")
```
![Data Integrity](pics/Integrity.png?raw=true "Title")

If you have `treemapify`, you can also draw a treemap:
```
# install.packages("treemapify")
# library(treemapify)
plot_data <-
  x %>% 
  count_NAs(T, N, M, D)
ggplot(plot_data, aes(area = n, fill = value, label = paste(name, "\n", n, sep = ""))) +
  geom_treemap() +
  geom_treemap_text(place = "centre")
```
![Missing Values](pics/NAs.png?raw=true "Title")

## 
