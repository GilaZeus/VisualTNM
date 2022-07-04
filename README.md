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
ggplot(y, aes(x, y, fill = D, group = -1L)) + 
  geom_voronoi_tile(expand = unit(-.3, 'mm'), radius = unit(1, 'mm')) + 
  facet_wrap(~ Altersgruppe, nrow = 2) + 
  geom_point(aes(size = nucl_r), alpha = 0.4)
```
![Grading plot](pics/Grading.png?raw=true "Title")

