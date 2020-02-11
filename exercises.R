library(tidyverse)

budget <- read_csv("data/2020-budget.csv")

# preview the tibble
glimpse(budget)

# Look at only Dept. of Treasury
treasury <- filter(budget, portfolio == "Queensland Treasury")

# budget distribution example from slides
ggplot(data = budget, aes(x = budget_2019_20)) +
  geom_histogram() +
  labs(
    title = "Distribution of 2020 Allocated Budget",
    x = "",
    y = "Number of projects"
  ) +
  scale_x_continuous(labels = scales::dollar) 



# plot treasury projects
# save into an object
projects_plot <- treasury %>% 
  ggplot(aes(project_name, budget_2019_20)) +
  geom_col()

# View the plot by printing it out
projects_plot

# Flip the axes to make it more legible
# add a `coord_flip()` layer
projects_plot +
  coord_flip()

# Make the budget axis show dollar ammounts
# More on scales:
# https://ggplot2.tidyverse.org/reference/scale_continuous.html
# https://scales.r-lib.org/

projects_plot + 
  coord_flip() +
  scale_y_continuous(labels = scales::dollar)

# Portofolio based summary statistics
budget_summary <- budget %>% 
  group_by(portfolio) %>% 
  summarise(n = n(),
            avg_budget = mean(estimated_cost, na.rm = TRUE),
            total_budget = sum(estimated_cost, na.rm = TRUE)) 

budget_summary

# Plot project level information
budget_summary %>% 
  # reorder the portfolio column based on the average budget column
  mutate(portfolio = fct_reorder(portfolio, avg_budget)) %>% 
  ggplot(aes(portfolio, avg_budget)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Average project budget", y = "", x = "")
