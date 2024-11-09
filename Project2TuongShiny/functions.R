

mobdevdata <- as.tibble(read.csv("/Users/User/repos/Project2Tuong/user_behavior_dataset.csv", header = TRUE))

numeric_vars <- c("Battery Drain (per mAh)" = "Battery.Drain..mAh.day.",
                  "Data Usage (per MB)" = "Data.Usage..MB.day.", 
                  "Screen Time (per hour)" = "Screen.On.Time..hours.day.")

category_vars <- c("Operating System" = "Operating.System",
                   "Genders" = "Gender")

combined_var <- c(category_vars, numeric_vars)


one_way_table <- function(data, var1) {
  table(data[var1])
}


two_way_table <- function(data, var1, var2, name1 = "OperatingSystem", name2 = "Gender") {
  table(data[[var1]], data[[var2]])
}


 
 num_sum_by_cat <- function(data, num_vars, cat_vars) {
  data %>% 
    group_by(across(all_of(cat_vars))) %>%
    summarise(across(all_of(num_vars),
                      list(min = min,
                         median = median,
                         max = max,
                         mean = mean,
                         sd = sd)))
 }

bar_plot <- function(data, var, fill_var = var) {
  ggplot(data = data |> drop_na({{ var }}), 
         aes(x = .data[[var]], fill = .data[[fill_var]])) + 
    geom_bar() + 
    ggtitle(paste("Distribution of", var)) + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
    labs(subtitle = paste("Bar Graph of", var), 
         x = var, 
         y = "Count") + 
    theme(plot.subtitle = element_text(hjust = 0.5)) + 
    scale_fill_discrete(name = fill_var)
}




