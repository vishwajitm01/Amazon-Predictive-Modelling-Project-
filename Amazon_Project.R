### Read in data and set libraries
amazon_u <- read.csv("~/Amazon/amazon_products_sales_data_uncleaned.csv")
library(tidyverse)
library(caret)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(rpart)
library(rpart.plot)
library(ipred)
library(broom)
another <- read.csv("~/Amazon/amazon_products_sales_data_cleaned.csv")

###1. Data Cleaning

#fix typo in row 10285, column 4 -- replace w/ NA
amazon_u[10285, 4] <- NA

#check that no other cells have this typo
amazon_u <- amazon_u |>
  mutate(bought_in_last_month = ifelse(
    !is.na(bought_in_last_month) & 
      !str_detect(bought_in_last_month, "bought in past month"),
    NA, 
    bought_in_last_month
  ))

#new column product_category
amazon_u$product_category <- another$product_category

#find duplicates
duplicated_rows_logical_1 <- duplicated(amazon_u[, c("title", "rating", "number_of_reviews", "bought_in_last_month", "current.discounted_price", "price_on_variant", "listed_price", "is_best_seller", "is_sponsored", "is_couponed", "buy_box_availability", "delivery_details", "sustainability_badges")])
sum(duplicated_rows_logical_1)

#filter out duplicate rows
clean_amazon <- amazon_u[!duplicated_rows_logical_1, ]

str(clean_amazon)

#empty values
empty_values_matrix <- sapply(clean_amazon, function(x) grepl("^\\s*$", x))
col_empty_counts <- colSums(empty_values_matrix)
print(col_empty_counts)

#change product rating
amazon_c <- separate(
  data = clean_amazon,
  col = rating,
  into = c("rating"),
  sep = " ")
amazon_c$rating <- as.numeric(amazon_c$rating)

#delete columns image and url and others
amazon_c <- amazon_c[, -c(12, 13, 14, 15, 16)]

#change buy_box_availability
amazon_f <- amazon_c %>%
  mutate(buy_box_availability = ifelse(buy_box_availability == "", "Unavailable", buy_box_availability))

#create the new column with 0 and 1 for is_couponed
amazon_f <- amazon_f |>
  mutate(
    coupon = if_else(
      (is_couponed == "No Coupon"), 0, 1)
  )

#create the new column with 0 and 1 for is_sponsored (0=organic, 1=sponsored)
amazon_f <- amazon_f |>
  mutate(
    sponsored = if_else(
      (is_sponsored == "Organic"), 0, 1)
  )

#Two types of best seller labels: Amazon's & Best Seller, make all bestsellers labeled as "Best Seller"
amazon_f <- amazon_f |>
  mutate(is_best_seller = ifelse(str_detect(is_best_seller, "Amazon's"),
                                 "Best Seller", 
                                 is_best_seller),
         
         is_best_seller = ifelse(!(is_best_seller %in% c("Best Seller", "No Badge")), 
                                 "No Badge", 
                                 is_best_seller)
  )

#create the new column for bestseller 
amazon_f <- amazon_f |>
  mutate(
    best_seller = if_else(
      (is_best_seller == "Best Seller"), 1, 0)
  )

#number of reviews as numeric
amazon_f$number_of_reviews<- as.numeric(gsub(",", "", amazon_f$number_of_reviews))

#change bought_in_last_month column to units_sold
amazon_f <- amazon_f |>
  mutate(
    units_sold = str_replace_all(bought_in_last_month, "\\+", ""),
    units_sold = str_replace_all(bought_in_last_month, "[^0-9Kk]", ""),
    units_sold = ifelse(str_detect(bought_in_last_month, "[Kk]"),
                        as.numeric(str_replace(units_sold, "[Kk]", "")) * 1000,
                        as.numeric(units_sold))
  )

options(scipen = 999)

#replace the missing values in units_sold column
#create the flag column for units_sold with 0 i flag that this value was missing and we change it
amazon_f <- amazon_f |>
  mutate(
    flag_units_sold = if_else(
      (is.na(units_sold)), 0, 1)
  )

#replace the missing values in units_sold column with median
amazon_f <- amazon_f |>
  group_by(product_category) |>
  mutate(
    units_sold = ifelse(is.na(units_sold), median(units_sold, na.rm = TRUE), units_sold)
  ) |>
  ungroup()

#replace the missing values
#flag the missing values in column rating (0 - no rating, 1 - rating)
amazon_f <- amazon_f |>
  mutate(
    flag_rating = if_else(
      (is.na(rating)), 0, 1)
  )

#flag the missing values in column reviews (0 - no reviews, 1 - yes)
amazon_f <- amazon_f |>
  mutate(
    flag_n_of_reviews = if_else(
      (is.na(number_of_reviews)), 0, 1)
  )

#change the missing values in columns rating and reviews
amazon_f <- amazon_f |>
  group_by(product_category) |>
  mutate(
    rating = ifelse(is.na(rating), median(rating, na.rm = TRUE), rating),
    number_of_reviews = ifelse(is.na(number_of_reviews), 
                               median(number_of_reviews, na.rm = TRUE), number_of_reviews)
  ) |>
  ungroup()

#new column rating_scale in percentages
amazon_f <- amazon_f |>
  mutate(rating_scale = (rating / 5) * 100)

#create the new columns discounted_price and original_price
amazon_pr <- amazon_f

#change the column price_on_variant
amazon_pr <- separate(
  data = amazon_pr,
  col = price_on_variant,
  c("x","original_price"), sep = "\\$")

#change the column listed_price
amazon_pr <- separate(
  data = amazon_pr,
  col = listed_price,
  c("x","listed_price"), sep = "\\$")

#combine 2 columns in one price
amazon_pr$listed_price <- as.numeric(amazon_pr$listed_price)
amazon_pr$original_price <- as.numeric(amazon_pr$original_price)

amazon_pr <- amazon_pr |>
  mutate(
    original_price = case_when(
      is.na(original_price) ~ listed_price,               # if NA, take listed_price
      original_price < listed_price ~ listed_price,       # if smaller, update to listed_price
      TRUE ~ original_price                               # otherwise keep as is
    )
  )

#replace NA in column price to value from column current.discount.price
amazon_pr$current.discounted_price <- as.numeric(amazon_pr$current.discounted_price)
amazon_pr <- amazon_pr |>
  mutate(
    original_price = case_when(
      is.na(original_price) ~ current.discounted_price,               
      original_price < current.discounted_price ~ current.discounted_price,       
      TRUE ~ original_price                               
    )
  )

#replace NA in column current.discount.price to value from column price
amazon_pr <- amazon_pr |>
  mutate(
    current.discounted_price = case_when(
      is.na(current.discounted_price) ~ original_price,               
      TRUE ~ current.discounted_price                             
    )
  )

#create the new column with 0 and 1 for discount 0 - no discount, 1 - yes  
amazon_pr <- amazon_pr |>
  mutate(
    discount = if_else(
      (current.discounted_price == original_price), 0, 1)
  )

#delete unnecessary columns
amazon_final <- amazon_pr[, -c(4,7,8)]

#filter the rows with NA price
na_counts <- colSums(is.na(amazon_final))
print(na_counts)

amazon_final <- amazon_final |>
  filter(!is.na(original_price))

#create discount percentage column
amazon_final <- amazon_final |>
  mutate(discount_percentage = round(((original_price - current.discounted_price) * 100) / original_price, digits = 1))

duplicated_rows_log <- duplicated(amazon_final)
sum(duplicated_rows_log)

#filter out duplicate rows
amazon_final <- amazon_final[!duplicated_rows_log, ]

#change some columns as factor
amazon_final$coupon <- as.factor(amazon_final$coupon)
amazon_final$sponsored <- as.factor(amazon_final$sponsored)
amazon_final$best_seller <- as.factor(amazon_final$best_seller)
amazon_final$flag_units_sold <- as.factor(amazon_final$flag_units_sold)
amazon_final$flag_rating <- as.factor(amazon_final$flag_rating)
amazon_final$flag_n_of_reviews <- as.factor(amazon_final$flag_n_of_reviews)
amazon_final$discount <- as.factor(amazon_final$discount)

#save cleaned file
write.csv(amazon_final, "amazon_clean.csv", row.names = FALSE)

###2. Data visualization
#Product Rating distribution by Best Seller Status 
ggplot(data = amazon_final, aes(x = rating, fill = best_seller)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#ff9966", "1" = "#009999"),
                    labels = c("0" = "Regular", "1" = "Best Seller")) +
  labs(title = "Product Ratings Distribution by Seller Status",
       subtitle = "Comparison of rating frequencies between best sellers and regular products",
       x = "Rating (Stars)",
       y = "Quantity Sold",
       fill = "Product Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

## above 3.5 Star rating 
ggplot(data = amazon_final |> filter(rating >= 3.5), aes(x = rating, fill = best_seller)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#ff9966", "1" = "#009999"),
                    labels = c("0" = "Regular", "1" = "Best Seller")) +
  labs(title = "Product Ratings Distribution by Seller Status",
       subtitle = "Comparison between best sellers and regular products (3.5+ Stars)",
       x = "Rating (Stars)",
       y = "Quantity Sold",
       fill = "Product Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Create price categories
amazonPR <- amazon_final |> 
  mutate(price_range = cut(original_price, 
                           breaks = c(0, 25, 50, 100, 200, 400, 600, 800,Inf),
                           labels = c("$0-25", "$25-50", "$50-100", 
                                      "$100-200", "$200-400", "$400-600", "$600-800", "$800+"),
                           right = FALSE))

# By best Seller Status
ggplot(data = amazonPR, aes(x = price_range, fill = best_seller)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#FF9966", "1" = "#009999"),
                    labels = c("0" = "Regular", "1" = "Best Seller")) +
  labs(title = "Products by Price Range and Best Seller Status",
       x = "Price Range",
       y = "Number of Products",
       fill = "Product Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Best Seller by product category 
amazonPR |>
  filter(best_seller == 1) |>
  count(product_category) |>
  ggplot(aes(x = n, y = reorder(product_category, n))) +
  geom_col(fill = "#FF9966", width = 0.7) +
  labs(
    title = "Best Sellers by Product Category",
    x = "Number of Best Sellers",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )

# Units Sold 
ggplot(data= amazon_final, aes(x = units_sold)) +
  geom_histogram(fill = "#3498db", color = "white", bins = 30, alpha = 0.8) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Distribution of Units Sold",
       subtitle = "Log scale transformation applied",
       x = "Units Sold",
       y = "Total") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())
colnames(amazon_final)
unique(amazon_final$is_sponsored)

#histogram Original price best seller
ggplot(data = amazon_final, aes(x = original_price, fill = factor(is_best_seller, levels = c("No Badge", "Best Seller")))) + 
  geom_histogram(bins = 25, alpha = 0.8, position = "identity") +
  scale_fill_manual(values = c("No Badge" = "#ff9966", "Best Seller" = "#009999"),
                    labels = c("No Badge" = "Regular", "Best Seller" = "Best Seller")) +
  labs(title = "Original Price Distribution",
       subtitle = "Comparison between best sellers and regular products",
       x = "Original Price ($)",
       y = "Number of Products",
       fill = "Product Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

##Histogram by Units Sold 
ggplot(data = amazon_final, aes(x = units_sold, fill = factor(is_best_seller, levels = c("No Badge", "Best Seller")))) + 
  geom_histogram(bins = 25, alpha = 0.8, position = "identity") +
  scale_fill_manual(values = c("No Badge" = "#ff9966", "Best Seller" = "#009999"),
                    labels = c("No Badge" = "Regular", "Best Seller" = "Best Seller")) +
  labs(title = "Units Sold Distribution",
       subtitle = "Comparison between best sellers and regular products",
       x = "Units Sold",
       y = "Number of Products",
       fill = "Product Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

#Unit Sold Histogram of 75% 
summary(amazon_final$units_sold)

quartile_units <- amazon_final|>
  filter(units_sold<= 1351)

ggplot(data = quartile_units, aes(x = units_sold, fill = factor(is_best_seller, levels = c("No Badge", "Best Seller")))) + 
  geom_histogram(bins = 25, alpha = 0.8, position = "identity") +
  scale_fill_manual(values = c("No Badge" = "#ff9966", "Best Seller" = "#009999"),
                    labels = c("No Badge" = "Regular", "Best Seller" = "Best Seller")) +
  labs(title = "Units Sold Distribution",
       subtitle = "Data contains only 75% (3rdQ)",
       x = "Units Sold",
       y = "Number of Products",
       fill = "Product Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

#Histogram by number of Reviews. ___ full data set 
ggplot(data = amazon_final, aes(x = number_of_reviews, fill = factor(is_best_seller, levels = c("No Badge", "Best Seller")))) + 
  geom_histogram(bins = 25, alpha = 0.8, position = "identity") +
  scale_fill_manual(values = c("No Badge" = "#ff9966", "Best Seller" = "#009999"),
                    labels = c("No Badge" = "Regular", "Best Seller" = "Best Seller")) +
  labs(title = "Reviews per Product",
       subtitle = "Comparison between best sellers and regular products",
       x = "Number of Reviews",
       y = "Number of Products",
       fill = "Product Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

#Histogram by number of Reviews. ___ Up to the 3rd quartile 
summary(amazon_final$number_of_reviews)
quartile_data <- amazon_final|>
  filter(number_of_reviews<= 3800)

ggplot(data = quartile_data, aes(x = number_of_reviews, fill = factor(is_best_seller, levels = c("No Badge", "Best Seller")))) + 
  geom_histogram(bins = 25, alpha = 0.8, position = "identity") +
  scale_fill_manual(values = c("No Badge" = "#ff9966", "Best Seller" = "#009999"),
                    labels = c("No Badge" = "Regular", "Best Seller" = "Best Seller")) +
  labs(title = "Reviews per Product",
       subtitle = "Data contains only 75% (3rdQ)",
       x = "Number of Reviews",
       y = "Number of Products",
       fill = "Product Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

#Histogram Discounted Price 
ggplot(data = amazon_final, aes(x = current.discounted_price, fill = factor(is_best_seller, levels = c("No Badge", "Best Seller")))) + 
  geom_histogram(bins = 25, alpha = 0.8, position = "identity") +
  scale_fill_manual(values = c("No Badge" = "#ff9966", "Best Seller" = "#009999"),
                    labels = c("No Badge" = "Regular", "Best Seller" = "Best Seller")) +
  labs(title = "Discounted Price Distribution",
       subtitle = "Comparison between best sellers and regular products",
       x = "Discounted Price ($)",
       y = "Number of Products",
       fill = "Product Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

#Total Sold by category 

# Calculate total products sold by category
sold_by_category <- amazon_final |>
  group_by(product_category) |>
  summarise(total_sold = sum(units_sold, na.rm = TRUE)) |>
  mutate(percentage = total_sold / sum(total_sold) * 100,
         label = paste0(product_category, "\n", scales::comma(total_sold), "\n(", round(percentage, 1), "%)"))

unique(amazon_final$product_category)

# Create pie chart

colors_16 <- c("#8dd3c7", "#bebada", "#fb8072", "#80b1d3", "#fdb462", 
               "#b3de69", "#fccde5", "#bc80bd", "#ccebc5", "#ffed6f",
               "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6",
               "#e78ac3")

ggplot(sold_by_category, aes(x = "", y = total_sold, fill = product_category)) +
  geom_col(color = "white", linewidth = 0.5) +
  coord_polar(theta = "y") +
  geom_text(aes(label = ""), 
            position = position_stack(vjust = 0.5),
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = colors_16)  +
  labs(title = "Products Sold by Category",
       subtitle = "Total units sold by product category") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11)
  )

# Formula to get the 80% of units sold to make a pie Chart that makes more sense 
sum_sold_cat <- amazon_final |>
  group_by(product_category) |>
  summarise(mean_sold = mean(units_sold, na.rm = TRUE),
            total_sold = sum(units_sold, na.rm = TRUE))|>
  mutate(percentage =  total_sold/sum(total_sold)*100)

sum_sold_cat

## All code to make a top 5 prod by category and the rest become other
# Create sold_by_category with all categories
sold_by_category <- amazon_final |>
  group_by(product_category) |>
  summarise(total_sold = sum(units_sold, na.rm = TRUE))

# Get top 5 and group rest as "Other"
sold_by_category_top5 <- sold_by_category |>
  arrange(desc(total_sold)) |>
  mutate(
    rank = row_number(),
    category_group = ifelse(rank <= 5, product_category, "Other")
  ) |>
  group_by(category_group) |>
  summarise(total_sold = sum(total_sold)) |>
  mutate(
    percentage = total_sold / sum(total_sold) * 100,
    label = paste0(round(percentage, 1), "%")
  )

# Create pie chart
ggplot(sold_by_category_top5, aes(x = "", y = total_sold, fill = category_group)) +
  geom_col(color = "white", linewidth = 0.5) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.6),
            size = 4.5, fontface = "bold") +
  scale_fill_manual(values = colors_16) +
  labs(title = "Products Sold by Category ",
       subtitle = "Top 5 and other ",
       fill = "Product Category") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11)
  )

## -=----------------------------------BOXPLOTS
# Rating by product category
ggplot(amazon_final, aes(x = product_category, y = rating, fill = product_category)) +
  geom_boxplot() +
  labs(
    title = "Rating by product category",
    x= "Product Category",
    y = "Rating",
    fill = "Category"
  )+
  coord_flip() +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12)
  )

# Price by best seller status
ggplot(amazon_final, aes(x = best_seller, y = original_price, fill = best_seller)) +
  geom_boxplot() +
  scale_y_log10() +  # Log scale for better visualization
  scale_x_discrete(labels = c("0" = "Regular", "1" = "Best Seller")) +
  labs(
    title = "Price by Best Seller",
    x= NULL,
    y = "Original Price"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 12, face = "bold"),
    legend.position = "none" 
  )

# Units sold by category
ggplot(amazon_final, aes(x = product_category, y = units_sold, fill = product_category)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "Units Sold by product category",
    x= "Product Category",
    y = "Units Sold",
    fill = "Category"
  )+
  coord_flip() +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12)
  )


# number of reviews by category
ggplot(amazon_final, aes(x = product_category, y = number_of_reviews, fill = product_category)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "Number of Reviews by Product Category",
    x= "Product Category",
    y = "Number or Reviews",
    fill = "Category"
  )+
  coord_flip() +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12)
  )


# Boxplot by Discounted Price 
ggplot(amazon_final, aes(x = product_category, y = current.discounted_price, fill = product_category)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "Discounted Price by Product Category",
    x= "Product Category",
    y = "Discounted Price",
    fill = "Category"
  )+
  coord_flip() +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12)
  )

#####________________________ LOG OF DATA -ALL DATA- ___________-

amazon_final_log <- amazon_final |>
  mutate(
    log_units_sold = log1p(units_sold), 
    log_reviews = log1p(number_of_reviews),
    log_disc.price = log1p(current.discounted_price),
    log_price = log1p(original_price)
  )

##1. HISTOGRAMS
## OR. Price was not transformed but made a graph in log scale
#histogram Original price best seller_LOG Scale Because there was no transformation of orginal price to log
ggplot(data = amazon_final, aes(x = original_price, fill = factor(is_best_seller, levels = c("No Badge", "Best Seller")))) + 
  geom_histogram(bins = 25, alpha = 0.8, position = "identity") +
  scale_x_log10(labels = scales::comma)+
  scale_fill_manual(values = c("No Badge" = "#ff9966", "Best Seller" = "#009999"),
                    labels = c("No Badge" = "Regular", "Best Seller" = "Best Seller")) +
  labs(title = "Original Price Distribution",
       subtitle = "Price in Log Scale",
       x = "Original Price ($)",
       y = "Number of Products",
       fill = "Product Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

##Histogram by number of Reviews. ___ full data set---- in log trasmformed Data

ggplot(data = amazon_final_log, aes(x = log_reviews, fill = factor(is_best_seller, levels = c("No Badge", "Best Seller")))) + 
  geom_histogram(bins = 25, alpha = 0.8, position = "identity") +
  scale_fill_manual(values = c("No Badge" = "#ff9966", "Best Seller" = "#009999"),
                    labels = c("No Badge" = "Regular", "Best Seller" = "Best Seller")) +
  labs(title = "Reviews per Product",
       subtitle = "Transformed data to log",
       x = "Number of Reviews",
       y = "Number of Products",
       fill = "Product Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


##Histogram by Units Sold __ log transformed data
ggplot(data = amazon_final_log, aes(x = log_units_sold, fill = factor(is_best_seller, levels = c("No Badge", "Best Seller")))) + 
  geom_histogram(bins = 25, alpha = 0.8, position = "identity") +
  scale_fill_manual(values = c("No Badge" = "#ff9966", "Best Seller" = "#009999"),
                    labels = c("No Badge" = "Regular", "Best Seller" = "Best Seller")) +
  labs(title = "Units Sold Distribution",
       subtitle = "Log transformed Data",
       x = "Units Sold",
       y = "Number of Products",
       fill = "Product Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

#Histogram Discounted Price _ log transformed data
ggplot(data = amazon_final_log, aes(x = log_disc.price, fill = factor(is_best_seller, levels = c("No Badge", "Best Seller")))) + 
  geom_histogram(bins = 25, alpha = 0.8, position = "identity") +
  scale_fill_manual(values = c("No Badge" = "#ff9966", "Best Seller" = "#009999"),
                    labels = c("No Badge" = "Regular", "Best Seller" = "Best Seller")) +
  labs(title = "Discounted Price Distribution",
       subtitle = "Log transformed data",
       x = "Discounted Price ($)",
       y = "Number of Products",
       fill = "Product Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


## BOXPLOTS
# number of reviews by best Seller__Log Transformed Data
ggplot(amazon_final_log, aes(x = best_seller, y = log_reviews, fill =best_seller)) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "#ff9966", "1" = "#009999"),
                    labels = c("0" = "Regular", "1" = "Best Seller"))+
  labs(
    title = "Number of Reviews by Best Seller",
    x= "Best Seller",
    y = "Number or Reviews",
    fill = "Category"
  )+
  coord_flip() +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12)
  )


# Units sold by category__log data
ggplot(amazon_final_log, aes(x = best_seller, y = log_units_sold, fill = best_seller)) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "#ff9966", "1" = "#009999"),
                    labels = c("0" = "Regular", "1" = "Best Seller"))+
  labs(
    title = "Units Sold by Best Seller",
    x= "Best Seller",
    y = "Units Sold",
    fill = "Category"
  )+
  coord_flip() +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12)
  )

# Boxplot by Discounted Price __ best Seller in Log data
ggplot(amazon_final_log, aes(x = best_seller, y = log_disc.price, fill = best_seller)) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "#ff9966", "1" = "#009999"),
                    labels = c("0" = "Regular", "1" = "Best Seller"))+
  labs(
    title = "Discounted Price by Best Seller",
    x= "Best Seller",
    y = "Discounted Price",
    fill = "Category"
  )+
  coord_flip() +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12)
  )

### Original Price not log Transformed but in log scale if want for comparison 
ggplot(amazon_final, aes(x = best_seller, y = original_price, fill = best_seller)) +
  geom_boxplot() +
  
  scale_fill_manual(values = c("0" = "#ff9966", "1" = "#009999"),
                    labels = c("0" = "Regular", "1" = "Best Seller"))+
  labs(
    title = "Discounted Price by Best Seller",
    x= "Best Seller",
    y = "Discounted Price",
    fill = "Category"
  )+
  coord_flip() +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12)
  )


### Comparison tables of summary stats
# summary sales volume
summary(amazon_final$units_sold)
# summary sales volume by best sellers
amazon_final |>
  group_by(best_seller) |>
  summarize(mean=mean(units_sold),median=median(units_sold),min=min(units_sold),max=max(units_sold))

# Reviews by product category. -- Split Best Seller - Regular ----NEW
ggplot(amazon_final, aes(x = product_category, y = number_of_reviews, fill = is_best_seller)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "Number of Reviews by Product Category",
    x= "Product Category",
    y = "Number of Reviews",
    fill = "Category"
  )+
  coord_flip() +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12)
  )
# NUMBER Rating by product category. -- Split Best Seller - Regular ----NEW
ggplot(amazon_final, aes(x = product_category, y = rating, fill = is_best_seller)) +
  geom_boxplot() +
  labs(
    title = "Rating by product category",
    x= "Product Category",
    y = "Rating",
    fill = "Category"
  )+
  coord_flip() +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12)
  )

# summary discount
mean(amazon_final$discount_percentage)
summary(amazon_final$discount_percentage)
# summary discount by best sellers
amazon_final |>
  group_by(best_seller) |>
  summarize(mean=mean(discount_percentage),median=median(discount_percentage),
            min=min(discount_percentage),max=max(discount_percentage))

# summary rating
summary(amazon_final$rating)
# summary rating by best seller
amazon_final |>
  group_by(best_seller) |>
  summarize(mean=mean(rating),median=median(rating),min=min(rating),max=max(rating))

# summary number of reviews
summary(amazon_final$number_of_reviews)
# summary number of reviews by best seller
amazon_final |>
  group_by(best_seller) |>
  summarize(mean=mean(number_of_reviews),median=median(number_of_reviews),
            min=min(number_of_reviews),max=max(number_of_reviews))

# summary original price
summary(amazon_final$original_price)
# summary original price by best seller
amazon_final |>
  group_by(best_seller) |>
  summarize(mean=mean(original_price),median=median(original_price),
            min=min(original_price),max=max(original_price))

# summary current price
summary(amazon_final$current.discounted_price)
# summary current price by best seller
amazon_final |>
  group_by(best_seller) |>
  summarize(mean=mean(current.discounted_price),median=median(current.discounted_price),
            min=min(current.discounted_price),max=max(current.discounted_price))


###3. Create models for the data
#split the data 
set.seed(123) #for consistency
total_obs <- dim(amazon_final)[1] #count number of data points
train_size <- floor(total_obs*0.7)

amazon_train <- sample_n(amazon_final, train_size)
amazon_test <- anti_join(amazon_final, amazon_train)



#REGRESSIONS
summary(amazon_train)

#some visualizations first
ggplot(data = amazon_train) + 
  geom_bar(aes(x = rating, fill = is_best_seller))

ggplot(data = amazon_train) + 
  geom_histogram(aes(x = original_price, fill = is_best_seller), bins = 25)

ggplot(data = amazon_train) + 
  geom_histogram(aes(x = current.discounted_price, fill = is_best_seller), bins = 25)

ggplot(data = amazon_train) + 
  geom_bar(aes(x = is_sponsored, fill = is_best_seller))

#start with logistic regression
full_model <- glm(best_seller ~ rating + number_of_reviews + current.discounted_price + original_price +
                    is_sponsored + coupon + buy_box_availability + product_category + units_sold + discount_percentage,
                  data = amazon_train,
                  family = "binomial")
summary(full_model)


# we chose the best glm
final_model <- glm(formula = best_seller ~ rating + number_of_reviews + current.discounted_price + 
                     is_sponsored + product_category + units_sold + discount_percentage, family = "binomial", 
                   data = amazon_train)
summary(final_model)


#our histograms are right skewed, let try log 
amazon_train_log <- amazon_train |>
  mutate(
    log_units_sold = log1p(units_sold), 
    log_reviews = log1p(number_of_reviews),
    log_disc.price = log1p(current.discounted_price),
    log_price = log1p(original_price)
  )
final_model_log <- glm(best_seller ~ rating + log_reviews + log_disc.price + is_sponsored + 
                         product_category + log_units_sold + discount_percentage,
                       family = "binomial",
                       data = amazon_train_log
)
summary(final_model_log)

# final log model

f_fullmodel_log <- glm(formula = best_seller ~ rating + log_reviews + is_sponsored + 
                         product_category + log_units_sold + discount_percentage, 
                       family = "binomial", 
                       data = amazon_train_log)
summary(f_fullmodel_log)

amazon_test_log <- amazon_test |>
  mutate(
    log_units_sold = log1p(units_sold), 
    log_reviews = log1p(number_of_reviews),
    log_disc.price = log1p(current.discounted_price),
    log_price = log1p(original_price)
  )

prediction_log <- ifelse(predict(f_fullmodel_log, amazon_test_log, type = "response") > 0.5, 1, 0)
confusionMatrix(factor(prediction_log), factor(amazon_test_log$best_seller), positive = "1")


prediction_log_2 <- ifelse(predict(f_fullmodel_log, amazon_test_log, type = "response") > 0.3, 1, 0)
confusionMatrix(factor(prediction_log_2), factor(amazon_test_log$best_seller), positive = "1")

prediction_log_3 <- ifelse(predict(f_fullmodel_log, amazon_test_log, type = "response") > 0.2, 1, 0)
confusionMatrix(factor(prediction_log_3), factor(amazon_test_log$best_seller), positive = "1")

#classification tree (without bagging)
tree_model <- rpart(best_seller ~ rating + log_reviews + is_sponsored + log_units_sold + discount_percentage, 
                    data = amazon_train_log,
                    method = "class",
                    control = rpart.control(minsplit = 20, cp = 0.008)
)
rpart.plot(tree_model, type=5)
pred_tree1 <- predict(tree_model, newdata=amazon_test_log, type="class")

accuracy_tree1 <- mean(pred_tree1 == amazon_test_log$best_seller)
print(accuracy_tree1)
table_tree <- table(Predicted = pred_tree1, Actual = amazon_test_log$best_seller)
print(table_tree)

#classification tree (with bagging)
bagging_model <- bagging(best_seller ~ rating + log_reviews + is_sponsored + log_units_sold + discount_percentage, 
                         data = amazon_train_log,
                         nbagg = 100)
pred_bagging_log <- predict(bagging_model, newdata=amazon_test_log, type="class")

accuracy_tree_log <- mean(pred_bagging_log == amazon_test_log$best_seller)
print(accuracy_tree_log)

table_tree_bagg_log <- table(Predicted = pred_bagging_log, Actual = amazon_test_log$best_seller)
print(table_tree_bagg_log)

#WHAT IF WE CHANGE THRESHOLD
# Get predicted probabilities
pred_probs <- predict(bagging_model, newdata = amazon_test_log, type="prob")[, "1"]
# Use a lower threshold (e.g., 0.2)
pred_tree_threshold <- ifelse(pred_probs > 0.2, 1, 0)
# Confusion matrix
table(Predicted = pred_tree_threshold, Actual = amazon_test_log$best_seller)
sensitivity <- sum(pred_tree_threshold == 1 & amazon_test_log$best_seller == 1) /
  sum(amazon_test_log$best_seller == 1)
print(sensitivity)
accuracy_bagging <- mean(pred_tree_threshold == amazon_test_log$best_seller)




#Q2. Can we train a model to predict the volume of units sold?
lin_reg_full <- lm(log_units_sold ~ rating + log_reviews + log_disc.price + log_price + discount_percentage +
                     is_sponsored + buy_box_availability + product_category  + best_seller + coupon,
                   data = amazon_train_log)
summary(lin_reg_full)

lin_reg_final <- step(lin_reg_full, direction = "backward")

summary(lin_reg_final)


#residuals analysis
residual <- residuals(lin_reg_final)
amazon_train_log$residuals <- residual

ggplot(data = amazon_train_log) + 
  geom_histogram(aes(x = residual))

ggplot(data = amazon_train_log) +
  geom_point(aes(x = lin_reg_final$fitted.values, y = residual)) + 
  geom_hline(yintercept = 0, linetype = "dashed")

predictions_linlog <- predict(lin_reg_full, newdata = amazon_test_log, type = 'response')
predictions_3 <- expm1(predictions_linlog) 


#colored by product category
ggplot(amazon_train_log, aes(x = lin_reg_final$fitted.values, y = residual, color = product_category)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(x = "Fitted values", y = "Residuals", color = "Category")

#colored by is_sponsored
ggplot(amazon_train_log, aes(x = lin_reg_final$fitted.values, y = residual, color = factor(is_sponsored))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(x = "Fitted values", y = "Residuals", color = "Sponsored?")

#fitted vs residuals by product category 
ggplot(amazon_train_log, aes(x = lin_reg_final$fitted.values, y = residual)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ product_category) +
  theme_minimal()

#Confusion Matrix Visual Representationn

prediction_log_3 <- ifelse(predict(f_fullmodel_log, amazon_test_log, type = "response") > 0.2, 1, 0)
confusionMatrix(factor(prediction_log_3), factor(amazon_test_log$best_seller), positive = "1")


truth <- amazon_test_log$best_seller
predicted <- factor(prediction_log_3)
table(truth,predicted)

df_cm <- data.frame(table(truth,predicted))
df_cm

df_cm$Freq <- factor(df_cm$Freq)
df_cm$Observation <- factor(c("True Negative","False Negative","False Positive","True Positive"))

ggplot(df_cm, aes(x = truth, y = predicted, fill = Observation)) +
  geom_tile(color = "black")+
  geom_text(aes(label = Freq),size = 6)+
  scale_fill_manual(values = c('lightgoldenrod','grey','mintcream','beige')) +
  labs(title = "The Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()+theme_bw()
