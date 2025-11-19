# Amazon-Predictive-Modelling-Project-
Amazon has over 600 million products listed, everyday consumers all across the globe actively use these E-Commerce websites. Have you ever wondered what kind of products sell best? which commodities see highest sales volume? questions like these can be answered using data. The project focuses on building  various models to predict sales volume

About Project

The dataset used in this exploration comes from kaggle.com and includes over 40,000 data points for electronics products. The dataset includes information about the products such as product name, ratings, reviews, pricing, discounts, best seller badge information, sponsor information, and more. An early observation about this data is the class imbalance of the dataset concerning the Best Seller information, with 5.9% of the data points being a Best Seller. Noting this is a difficulty, the present exploration aims to answer the following questions:

1. Can we create a model to predict which products will be labeled as a Best Seller with high sensitivity?

2. Can we create a model that predicts the volume of a product that has been purchased?

The first question, if answered, can help Amazon choose which products to push and apply certain deals to in order to increase sales and traffic. The second question can help Amazon and sellers on Amazon increase shop efficiency by informing sellers on which products to keep stocked, quantity of supplier reorders and more.

An important note for this investigation is that the accuracy rates of the models were not considered due to the disproportionately high number of non-Best Sellers. Since accuracy could be misleading, sensitivity was used as the metric for success, with the goal of maximizing True Positives.
