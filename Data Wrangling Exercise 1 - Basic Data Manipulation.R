library(dplyr)
library(tidyr)
library(data.table)
library(stringdist)

## Loading CSV file
refine <- read.csv("refine_original.csv")

## Standardizing company names using string distance
codes <- c("phillips", "akzo", "van houten", "unilever")
company <- refine$company
strdist <- adist(company, codes)
colnames(strdist) <- codes
rownames(strdist) <- company
a <- amatch(company, codes, maxDist = 4)
refine_df <- data.frame(rawtext = company, company_name = codes[a])
refine_dfd <- distinct(refine_df)

## Add standardized columns to original data
refine1 <- left_join(refine, refine_dfd, by = c("company" = "rawtext"))
refine_tbl <- tbl_df(refine1)

## Split proudct code and product number into two columns
refine_tbl1 <- refine_tbl %>% 
     separate(Product.code...number, c("product code", "product number"))

## Create lookup values for product code
lookup_tbl <- c(p = "Smartphone", v = "TV", x = "Laptop", q = "Tablet")
refine_tbl2 <- refine_tbl1 %>%
     mutate(product_category = lookup_tbl[`product code`])

## Group address into one column: full_address
refine_tbl3 <- unite(refine_tbl2, full_address, address, city, country, sep = ",")

## Create dummy variables for company and product categories
refine_tbl4 <- refine_tbl3 %>% 
     mutate(companies = paste("company", company_name, sep = "_"), 
            product_categories = paste("product", product_category, sep = "_")) %>%
     spread(companies, companies) %>% 
     spread(product_categories, product_categories)

refine_tbl4[,8:15] <- ifelse(!is.na(refine_tbl4[,8:15]),1,0)

refine_final <- refine_tbl4 %>% 
     select(company_name, `product code`, `product number`, full_address, name
            ,product_category, 8:15)

## write to CSV file
write.csv(refine_final, "refine_clean.csv")
