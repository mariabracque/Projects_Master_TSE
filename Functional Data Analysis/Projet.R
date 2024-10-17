################################################################################
#                                                                              #
#                         Functional Data Analysis                             #
#                                                                              #
#               Retail Sales Data: a Functional Perspective                    #
#                                                                              #
#                 Theo Condette & Maria Bracque Vendrell                       #
#                                                                              #
################################################################################

#### Libraries ####
library(tidyverse)
library(fda)
library(ggplot2)
library(data.table)
library(dplyr)
library(plotly)
library(patchwork)
library(glmnet)
library(fdapace)

# load datasets
sales <- read.csv('sales.csv')
product_hierarchy <- read.csv('product_hierarchy.csv')

# I - Exploring Data ----

# a) Economic context ----
  
    # let's have a look at the variables' type
  sapply(sales, class)
  sapply(product_hierarchy, class)
  
    # let's convert the columns to the right data type
  sales$date <- as.Date(sales$date)
  sales$sales <- as.numeric(sales$sales)
  
    # We have data from 2017-01-02 to 2019-12-29
  min(sales$date)
  max(sales$date)
  
    # let's see the number of stores and products
  store_ids <- unique(sales$store_id)
  length(store_ids) #there are 144 stores
  product_ids <- unique(sales$product_id)
  length(product_ids) #there are 20 products
  
    # Number of observation by store :
  sort(table(sales$store_id)) # We have new and old store.
  cbind(aggregate(date ~store_id,data=sales,min),aggregate(date ~store_id,data=sales,max)) # Same lifetime! 
  
    # Number of observation by product :
  sort(table(sales$product_id)) # We have different number of observation by product
  
    # Let's keep the products than have been sold during the entire 2017-2019 period without interruptions
  sales_summary <- sales %>%
    group_by(product_id) %>%
    summarize(total_dates = n_distinct(date))
  
  products_on_sale_whole_period <- sales_summary %>%
    filter(total_dates == length(seq(as.Date("2017-01-02"), as.Date("2019-12-29"), by = "day")))%>%
    select(product_id)
  
  old_sales <- sales #to save the old dataset in case we need it
  sales <- sales %>%
    filter(product_id %in% products_on_sale_whole_period$product_id)
  
  product_ids <- unique(sales$product_id)
  length(product_ids) #there are 17 products now (20 before)
  
    # Lifetime :
  cbind(aggregate(date ~product_id,data=sales,min),aggregate(date ~product_id,data=sales,max)) # Same lifetime! 
    
    # Might be due because some store sell more some products than others:
  tables<-tapply(sales$store_id, sales$product_id, table) 
  sorted_tables <- lapply(tables, function(x) sort(x, decreasing = TRUE))
  print(sorted_tables)

    # Different promotions :
  table(sales$promo_bin_1)
  
  
    
# b) Missing values ----
  
    # let's have a look at the missing values
  colSums(is.na(sales)) # sales: 41431 missing values
                        # price: 29292 missing values
  
    # let's explore the missing values for sales
  missing_sales <- sales %>%
    filter(is.na(sales))  
  
  length(unique(missing_sales$product_id)) # Missing values occurs for all products
  table(missing_sales$store_id) # Missing values occurs more for some stores.
  sales$store_id[which(!unique(sales$store_id)%in%unique(missing_sales$store_id))] #The 9 stores with no missing values.
  
  table(missing_sales$date)
  
  sales%>%
    filter(date>"2019-11-01")%>%
    summary() #we notice that all the missing values are between 2019-11-01 and 2019-12-29 and for all products.
  
  sales <- sales[!is.na(sales$sales),]

    # let's explore the missing values for price
  missing_sales <- sales %>%
    filter(is.na(price)) 
  sort(table(missing_sales$product_id)) # Some products are more missing than others
  sort(table(missing_sales$store_id)) # Some store have more missing price than others

    # Since the same product is sold in different stores, we might find a way to replace missing values 
    
    # Do we have the same applied price in all store ?
  df_summary_by_date<-sales%>%# without promo
    group_by(product_id,date)%>%
    summarize(min=min(price),max=max(price),std=sd(price))
  
  df_summary_by_date%>%
    filter(std>0)%>%
    select(product_id)%>%
    table()
  
    # We have price variability among the different store for few products :
  df_summary_by_date%>%
    filter(std>0)%>%
    select(date)%>%
    summarise(min=min(date),max=max(date))

    # This variability occurs only for few days for some products, however it occurs
  # for the whole lifetime for others : P0035, P0055
  
    # let's replace these missing values by median !
  med_prices <- sales %>%
    filter(!is.na(price)) %>%
    group_by(product_id, date) %>%
    summarise(med_price = median(price))
  
  sales <- sales %>%
    left_join(med_prices, by = c("product_id", "date"))
  
  sales$price[is.na(sales$price)] <- sales$med_price[is.na(sales$price)]
  sales$med_price <- NULL
  
    # let's look at the missing values now
  colSums(is.na(sales))
 
    # let's remove from the environment everything we don't need anymore
  rm(tables, med_prices, missing_sales, sorted_tables, df_summary_by_date, products_on_sale_whole_period, sales_summary)
  

  
# c) Outliers ----
  
  summary(sales[,c("sales","price")])
  
  sales%>%
    filter(sales>150)
  # Actually, we do not consider high amount of sales as outlier for what is around Christmas and around some event in Turkey : Canakkale Victory for example where people consume. 
  # Also it appears ok since it occurs only on few store and always the same.
  
  sales%>%
    filter(price>90)%>%
    select(product_id)%>%
    table()
  
  # The high price correspond to one product, it looks fine.
  

# d) New features ----
  
  # Creation of gain:
  sales$gain <- sales$sales*sales$price
  # Get numeric level of promotion : 
  sales$promo_numeric <- as.numeric(as.character(factor(sales$promo_bin_1,
                                                        levels = c("", "verylow", "low", "moderate", "high", "veryhigh"),
                                                        labels = c(NA, 1, 2, 3, 4, 5))))
  # Creation of week :
  sales$week <- format(sales$date, "%Y-%V")
  
  
  # Let's modify the dataset so that all the product history of all stores are of the same size.
  
    # We suppose that not having the product in the store is equivalent to not selling it. Thus, we create artificial rows with
    # with sales equal 0 and a price which correspond to the median at the corresponding date of all the stores
  
    # Let's create an identifier if needed later :
  save_sales<-sales
  sales <- save_sales
  sales$in_shop <- 1
  sales$sales_pred <- NA
  full_dates <- sort(unique(sales$date))
  
  list_prod <- unique(sales$product_id)
  list_store<- unique(sales$store_id)
  df_missing <- data.frame()
  
  
  # How many rows do we miss :
  for(prod in list_prod){
    for(store in list_store){
      print(prod)
      print(store)
      
      current_df <- sales%>%
        filter(product_id==prod,store_id==store)  
      n_row_to_add <- sum(!full_dates%in%current_df$date)
      missing_date <- full_dates[which(!full_dates%in%current_df$date)]
      
      if(length(missing_date)>1){
        
        df_to_rbind <- data.frame(product_id=rep(prod,n_row_to_add),
                                  store_id=rep(store,n_row_to_add),
                                  date=missing_date,
                                  sales=rep(0,n_row_to_add),
                                  price=rep(NA,n_row_to_add),
                                  promo_bin_1=rep("",n_row_to_add),
                                  gain=rep(0,n_row_to_add),
                                  promo_numeric=rep(NA,n_row_to_add),
                                  week=format(missing_date, "%Y-%V"),
                                  sales_pred=rep(NA,n_row_to_add),
                                  in_shop=0
        )
        df_missing <- rbind(df_missing,df_to_rbind)
        
      }
      
      
    }
  }
  
  
  sales <- rbind(sales,df_missing)
  
  # Replace NA's in price:
  
  med_prices <- sales %>%
    filter(!is.na(price)) %>%
    group_by(product_id, date) %>%
    summarise(med_price = median(price))
  
  sales <- sales %>%
    left_join(med_prices, by = c("product_id", "date"))
  
  sales$price[is.na(sales$price)] <- sales$med_price[is.na(sales$price)]
  sales$med_price <- NULL
  
  
  sales<-arrange(sales,date,product_id,store_id)
  
  
  
  
  
# e) Descriptive analysis ----
  # Univariate analysis ----
  
    # Number of day spend in all the stores by product :
  df_count <- as.data.frame(table(sales$product_id))
    
  ggplot(df_count)+
    geom_col(aes(x=reorder(Var1,desc(Freq)),y=Freq))+
    theme_minimal() +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(title = "Number of day spent in all the stores by product", x = "Product", y = "Number of day a product has spend in the stores")

  
    # Product that are the most sold :
  sales%>%filter(in_shop==1)%>%
    group_by(product_id)%>%
    summarize(Total_sales = sum(sales))%>%
    ggplot()+
    geom_col(aes(x=reorder(product_id,desc(Total_sales)),y=Total_sales))+    
    theme_minimal() +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(title = "Product that are the most sold", x = "Product", y = "Sales quantity")
  
  
    # Store that sales the most :
  
  sales%>%filter(in_shop==1)%>%
    group_by(store_id)%>%
    summarize(Total_sales = sum(sales))%>%
    top_n(20)%>%
    ggplot()+
    geom_col(aes(x=reorder(store_id,desc(Total_sales)),y=Total_sales))+    
    theme_minimal() +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(title = "Store that sales the most", x = "Store", y = "Sales quantity")
  
    # Store that gain the most :
  sales%>%filter(in_shop==1)%>%
    group_by(store_id)%>%
    summarize(Total_gain = sum(gain))%>%
    top_n(20)%>%
    ggplot()+
    geom_col(aes(x=reorder(store_id,desc(Total_gain)),y=Total_gain))+    
    theme_minimal() +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(title = "Store that gain the most", x = "Store", y = "Sales gain") 
  
    # Price distribution :
  ggplot(sales)+
    geom_boxplot(aes(y = price))+
    theme_minimal()+
    ggtitle("Overall price distribution")

    # gain over time :
  df_gain <- sales%>%filter(in_shop==1)%>%
    group_by(week) %>%
    summarise(
      weekly_gain = sum(gain),
      date =max(date)
    )
  
  ggplot(df_gain)+
    geom_line(aes(x=date,y=weekly_gain))+
    xlab("Date")+
    ggtitle("gain evolution")
  
  
    # Why registration might not be relevant :
    list_store<-unique(sales$store_id)
    sales %>%
      filter(in_shop == 1, product_id == "P0001") %>%
      group_by(store_id) %>%
      filter(!promo_bin_1 == "",store_id%in%list_store[sample(1:144,30)]) %>%
      ggplot(aes(x = store_id, fill = promo_bin_1)) +
      geom_bar(position = "dodge", width = 0.7, color = "black") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3, color = "black") +
      theme_minimal() +  
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Number of promotions on period 2017-2019", x = "Store", y = "Number of promotion days", fill = "Promotion Type")
  
    sales %>%
      filter(in_shop == 1, product_id == "P0035") %>%
      group_by(store_id) %>%
      filter(!promo_bin_1 == "",store_id%in%list_store[sample(1:144,30)]) %>%
      ggplot(aes(x = store_id, fill = promo_bin_1)) +
      geom_bar(position = "dodge", width = 0.7, color = "black") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3, color = "black") +
      theme_minimal() +  
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Number of promotions on period 2017-2019", x = "Store", y = "Number of promotion days", fill = "Promotion Type")
    
    sales %>%
      filter(in_shop == 1, product_id == "P0055") %>%
      group_by(store_id) %>%
      filter(!promo_bin_1 == "",store_id%in%list_store[sample(1:144,30)]) %>%
      ggplot(aes(x = store_id, fill = promo_bin_1)) +
      geom_bar(position = "dodge", width = 0.7, color = "black") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3, color = "black") +
      theme_minimal() +  
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Number of promotions on period 2017-2019", x = "Store", y = "Number of promotion days", fill = "Promotion Type")
    
    
    sales %>%
      filter(in_shop == 1, product_id == "P0066") %>%
      group_by(store_id) %>%
      filter(!promo_bin_1 == "",store_id%in%list_store[sample(1:144,30)]) %>%
      ggplot(aes(x = store_id, fill = promo_bin_1)) +
      geom_bar(position = "dodge", width = 0.7, color = "black") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3, color = "black") +
      theme_minimal() +  
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Number of promotions on period 2017-2019", x = "Store", y = "Number of promotion days", fill = "Promotion Type")
    
  # Multivariate analysis ----
  ## Analysis by product ##
  
    ### Quantity analysis :
  
    #	Sales quantity in function of date (daily) by product:
  df_sales_product <- sales%>%filter(in_shop==1)%>%
    group_by(product_id,date)%>%
    summarize(total_sales=sum(sales))
  
  ggplot(df_sales_product)+
    geom_line(aes(x=date, y = total_sales, color = product_id))+
    ggtitle("	Sales quantity in function of date (daily) by product")
  
  
    # Sales quantity in function of date (weekly) by product :   
  summary_df <- sales%>%filter(in_shop==1)%>%
    group_by(week,product_id) %>%
    summarise(
      total_sales = sum(sales),
      date=max(date)
    )
  
  ggplot(summary_df)+
    geom_line(aes(x=date, y = total_sales, color = product_id))+
    ggtitle("	Sales quantity in function of date (weekly) by product")
              # Smoothing would be better maybe !
  
    # Sales quantity in function of date (daily) by product with promotion :  
  sales_weekly <- sales%>%filter(in_shop==1)%>%
    group_by(product_id,date) %>%
    summarise(
      sales_quantity = sum(sales),
      promo = mean(promo_numeric,na.rm = T)
      )
  
  gg<-ggplot(sales_weekly)+
    geom_line(aes(x=date, y =sales_quantity, color = product_id))+
    geom_point(data=subset(sales_weekly, !is.na(promo)),aes(x=date, y =sales_quantity,color=product_id,size=promo))
  
  ggplotly(gg) # Interesting
  
    # Sales quantity in function of date (weekly) by product with promotion :  
  sales_weekly <- sales%>%filter(in_shop==1)%>%
    group_by(product_id,week) %>%
    summarise(
      sales_quantity = sum(sales),
      promo = mean(promo_numeric,na.rm = T),
      date = max(date))
  
  gg<-ggplot(sales_weekly)+
    geom_line(aes(x=date, y =sales_quantity, color = product_id))+
    geom_point(data=subset(sales_weekly, !is.na(promo)),aes(x=date, y =sales_quantity,color=product_id,size=promo))
  
  ggplotly(gg) # Interesting
  
  ## Analysis by store ##  
  
    # Sales quantity in function of date (weekly) by store with promotion :  
  sales_weekly <- sales%>%filter(in_shop==1)%>%
    filter(product_id =="P0055")%>%
    group_by(store_id,week) %>%
    summarise(
      sales_quantity = sum(sales),
      promo = mean(promo_numeric,na.rm = T),
      date = max(date))
  
  gg<-ggplot(sales_weekly)+
    geom_line(aes(x=date, y =sales_quantity, color = store_id))+
    geom_point(data=subset(sales_weekly, !is.na(promo)),aes(x=date, y =sales_quantity,color=store_id,size=promo))
  
  ggplotly(gg) # Interesting
  

    # There is a need of smoothing! that's why it is clearer when we look at weekly data.
  
  
    ### Price analysis :

    #	Sales price in function of date (daily) by product:
  df_sales_product <- sales%>%filter(in_shop==1)%>%
    group_by(product_id,date)%>%
    summarize(sales_price=mean(price))
  
  gg<-ggplot(df_sales_product)+
    geom_line(aes(x=date, y = sales_price, color = product_id))+
    ggtitle("	Sales price in function of date (daily) by product")

  ggplotly(gg)# not very interesting
  
  
    # Sales price in function of date (weekly) by product :   
  summary_df <- sales%>%filter(in_shop==1)%>%
    group_by(week,product_id) %>%
    summarise(
      sales_price = mean(price),
      date=max(date)
    )
  
  gg<-ggplot(summary_df)+
    geom_line(aes(x=date, y = sales_price, color = product_id))+
    ggtitle("	Sales price in function of date (weekly) by product")

  ggplotly(gg) # not very interesting
  
    # Sales price in function of date (daily) by product with promotion :  
  sales_weekly <- sales%>%filter(in_shop==1)%>%
    group_by(product_id,date) %>%
    summarise(
      sales_price = mean(price),
      promo = mean(promo_numeric,na.rm = T)
    )
  
  gg<-ggplot(sales_weekly)+
    geom_line(aes(x=date, y =sales_price, color = product_id))+
    geom_point(data=subset(sales_weekly, !is.na(promo)),aes(x=date, y =sales_price,color=product_id,size=promo))
  
  ggplotly(gg) # Interesting no price change for promotion
  
    # Sales price in function of date (weekly) by product with promotion :  
  sales_weekly <- sales%>%filter(in_shop==1)%>%
    group_by(product_id,week) %>%
    summarise(
      sales_price = mean(price),
      promo = mean(promo_numeric,na.rm = T), # Average promo for those who propose a promo. If we remove na.rm we have the promo that are made by all the store at the same time
      date = max(date))
  
  gg<-ggplot(sales_weekly)+
    geom_line(aes(x=date, y =sales_price, color = product_id))+
    geom_point(data=subset(sales_weekly, !is.na(promo)),aes(x=date, y =sales_price,color=product_id,size=promo))
  
  ggplotly(gg) # For most of the cases, there is no real promotion. In general, the price is the same before and after "promotion".
  
  
  ## Analysis by store ##   

  
  #	Sales price in function of date (weekly) by store : Product P0055
  
  sales_weekly <- sales%>%filter(in_shop==1)%>%
    filter(product_id =="P0055")
  
  ggplot(sales_weekly)+
    geom_line(aes(x=date, y =price, color = store_id))+
    geom_point(data=subset(sales_weekly, !is.na(promo_bin_1)),aes(x=date, y =price,color=store_id,size=promo_bin_1))
  
  ggplotly(gg) # not interesting
  

    #	Sales price in function of date (weekly) by store : Product P0055
  sales_weekly <- sales%>%filter(in_shop==1)%>%
    filter(product_id =="P0055")%>%
    group_by(store_id,week) %>%
    summarise(
      sales_price = mean(price),
      promo = mean(promo_numeric,na.rm = T),
      date = max(date))
  
  gg<-ggplot(sales_weekly)+
    geom_line(aes(x=date, y =sales_price, color = store_id))+
    geom_point(data=subset(sales_weekly, !is.na(promo)),aes(x=date, y =sales_price,color=store_id,alpha=promo))
  
  ggplotly(gg) # not interesting
  
  ## Analysis of the promotion ##
  ggplotly(sales%>%filter(in_shop==1)%>%
             filter(product_id=="P0055")%>%
             group_by(promo_bin_1,date)%>%
             summarise(n_store_promo=n())%>%
             ggplot()+
             geom_bar(aes(x=date,y=n_store_promo, fill=promo_bin_1),stat = "identity")+
             theme_minimal() +  
             theme(axis.text.x = element_text(angle = 45, hjust = 1))+
             labs(title = "Stores promotion for product P0055 for 2017-2019", x = "Date", y = "Number of store" ) )


# II - Data smoothing ----

  # FDA techniques are helpful. Smoothing using splines regression analysis can help identify underlying trends and seasonal patterns
  
  
  # a) Smoothing by regression analysis for sales for Product P0051 ----
  
  
  # let's first focus on only one product and one store only : 
  
  df_p51_s2<- sales %>% 
    filter(store_id == "S0002", product_id=="P0051")
  
  ggplot(df_p51_s2)+
    geom_point(aes(x=date, y = sales))+
    ggtitle("Sales quantity in function of date (daily) for Product P0051 and Store S0002")
  
  # We wish to go from this discrete version of the data to a functional object. 
  # To do so, we perform some regression splines.
  
  time <- as.numeric(df_p51_s2$date)
  Spline_b <- create.bspline.basis(range(time), nbasis = 20, norder = 4)
  plot(Spline_b)
  basismat <- eval.basis(time, Spline_b)
  result_coef = lsfit(basismat, df_p51_s2$sales, intercept=FALSE)$coef 
  my_smooth_func = fd(result_coef, Spline_b)
  plot(my_smooth_func)
  df_p51_s2$sales_pred <- eval.fd(time, my_smooth_func)
  
  ggplot(df_p51_s2)+
    geom_point(aes(x=date,y=sales))+
    geom_line(aes(x=date,y=sales_pred),color="red",linewidth=1) # We have negative values which is not desirable, we replace them:
  
  
  df_p51_s2$sales_pred[df_p51_s2$sales_pred<0]<-0
  
  ggplot(df_p51_s2)+
    geom_point(aes(x=date,y=sales))+
    geom_line(aes(x=date,y=sales_pred),color="red",linewidth=1) #better.
  
  
  # We did it with nbasis = 20, but which would be the most optimal number of B-Spline Control Points?
  
  # According to Harmening and Neuner (2016), the BIC criteria is a good method to use
  # the BIC criteria.
  
  # 1. Define a range of possible numbers of basis functions
  nbasis_range <- seq(from = 5, to = 30, by = 1)
  
  # 2. Calculate BIC for each number of basis functions
  bic_values <- numeric(length(nbasis_range))
  
  for (i in seq_along(nbasis_range)) {
    nbasis <- nbasis_range[i]
    Spline_b <- create.bspline.basis(range(time), nbasis = nbasis, norder = 4)
    basismat <- eval.basis(time, Spline_b)
    fit <- lm(df_p51_s2$sales ~ basismat-1)
    bic_values[i] <- BIC(fit)
  }
  
  # 3. Find the optimal number of basis functions
  optimal_nbasis <- nbasis_range[which.min(bic_values)]
  print(optimal_nbasis) #we find that 10 is the optimal number of splines
  
  # let's adapt our previous B-Spline curve
  Spline_b <- create.bspline.basis(range(time), nbasis = optimal_nbasis , norder = 4)
  basismat <- eval.basis(time, Spline_b)
  result_coef = lsfit(basismat, df_p51_s2$sales, intercept=FALSE)$coef 
  my_smooth_func = fd(result_coef, Spline_b)
  plot(my_smooth_func)
  df_p51_s2$sales_pred <- eval.fd(time, my_smooth_func)
  
  ggplot(df_p51_s2)+
    geom_point(aes(x=date,y=sales))+
    geom_line(aes(x=date,y=sales_pred),color="red",linewidth=1) # We have negative values which is not desirable, we replace them:
  
  df_p51_s2$sales_pred[df_p51_s2$sales_pred<0]<-0 # replace negative values
  
  ggplot(df_p51_s2)+
    geom_point(aes(x=date,y=sales))+
    geom_line(aes(x=date,y=sales_pred),color="red",linewidth=1) #better.
  
  
  
  # Now let's do it for all the stores for this product :
  product_data_51 <- sales %>% 
    filter(product_id == "P0051")
  nbasis_range <- seq(from = 5, to = 30, by = 1)
  
  product_data_51$sales_pred <- NA
  
  full_dates <-sort(unique(sales$date))
  
  for (i in unique(product_data_51$store_id)) {
    print(i)
    df_sales_product51_storex <- product_data_51 %>% 
      filter(store_id == i)
    
    bic_values <- numeric(length(nbasis_range))
    time <- as.numeric(df_sales_product51_storex$date)
    
    
    for (j in seq_along(nbasis_range)) {
      
      nbasis <- nbasis_range[j]
      Spline_b <- create.bspline.basis(range(time), nbasis = nbasis, norder = 4)
      basismat <- eval.basis(time, Spline_b)
      fit <- lm(df_sales_product51_storex$sales ~ basismat)
      bic_values[j] <- BIC(fit)
      
    }
    
    optimal_nbasis <- nbasis_range[which.min(bic_values)]
    
    Spline_b <- create.bspline.basis(range(time), nbasis = optimal_nbasis, norder = 4)
    basismat <- eval.basis(time, Spline_b)
    result_coef = lsfit(basismat, df_sales_product51_storex$sales, intercept=FALSE)$coef
    my_smooth_func = fd(result_coef, Spline_b)
    product_data_51$sales_pred[which(product_data_51$store_id==i)] <- eval.fd(time, my_smooth_func)
    product_data_51$sales_pred[which(product_data_51$store_id==i)][product_data_51$sales_pred[which(product_data_51$store_id==i)]<0] <- 0
    
  }
  
  # Plot :
  ggplotly(ggplot(product_data_51)+
             geom_line(aes(x=date,y=sales_pred,color=store_id)))
  
  Plot_list<-list()
  
  for(i in unique(product_data_51$store_id)){
    
    Plot_list[[i]]<-product_data_51%>%
      filter(store_id==i)%>%
      ggplot()+
      geom_point(aes(x=date,y=sales))+
      geom_line(aes(x=date,y=sales_pred),color="red",linewidth=1) #better.
    
    
  }
  
  #Let's look at the 12 first plot :
  grid.arrange(grobs=Plot_list[1:12])
  
  
  rm(basismat, product_data_51, df_p51_s2, product51_data, df_sales_product51, df_sales_product51_storex, fit, funList,smooth_funcs_51, Spline_b, bic_values, i, j, nbasis, nbasis_range, time )
  
  # b) Smoothing by regression analysis for total sales for Product P0051 ----
  
  # let's first focus on only one product: P0051
  product51_data <- sales %>% 
    filter(product_id == "P0051")
  
  df_sales_product51 <- product51_data%>%
    group_by(product_id,date)%>%
    summarize(total_sales=sum(sales))
  
  ggplot(df_sales_product51)+
    geom_point(aes(x=date, y = total_sales))+
    ggtitle("	Sales quantity in function of date (daily) for Product P0051")
  
  # We wish to go from this discrete version of the data to a functional object. 
  # To do so, we perform some regression splines.
  
  time <- as.numeric(df_sales_product51$date)
  
  
  Spline_b <- create.bspline.basis(range(time), nbasis = 20, norder = 4)
  plot(Spline_b)
  basismat <- eval.basis(time, Spline_b)
  
  result_coef = lsfit(basismat, df_sales_product51$total_sales, intercept=FALSE)$coef
  my_smooth_func_51 = fd(result_coef, Spline_b)
  plot(my_smooth_func_51)
  
  df_sales_product51$sales_pred <- as.numeric(eval.fd(time, my_smooth_func_51))
  
  ggplot(df_sales_product51)+
    geom_point(aes(x=date,y=total_sales))+
    geom_line(aes(x=date,y=sales_pred),color="red",linewidth=1) # We have negative values which is not desirable, we replace them:
  
  
  df_sales_product51$sales_pred[df_sales_product51$sales_pred<0]<-0
  
  ggplot(df_sales_product51)+
    geom_point(aes(x=date,y=total_sales))+
    geom_line(aes(x=date,y=sales_pred),color="red",linewidth=1) #better.
  
  
  # We did it with nbasis = 20, but which would be the most optimal number of B-Spline Control Points?
  # According to Harmening and Neuner (2016), the BIC criteria is a good method to use
  # the BIC criteria 
  
  # 1. Define a range of possible numbers of basis functions
  nbasis_range <- seq(from = 5, to = 50, by = 1)
  
  # 2. Calculate BIC for each number of basis functions
  bic_values <- numeric(length(nbasis_range))
  
  for (i in seq_along(nbasis_range)) {
    nbasis <- nbasis_range[i]
    Spline_b <- create.bspline.basis(range(time), nbasis = nbasis, norder = 4)
    basismat <- eval.basis(time, Spline_b)
    fit <- lm(df_sales_product51$total_sales ~ basismat)
    bic_values[i] <- BIC(fit)
  }
  
  # 3. Find the optimal number of basis functions
  optimal_nbasis <- nbasis_range[which.min(bic_values)]
  print(optimal_nbasis) #we find that 19 is the optimal number of splines
  
  # let's adapt our previous B-Spline curve
  Spline_b <- create.bspline.basis(range(time), nbasis = optimal_nbasis, norder = 4)
  basismat <- eval.basis(time, Spline_b)
  
  result_coef = lsfit(basismat, df_sales_product51$total_sales, intercept=FALSE)$coef
  my_smooth_func_51 = fd(result_coef, Spline_b)
  plot(my_smooth_func_51)
  
  rm(basismat, fit, product51_data, Spline_b, bic_values, i, nbasis, nbasis_range, optimal_nbasis, time )
  
  # c) Smoothing by regression analysis for all products ----
  
  
  nbasis_range <- seq(from = 5, to = 30, by = 1)
  full_dates <-sort(unique(sales$date))
  list_of_prod <- unique(sales$product_id)
  
  for(prod in list_of_prod){
    
    print(prod)
    sales_sub <- sales %>% 
      filter(product_id == prod)
    
    
    for (i in unique(sales_sub$store_id)) {
      
      sales_sub_sub <- sales_sub %>% 
        filter(store_id == i)
      
      bic_values <- numeric(length(nbasis_range))
      time <- as.numeric(sales_sub_sub$date)
      
      
      for (j in seq_along(nbasis_range)) {
        
        nbasis <- nbasis_range[j]
        Spline_b <- create.bspline.basis(range(time), nbasis = nbasis, norder = 4)
        basismat <- eval.basis(time, Spline_b)
        fit <- lm(sales_sub_sub$sales ~ basismat)
        bic_values[j] <- BIC(fit)
        
      }
      
      optimal_nbasis <- nbasis_range[which.min(bic_values)]
      
      Spline_b <- create.bspline.basis(range(time), nbasis = optimal_nbasis, norder = 4)
      basismat <- eval.basis(time, Spline_b)
      result_coef = lsfit(basismat, sales_sub_sub$sales, intercept=FALSE)$coef
      my_smooth_func = fd(result_coef, Spline_b)
      sales$sales_pred[which(sales$store_id==i & sales$product_id==prod)] <- eval.fd(time, my_smooth_func)
      sales$sales_pred[which(sales$store_id==i & sales$product_id==prod)][sales$sales_pred[which(sales$store_id==i & sales$product_id==prod)] < 0] <- 0
      
    }
  }
  
  
  ggplotly(sales%>%
             filter(product_id=="P0055")%>%
             ggplot()+
             geom_line(aes(x=date,y=sales_pred,color=store_id))+
             geom_point(aes(x=date,y=sales,color=store_id)))
  
  rm(i, j, bic_values, time, nbasis, Spline_b, basismat, fit, optimal_nbasis)
  
  # d) Smoothing by regression analysis for total sales all products ----
  
  sales_wt_store<-sales%>%
    group_by(product_id,date)%>%
    summarise(total_sales=sum(sales),
              price=median(price,na.rm=T),
              gain=total_sales*price,
              promo_numeric=mean(promo_numeric,na.rm=T)
    )
  sales_wt_store$week =format(sales_wt_store$date, "%Y-%V")
  sales_wt_store$sales_pred <-NA
  
  for (i in list_of_prod) {
    
    sales_wt_store_sub <- sales_wt_store%>%
      filter(product_id==i)
    
    print(i)
    bic_values <- numeric(length(nbasis_range))
    time <- as.numeric(sales_wt_store_sub$date)
    
    for (j in seq_along(nbasis_range)) {
      nbasis <- nbasis_range[j]
      Spline_b <- create.bspline.basis(range(time), nbasis = nbasis, norder = 4)
      basismat <- eval.basis(time, Spline_b)
      fit <- lm(sales_wt_store_sub$total_sales ~ basismat)
      bic_values[j] <- BIC(fit)
    }
    
    optimal_nbasis <- nbasis_range[which.min(bic_values)]
    
    Spline_b <- create.bspline.basis(range(time), nbasis = optimal_nbasis, norder = 4)
    basismat <- eval.basis(time, Spline_b)
    
    result_coef = lsfit(basismat, sales_wt_store_sub$total_sales, intercept=FALSE)$coef
    
    my_smooth_func = fd(result_coef, Spline_b)
    sales_wt_store$sales_pred[which(sales_wt_store$product_id==i)] <- eval.fd(time, my_smooth_func)
    sales_wt_store$sales_pred[which(sales_wt_store$product_id==i)][sales_wt_store$sales_pred[which(sales_wt_store$product_id==i)] < 0] <- 0
    
  }
  
  
  ggplotly(ggplot(sales_wt_store) +
             geom_point(aes(x = date, y =total_sales, color = product_id))+
             geom_line(aes(x = date, y = sales_pred, color = product_id)) +
             ggtitle("Sales quantity in function of date (daily) by product"))
  
  ggplotly(ggplot(sales_wt_store)+
             geom_line(aes(x = date, y = sales_pred, color = product_id)) +
             ggtitle("Sales quantity in function of date (daily) by product"))
  
  # let's delete all the variables we don't need before moving to the next part
  rm(basismat, df_sales_product51, fit, gg, my_smooth_func_51, Spline_b, bic_values,i, j, nbasis, nbasis_range, result_coef, optimal_nbasis)
  
  
  # e) Smoothing by regression analysis for the price for Product P0011 ----
  
  # let's first focus on only one product: P0011
  df_sales_product11 <- sales %>% 
    filter(product_id == "P0011")%>%
    group_by(date)%>%
    summarize(price=median(price))
  
  ggplot(df_sales_product11)+
    geom_point(aes(x=date, y = price))+
    ggtitle("	Price in function of date (daily) for Product P0011")
  
  # We wish to go from this discrete version of the data to a functional object. 
  # To do so, we perform some regression splines.
  
  time <- as.numeric(df_sales_product11$date)
  
  # Here we few basis and knots are enough due to the staircase 
  Spline_b <- create.bspline.basis(range(time), nbasis = 5, norder = 4)
  plot(Spline_b)
  
  basismat <- eval.basis(time, Spline_b)
  
  result_coef = lsfit(basismat, df_sales_product11$price, intercept=FALSE)$coef
  my_smooth_func_11 = fd(result_coef, Spline_b)
  plot(my_smooth_func_11)
  
  # We did it with nbasis = 20, but which would be the most optimal number of B-Spline Control Points?
  # According to Harmening and Neuner (2016), the BIC criteria is a good method to use
  # the BIC criteria 
  
  # 1. Define a range of possible numbers of basis functions
  nbasis_range <- seq(from = 4, to = 50, by = 1)
  
  # 2. Calculate BIC for each number of basis functions
  bic_values <- numeric(length(nbasis_range))
  
  for (i in seq_along(nbasis_range)) {
    nbasis <- nbasis_range[i]
    Spline_b <- create.bspline.basis(range(time), nbasis = nbasis, norder = 4)
    basismat <- eval.basis(time, Spline_b)
    fit <- lm(df_sales_product11$price ~ basismat)
    bic_values[i] <- BIC(fit)
  }
  
  # 3. Find the optimal number of basis functions
  optimal_nbasis <- nbasis_range[which.min(bic_values)]
  print(optimal_nbasis) #we find that 19 is the optimal number of splines
  
  # let's adapt our previous B-Spline curve
  Spline_b <- create.bspline.basis(range(time), nbasis = 49, norder = 4)
  basismat <- eval.basis(time, Spline_b)
  
  result_coef = lsfit(basismat, df_sales_product11$price, intercept=FALSE)$coef
  my_smooth_func_11 = fd(result_coef, Spline_b)
  plot(my_smooth_func_11)
  
  # f) Smoothing by regression analysis for the price of all products ----
  
  df_price_wt_store <-sales%>%
    group_by(product_id,date)%>%
    summarize(price=median(price))
  
  df_price_wt_store$price_pred <-NA
  
  
  
  for (i in list_of_prod){
    
    df_price_wt_store_sub<-df_price_wt_store%>%
      filter(product_id==i)
    
    bic_values <- numeric(length(nbasis_range))
    
    for(j in seq_along(nbasis_range)){
      nbasis <- nbasis_range[j]
      nbasis <- nbasis_range[j]  
      Spline_b <- create.bspline.basis(range(time), nbasis = nbasis, norder = 4)
      basismat <- eval.basis(time, Spline_b)
      fit <- lm(df_price_wt_store_sub$price ~ basismat)
      bic_values[j] <- BIC(fit)
    }
    
    optimal_nbasis <- nbasis_range[which.min(bic_values)]
    
    Spline_b <- create.bspline.basis(range(time), nbasis = optimal_nbasis, norder = 4)
    basismat <- eval.basis(time, Spline_b)
    
    result_coef = lsfit(basismat, df_price_wt_store_sub$price, intercept=FALSE)$coef
    my_smooth_func = fd(result_coef, Spline_b)
    df_price_wt_store$price_pred[which(df_price_wt_store$product_id==i)] <- eval.fd(time, my_smooth_func)
    
  }
  
  
  ggplotly(ggplot(df_price_wt_store) +
             geom_line(aes(x = date, y = price, color = product_id)) +
             ggtitle("Sales price in function of date (daily) by product"))
  
  
  ggplotly(ggplot(df_price_wt_store) +
             geom_line(aes(x = date, y = price_pred, color = product_id)) +
             ggtitle("Sales price in function of date (daily) by product"))
  
  # Smoothing prices doesn't really make sense
  
  # let's delete all the variables we don't need before moving to the next part
  rm(basismat, df_sales_product11, df_sales_productx, fit, my_smooth_func_11, product11_data, smooth_func, smooth_values, Spline_b, bic_values, i, j, nbasis, nbasis_range, optimal_nbasis, result_coef, time)
  
  

  
  write.csv(sales,"full_sales.csv",row.names=FALSE)
  write.csv(df_price_wt_store,"Simplified_price.csv",row.names=FALSE)
  write.csv(sales_wt_store,"Simplified_sales.csv",row.names=FALSE)
  
# III - Functional PCA ----
  
  # Reload interesting data :
 
  sales <- read.csv("full_sales.csv")
  sales$date<-as.Date(sales$date)
  Simplified_price <- read.csv("Simplified_price.csv")
  Simplified_price$date<-as.Date(Simplified_price$date)
  Simplified_sales <- read.csv("Simplified_sales.csv")
  Simplified_sales$date<-as.Date(Simplified_sales$date)

  
  
# We want to use FPCA to see if some interesting patterns that causes variability can emerge. 
  
  # a) FPCA on total sales quantity by product ----

  functional_data_list_y <- split(Simplified_sales$sales_pred, Simplified_sales$product_id)
  functional_data_list_x <- split(as.numeric(Simplified_sales$date), Simplified_sales$product_id)
  n_time <- length(functional_data_list_x[[1]])
  
  FPCAobj <- FPCA(Ly=functional_data_list_y, Lt=functional_data_list_x,list(numBins=n_time))
  plot(FPCAobj)
  # Top left : all domain of values observed (logic we have series of equal length with the same domain)
  # Top right: Mean of sales
  # Bottom left: Variance explained 99% for the 3 first components.
  # Bottom righ: Three first eigen function : 
  
  #   - First component that looks like the mean functions : coefficient associated would indicate level of sales of the product.
  plot(x=unique(Simplified_sales$date),y=FPCAobj$phi[,1],type='l',xlab="Date",ylab = "Eigen function",main="First eigen function")
  
  #   - Second component : coefficient associated would indicate if a product is more bought during summer. We have pick in the middle of  the year and it decreases for winter.
  plot(x=unique(Simplified_sales$date),y=FPCAobj$phi[,2],type='l',xlab="Date",ylab = "Eigen function",main="Second eigen function")
  
  #   - Third component : Picks around october-december : the coefficient associated would indicate product that are bought for events in this period !
  plot(x=unique(Simplified_sales$date),y=FPCAobj$phi[,3],type='l',xlab="Date",ylab = "Eigen function",main="Third eigen function")
  
  
  # Let's analyse the coefficient : 
  df_fpca_res <- data.frame(FPCAobj$xiEst)
  df_fpca_res <- df_fpca_res[,c(1:3)]
  colnames(df_fpca_res) <- c("Coef_comp_1","Coef_comp_2","Coef_comp_3")
  row.names(df_fpca_res)<-names(functional_data_list_y)
  
  mat_df <- as.matrix(df_fpca_res)
  colnames(mat_df) <- c("Sales_amount","Sales_MAY-SEPT","Sales_OCT-APRL")
  
  levelplot(mat_df,
            main = "Level associated to each components",
            xlab ="Product",
            ylab ="Component interpretation",
            scales = list(x = list(rot = 60)), 
            col.regions = colorRampPalette(c("red","white", "blue"))(101),
            at=c(quantile(mat_df,seq(0,1,0.01)),0),
            pretty=TRUE)
  
  ggplotly(ggplot(Simplified_sales)+
    geom_line(aes(x=date,y=sales_pred,color=product_id)))
  ggplotly(ggplot(Simplified_sales)+
             geom_point(aes(x=date,y=total_sales,color=product_id)))
  
  
  # b) FPCA on sales quantity for a product ----
  
  df_p35<-sales%>%
    filter(product_id=="P0035")
  
  functional_data_list_y <- split(df_p35$sales_pred, df_p35$store_id)
  functional_data_list_x <- split(as.numeric(df_p35$date), df_p35$store_id)
  n_time <- length(functional_data_list_x[[1]])
  
  
  FPCAobj <- FPCA(Ly=functional_data_list_y, Lt=functional_data_list_x,list(numBins=n_time))
  # plot(FPCAobj) : you better not run 
  
  
  #   - First component that looks like the mean functions : coefficient associated would indicate level of sales of the product. Also there is a clear pattern during winter.
  plot(x=unique(df_p35$date),y=FPCAobj$phi[,1],type='l',xlab="Date",ylab = "Eigen function",main="First eigen function")
  
  #   - Second component : coefficient associated would indicate if a product is more bought after 2018.
  plot(x=unique(df_p35$date),y=FPCAobj$phi[,2],type='l',xlab="Date",ylab = "Eigen function",main="Second eigen function")
  
  #   - Third component : Looks to complete first component : sales during summer
  plot(x=unique(df_p35$date),y=FPCAobj$phi[,3],type='l',xlab="Date",ylab = "Eigen function",main="Third eigen function")
  
  
  
  df_fpca_res <- data.frame(FPCAobj$xiEst)
  df_fpca_res <- df_fpca_res[,c(1:3)]
  colnames(df_fpca_res) <- c("Coef_comp_1","Coef_comp_2","Coef_comp_3")
  row.names(df_fpca_res)<-names(functional_data_list_y)
  df_fpca_res<-arrange(df_fpca_res,Coef_comp_1,Coef_comp_2,Coef_comp_3)
  
  mat_df <- as.matrix(df_fpca_res)
  colnames(mat_df) <- c("Sales_amount_winter","Sales_after_2018","Sales_summer")
  
  levelplot(mat_df[1:50,],
            main = "Level associated to each components",
            xlab ="Store",
            ylab ="Component interpretation",
            scales = list(x = list(rot = 60)), 
            col.regions = colorRampPalette(c("red","white", "blue"))(101),
            at=unique(c(quantile(mat_df,seq(0,1,0.01)),0)),
            pretty=TRUE)
  
  levelplot(mat_df[51:100,],
            main = "Level associated to each components",
            xlab ="Store",
            ylab ="Component interpretation",
            scales = list(x = list(rot = 60)), 
            col.regions = colorRampPalette(c("red","white", "blue"))(101),
            at=unique(c(quantile(mat_df,seq(0,1,0.01)),0)),
            pretty=TRUE)
  
  levelplot(mat_df[101:144,],
            main = "Level associated to each components",
            xlab ="Store",
            ylab ="Component interpretation",
            scales = list(x = list(rot = 60)), 
            col.regions = colorRampPalette(c("red","white", "blue"))(101),
            at=unique(c(quantile(mat_df,seq(0,1,0.01)),0)),
            pretty=TRUE)
  
  ggplotly(ggplot(df_p35)+
             geom_line(aes(x=date,y=sales_pred,color=store_id)))

  
  df_fpca_res<-arrange(df_fpca_res,Coef_comp_3,Coef_comp_1,Coef_comp_2)
  
  mat_df <- as.matrix(df_fpca_res)
  colnames(mat_df) <- c("Sales_amount_winter","Sales_after_2018","Sales_summer")
  
  levelplot(mat_df[1:50,],
            main = "Level associated to each components",
            xlab ="Store",
            ylab ="Component interpretation",
            scales = list(x = list(rot = 60)), 
            col.regions = colorRampPalette(c("red","white", "blue"))(101),
            at=unique(c(quantile(mat_df,seq(0,1,0.01)),0)),
            pretty=TRUE)
  
  levelplot(mat_df[51:100,],
            main = "Level associated to each components",
            xlab ="Store",
            ylab ="Component interpretation",
            scales = list(x = list(rot = 60)), 
            col.regions = colorRampPalette(c("red","white", "blue"))(101),
            at=unique(c(quantile(mat_df,seq(0,1,0.01)),0)),
            pretty=TRUE)
  
  levelplot(mat_df[101:144,],
            main = "Level associated to each components",
            xlab ="Store",
            ylab ="Component interpretation",
            scales = list(x = list(rot = 60)), 
            col.regions = colorRampPalette(c("red","white", "blue"))(101),
            at=unique(c(quantile(mat_df,seq(0,1,0.01)),0)),
            pretty=TRUE)
  
  
