library(tidyverse)

# SQL code for combining PPP with delinquent taxpayers for matches. 

  # For PPP loans of $150k or more:
    # SELECT PPP_More_150.LoanNumber, D.flngCustomerKey as "Delinquent_CustomerKey", PPP_More_150.DateApproved, D.AmountDue, PPP_More_150.CurrentApprovalAmount, PPP_More_150.JobsReported, D.Name, PPP_More_150.BorrowerName, PPP_More_150.FranchiseName, PPP_More_150.BorrowerAddress, PPP_More_150.BorrowerCity, PPP_More_150.BorrowerZip
    # FROM D, PPP_More_150
    # WHERE D.Name LIKE PPP_More_150.BorrowerName

  # PPP loans of less than $150k with 07/09/21 delinquent taxpayer data:

    # SELECT PPP_Under_150.LoanNumber, D.flngCustomerKey as "Delinquent_CustomerKey", PPP_Under_150.DateApproved, D.AmountDue, PPP_Under_150.CurrentApprovalAmount, PPP_Under_150.JobsReported, D.Name, PPP_Under_150.BorrowerName, PPP_Under_150.FranchiseName, PPP_Under_150.BorrowerAddress, PPP_Under_150.BorrowerCity, PPP_Under_150.BorrowerZip
    # FROM D, PPP_Under_150
    # WHERE D.Name LIKE PPP_Under_150.BorrowerName

delinq <- read_csv("Delinquent Taxpayers PPP For Analysis_2.csv")

# To get totals #

delinq %>% 
  summarize(count = n(), total_owed = sum(AmountDue),
            total_paid = sum(CurrentApprovalAmount)) %>% 
  View()

## To get who owed the most and was paid ##

delinq %>% 
  group_by(Name) %>% 
  summarize(count = n(), total_owed = sum(AmountDue),
            total_paid = sum(CurrentApprovalAmount), difference_owed_paid = sum(Amount_PaidMoreThanTaxOwed)) %>% 
  arrange(desc(total_paid))

delinq %>% 
  group_by(Name) %>% 
  summarize(count = n(), total_owed = sum(AmountDue),
            total_paid = sum(CurrentApprovalAmount), difference_owed_paid = sum(Amount_PaidMoreThanTaxOwed)) %>% 
  arrange(desc(count = n())) %>% 
  View()

delinq <- read_csv("Delinquent Taxpayers PPP For Analysis_3.csv")

# Had to reimport bc the analysis showed repeats for Advanced Security when there shouldn't have been any repeats. 
# I went back into source CSV to ID the wrong line of data and re-did it

# Colorado Mushroom Farm LLC,  a mushroom farm based in Alamosa, owes $354,254.93 in taxes as of July 2021 data. But that didn't stop the company from getting two PPP loans totaling more than $1.7 million. 
# This $1.3 million gap between owed taxes and payment is the most significant chasm identified by the analysis. 

delinq %>% 
  group_by(Name) %>% 
  summarize(count = n(), total_owed = sum(AmountDue),
            total_paid = sum(CurrentApprovalAmount), difference_owed_paid = sum(Amount_PaidMoreThanTaxOwed)) %>% 
  arrange(desc(count = n())) %>% 
  View()

# Totals #

delinq %>% 
  summarize(count = n(), total_owed = sum(AmountDue),
            total_paid = sum(CurrentApprovalAmount))

# 161 companies and people received PPP payments while also owing the government millions in delinquent loans, a 9Wants to Know analysis of federal Small Business Administration and state Department of Revenue data found. 
# In total - Colorado is owed more than $6.6 million as of July 2021 but these entities were paid $13.2 million.
    # To be more precise with the July numbers: 
      # 161 companies/people paid and owe. 
      # $6,659,763 owed
      # $13,259,994 paid to these 161 companies/people.

delinq %>% 
  filter(grepl("Yes", 'YN_PaidMoreThanTaxOwed')) %>% 
  summarize(count = n(), number_paid_more = count(Delinquent_CustomerKey),total_owed = sum(AmountDue),
            total_paid = sum(CurrentApprovalAmount), total_jobs = sum(JobsReported)) %>% 
  View()

delinq %>% 
  filter(delinq, Amount_PaidMoreThanTaxOwed > 0) %>% 
  summarize(number_paid_more = n(),
            total_owed = sum(AmountDue),
            total_paid = sum(CurrentApprovalAmount),
           total_jobs = sum(JobsReported))


# Neither of the above methods worked, I emailed Charles Minshew at IRE about it to figure out what happened.
  # This is very doable in SQL and Excel.
  # Within tab "DelinqPPP_150-+_ForSQL", within the spreadsheet "Delinquent Taxpayers 20210709," I created a new column.
  # Put this formula into that column: =IF(H2>0,"Yes", "No")

  # Then ran calculations in SQL:
  # SELECT Name, NumberofLoans, NumberofDelinquency, JobsReported, AmountDue, CurrentApprovalAmount, Amount_PaidMoreThanTaxOwed, BorrowerAddress, BorrowerCity, BorrowerZip, Address2, City2, BorrorowerZip2, Address3, City3, BorrorowerZip3
  # FROM DTP
  # ORDER BY Amount_PaidMoreThanTaxOwed
  

# Want to figure out median gap between paid and tax owed:
  # Template:
    # dock_tab %>% 
    # summarize(average_payment = mean(july_to_oct_tax_diff), 
           # median_payment = median(july_to_oct_tax_diff))

delinq <- Delinquent_Taxpayers_PPP_For_Analysis_5 

delinq %>% 
  summarize(average_paid_more_than_owe = mean(PAIDMORE), 
            median_paid_more_than_owe = median(PAIDMORE))

  # Typically, entities were paid $2,041 more than they owed (median). $40,995 on average. Huge outliers pulled the average up significantly. 

# Let's try to make a bar chart with how many folks paid more than owed

delinq <-read_csv("Delinquent Taxpayers PPP For Analysis_4.csv") 

delinq %>% 
  group_by(NAME) %>% 
  summarize(paid_more = sum(PAIDMORE) %>% 
  ggplot(aes(y = reorder(NAME, paid_more),
             x = paid_more))) +
    geom_bar(stat = 'identity')


# I want to compare new delinquent taxpayer list to the old one to see how many folks paid off their taxes since July 
  # I kept delinquent customer key across all, so will be able to JOIN off of that. 
  # Going to do a LEFT JOIN off of "Delinquent Taxpayers PPP For Analysis_5" because then you can see how many stayed.
  # I grouped the delinquent taxpayer CSV in SQL and brought it back in. So going to do it again below. 
    # Will try anti-join below too. 

library(tidyverse)

  still_delinq <- delinq_tax_102921 %>% inner_join(Delinquent_Taxpayers_PPP_For_Analysis_5)
  View()

  # Going to export and re-import to see if that would solve the character issue
  
  still_delinq %>% write_csv("still_delinq.csv", na = "")
  
# There are a bunch of repeat customer keys because folks have the same tax key for personal and business. 
  # Going to add them together and group by that cust_key
  # owe_amt is the amount owed for taxes as of 10/29 
  

  still_delinq %>% 
    group_by(cust_key) %>% 
    summarize(oct_total_owe = sum(owe_amt)) %>% 
    View()
  
  # Attempt 2:
  
  aggregate(owe_amt ~ cust_key, still_delinq, sum)
 
  # Attempt 3
   
  aggregate(owe_amt ~ cust_key, still_delinq, sum)

  
# Would an anti-join be more effective to see who is no longer there? I am going to group the 10/29 data by cust key in SQL and then bring it back in 

  # This was the SQL code that I used to group the customer keys together in the updated delinq data
  
    # SELECT cust_key, count(cust_key) as "oct_num_cust_key_repeat", name, name_dba2, sum(owe_amt) AS "total_oct_owe"
    # FROM delinq_tax_102921
    # GROUP by cust_key
  
  
  # Need to delete empty columns first
   #  This worked for Aurora Police swipe data project
    # library(janitor)
    # df <- remove_empty(cassidee_carlson_entranceswipes, which = "cols")
  
  library(janitor)
  delinq_tax_102921_grouped <- remove_empty(delinq_tax_102921_grouped, which = "cols")
  
  # That worked well. Now let's see how many are still there. 
  
  still_delinq <- delinq_tax_102921_grouped %>% inner_join(Delinquent_Taxpayers_PPP_For_Analysis_5)

  still_delinq_102921 <- still_delinq
  
  still_delinq_102921 %>% write_csv("still_delinq_102921.csv", na = "")
  
  # 148 business are still on that list. 
  
  # Let's try the anti-join:
    # This resource is helpful: https://campus.datacamp.com/courses/introduction-to-spark-with-sparklyr-in-r/tools-of-the-trade-advanced-dplyr-usage?ex=12
  
  anti_join(Delinquent_Taxpayers_PPP_For_Analysis_5, delinq_tax_102921_grouped, by = "cust_key") %>% 
    View()

  # There are 13 businesses that paid off their delinquent taxes. Most notably AFTEROURS, a company we focused on, paid off the $1k that it was delinquent on.
  
  paid_off_102921 <- anti_join(Delinquent_Taxpayers_PPP_For_Analysis_5, delinq_tax_102921_grouped, by = "cust_key")

    paid_off_102921 %>% write_csv("paid_off_102921.csv", na = "")
  
# I want to see how much was paid off in delinquent taxes in those 148 that are still on the list. Even if they didn't pay it all of, they may have paid more.
  # This Reddit post was helpful: https://www.reddit.com/r/Rlanguage/comments/aw3nkb/subtracting_data_in_column_from_data_in_another/
    
    still_delinq_102921 %>% 
      mutate(july_to_oct_tax_diff = total_oct_owe - AmountDue) %>% 
      View()
    
    still_delinq_102921_2 <- still_delinq_102921 %>% 
      mutate(july_to_oct_tax_diff = total_oct_owe - AmountDue) %>% 
      arrange(desc(july_to_oct_tax_diff)) %>% 
      View()
    
    still_delinq_102921_2 %>% write_csv("still_delinq_102921_2.csv", na = "")
  
    # How many added to their tab?
    
    still_delinq_102921_2 %>% 
      filter(july_to_oct_tax_diff > 0) %>% 
    View()
    
    add_tab <- still_delinq_102921_2 %>% 
      filter(july_to_oct_tax_diff > 0)
    
    add_tab %>% write_csv("add_tab.csv", na = "")
    
     # 66 companies added delinquent taxes to their tab since July. 
    
    # What is average and median of what companies added?
    
    add_tab %>% 
      summarize(average_payment = mean(july_to_oct_tax_diff), 
                median_payment = median(july_to_oct_tax_diff))
    
    # Companies that added $8,709 on average and a median of $1,351 to their taxpayer debt 
    
    # How many paid off taxes that they owe?
    
    still_delinq_102921_2 %>% 
      filter(july_to_oct_tax_diff < 0) %>% 
      View()
    
    dock_tab <-  still_delinq_102921_2 %>% 
      filter(july_to_oct_tax_diff < 0)
    
    dock_tab %>% write_csv("dock_tab.csv", na = "")
    
    
    # 61 companies paid off some of what they owe, but are still delinquent. 
    
    # What is average and median of what companies paid off?
    
    dock_tab %>% 
      summarize(average_payment = mean(july_to_oct_tax_diff), 
                median_payment = median(july_to_oct_tax_diff))

    # Companies paid $4,228 on average and a median of $2,193.
    
    # How many had no movement whatsoever since July?
    
    still_delinq_102921_2 %>% 
      filter(july_to_oct_tax_diff == 0) %>% 
      View()
    
    # 21 companies had no movement. 
    
# Oct 2021 Totals #
    
    library(tidyverse)
    
    still_delinq_102921_2 %>% 
      summarize(count = n(), all_total_oct_owe = sum(total_oct_owe),
                total_paid = sum(CurrentApprovalAmount))
    
    # 148 companies and people received PPP payments while also owing the government millions in delinquent loans, a 9Wants to Know analysis of federal Small Business Administration and state Department of Revenue data found. 
    # In total in October - Colorado is owed $6,681,178
    # Colorado is owed more than $6.6 million as of July 2021 but these entities were paid $13.2 million.
    # Need to add up the total_oct_owe column to make sure the sum function is focused on that time period. 
    # There is a difference of $21,414.81 across ALL companies/people. The growth/decrease of the amount owed can vary greatly between companies/people.

    
    #Median amount owed:
    
    still_delinq_102921_2 %>% 
    summarize(average_owed = mean(total_oct_owe), 
              median_owed = median(total_oct_owe))
    
    # Companies and people owed $45,143 on average and a median of $30,396.
    
    # Redownload because I accidentally deleted the CSV itself:
    Delinquent_Taxpayers_PPP_For_Analysis_5 %>% write_csv("Delinquent_Taxpayers_PPP_For_Analysis_5.csv", na = "")
    
# I want to pull in the most updated PPP numbers to see how many loans were approved, $ amount of how much was approved and how much was forgiven.
    # Data source: https://data.sba.gov/dataset/ppp-foia
    # Metadata said it was updated on 07/01/21. Email with Chris Chavez at SBA said there isn't any more recent data. 
    # Two parts: Up to 150k and and 150k+
    
    plus_150 <- CO_PPP_public_150k_plus_070121
    upto_150 <- CO_PPP_public_up_to_150k_2_070121
    
    plus_150 %>% 
      summarize(total_approved = sum(CurrentApprovalAmount), 
                total_forgiven = sum(ForgivenessAmount))
    
    # Returned NA because there are some NA's in the forgiveness column. Need to tell R to ignore it.
    # Add this to ignore blanks: na.rm = TRUE
      # Source: https://datacarpentry.org/R-genomics/04-dplyr.html 
    # THIS WORKED!
    
    plus_150 %>% 
      summarize(total_approved = sum(CurrentApprovalAmount), 
                total_forgiven = sum(ForgivenessAmount, na.rm = TRUE))
    
    # So, for loans more than 150k:
      # $10,034,192,612 was approved.
      # $5,701,354,333 was forgiven. 
    
    # This checks out in SQL too! :) 
      # SELECT sum(CurrentApprovalAmount), sum(ForgivenessAmount)
      # FROM plus_150
  
    5701354333/10034192612
    
    (5701354333/10034192612)*100
    
      # So for the 150k category, 56% of the funds approved have been forgiven. 
    
    # Let's do the same thing for the up to 150k crowd
    
    upto_150 %>% 
      summarize(total_approved = sum(CurrentApprovalAmount), 
                total_forgiven = sum(ForgivenessAmount, na.rm = TRUE))
    
    # So, for loans up to 150k:
       # $5,075,656,773 was approved.
       # $2,733,816,019 was forgiven.
    
    # Got same numbers in SQL:
      # SELECT sum(CurrentApprovalAmount), sum(ForgivenessAmount)
      # FROM upto_150
    
    # Adding approved amounts from 150k+ and up to 150k:
    
     10034192612 + 5075656773
     
         # Total approved funding: $15,109,849,385
    
    # Adding forgiven amounts from 150k+ and up to 150k:
    
     5701354333 + 2733816019
    
     # Total forgiven funding: $8,435,170,352
     
     # Percentage of funding forgiven: 
     
     (8435170352/15109849385)*100
     
     