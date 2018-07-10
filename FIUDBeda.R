library(dplyr)
library(scales)
path = "/Users/apoorvahavanur/Documents/School/2017-2018/Other/TCinGC/fiu/FIUExport/"
ctr_df = read.csv(paste(path,"CTRs.csv",sep=''))
str_df = read.csv(paste(path,"STRs.csv",sep=''))
case_df = read.csv(paste(path,"Cases.csv",sep=''))
immigration_df = read.csv(paste(path,"ImmigrationRecords.csv",sep=''))
customs_df = read.csv(paste(path,"CustomsRecords.csv",sep=''))
pit_df = read.csv(paste(path,"PersonInTransaction.csv",sep=''))

ctr_df$dateOfTransaction = as.Date(strptime(ctr_df$dateOfTransaction, "%m/%d/%Y %H:%M:%S"))
ctr_df$cashAmount = as.numeric(gsub("[\\$,]", "", as.character(ctr_df$cashAmount)))
ctr_df$fullNameOfFinancialInstitution = as.character(toupper(ctr_df$fullNameOfFinancialInstitution))
ctr_df$cashAmount[which(is.na(ctr_df$cashAmount))] = 0
hist(as.Date(ctr_df$dateOfTransaction), breaks = 36)

pit_df$cashAmount =  as.numeric(gsub("[\\$,]", "", as.character(pit_df$cashAmount)))
pit_df$cashAmount[which(is.na(pit_df$cashAmount))] = 0



ctr_by_month = ctr_df %>%
  mutate(month = format(dateOfTransaction, "%m"), year = format(dateOfTransaction, "%Y")) %>%
  group_by(month, year, fullNameOfFinancialInstitution, cashDirection) %>%
  summarise(total = sum(cashAmount), count = n())

ctr_direction_by_month = ctr_df %>%
  mutate(month = format(dateOfTransaction, "%m"), year = format(dateOfTransaction, "%Y")) %>%
  group_by(month, year, fullNameOfFinancialInstitution, cashDirection) %>%
  summarise(count = n())

View(ctr_direction_by_month)

View(ctr_by_month)
hist(ctr_by_month$total)
ctr_by_month = ctr_by_month[-(which(ctr_by_month$year < 2015)),]
ctr_by_month$date = as.Date(paste(ctr_by_month$year, ctr_by_month$month, "01", sep="-"), "%Y-%m-%d", origin = "1960-10-01")
ctr_by_month$avg = ctr_by_month$total/ctr_by_month$count

deposits = ctr_by_month[which(ctr_by_month$cashDirection == 'Deposit'),]
withdrawals = ctr_by_month[which(ctr_by_month$cashDirection == 'Withdrawal'),]

withdrawals$total = -1 * withdrawals$total 
withdrawals$count = -1 * withdrawals$count
withdrawals$avg = -1 * withdrawals$avg

net = deposits
net$total = net$total + withdrawals$total
net$count = net$count + withdrawals$count
net$avg = net$avg + withdrawals$avg

plot(ctr_by_month$date, ctr_by_month$total)
library(ggplot2)
rnge = seq(min(withdrawals$total)-500000, max(deposits$total)+500000, 1)
w_d_total <- ggplot(NULL, aes(date, total)) + 
  geom_bar(stat = "identity", aes(fill = fullNameOfFinancialInstitution), data = deposits, fill = "forest green") +
  geom_bar(stat = "identity", aes(fill = fullNameOfFinancialInstitution), data = withdrawals, fill = "red") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) + ylab("Total Amount ($)") + ggtitle("CTR Cash Flow Per Bank", subtitle = "Deposits in Green, Withdrawals in Red") + facet_wrap(~fullNameOfFinancialInstitution) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(breaks=rnge[rnge%%500000 == 0])
w_d_total

rnge = seq(min(net$total)-500000, max(net$total)+500000, 1)
net_total <- ggplot(NULL, aes(date, total)) + 
  geom_bar(stat = "identity", aes(fill = fullNameOfFinancialInstitution), data = net, fill = "dark blue") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) + ylab("Total Amount ($)") + ggtitle("Net CTR Cash Flow Per Bank", subtitle = "Deposits Minus Withdrawals") + facet_wrap(~fullNameOfFinancialInstitution) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(breaks=rnge[rnge%%500000 == 0])
net_total


w_d_count <- ggplot(NULL, aes(date, count)) + 
  geom_bar(stat = "identity", aes(fill = fullNameOfFinancialInstitution), data = deposits, fill = "forest green") +
  geom_bar(stat = "identity", aes(fill = fullNameOfFinancialInstitution), data = withdrawals, fill = "red") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
w_d_count

rnge = seq(min(withdrawals$avg)-5000, max(deposits$avg)+ 5000, 1)
w_d_avg <- ggplot(NULL, aes(date, avg)) + 
  geom_bar(stat = "identity", aes(fill = fullNameOfFinancialInstitution), data = deposits, fill = "forest green") +
  geom_bar(stat = "identity", aes(fill = fullNameOfFinancialInstitution), data = withdrawals, fill = "red") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y")) + ylab("Avg. Amount ($)") + ggtitle("Avg. CTR Cash Flow Per Bank", subtitle = "Deposits in Green, Withdrawals in Red") + facet_wrap(~fullNameOfFinancialInstitution) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(breaks=rnge[rnge%%5000 == 0])
w_d_avg

rnge = round(seq(min(net$avg)-5000, max(net$avg) + 5000, 1))
net_avg <- ggplot(NULL, aes(date, avg)) + 
  geom_bar(stat = "identity", aes(fill = fullNameOfFinancialInstitution), data = net, fill = "dark blue") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y")) + ylab("Net Amount ($)") + ggtitle("Net Avg. CTR Cash Flow Per Bank", subtitle = "Deposits Minus Withdrawals") + facet_wrap(~fullNameOfFinancialInstitution) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(breaks=rnge[rnge%%5000 == 0])
net_avg

library(tidyr)
pit_df = separate_rows(pit_df,accountNumbers,sep="/")
pit_df = separate_rows(pit_df,accountNumbers,sep=",")
pit_df_by_ctr = pit_df %>% group_by(CTRID, cashDirection, relationshipToTransaction) %>% summarise(total = sum(cashAmount), count = n())
pit_df_by_account = pit_df %>% group_by(accountNumbers, cashDirection, relationshipToTransaction) %>% summarise(total = sum(cashAmount), count = n())

unique(toupper(pit_df$'lastNameOrNameOfEntity'[(pit_df$accountNumbers %in% pit_df_by_account$accountNumbers[which(pit_df_by_account$count > 500)])]))

unique(pit_df$firstName[which(toupper(pit_df$lastNameOrNameOfEntity) == 'SUZUKI')])


str_df = separate_rows(str_df,accountNumbers,sep="/")
str_df = separate_rows(str_df,accountNumbers,sep=",")
str_df$startDateOfSuspiciousActivity = as.Date(strptime(str_df$startDateOfSuspiciousActivity, "%m/%d/%Y %H:%M:%S"))
str_df$endDateOfSuspiciousActivity = as.Date(strptime(str_df$endDateOfSuspiciousActivity, "%m/%d/%Y %H:%M:%S"))
str_df$amountOfCash =  as.numeric(gsub("[\\$,]", "", as.character(str_df$amountOfCash)))
str_df$strDateGenerate = as.Date(strptime(str_df$strDateGenerate, "%m/%d/%Y %H:%M:%S"))
str_df$duration = str_df$endDateOfSuspiciousActivity - str_df$startDateOfSuspiciousActivity
str_df$narrative = as.character(str_df$narrative)
str_df$duration[which(is.na(str_df$duration))] = 0
str_df$duration[which(str_df$duration < 0)] = 0
library(janeaustenr)
library(tidytext)
str_df %>% unnest_tokens(word, narrative)
narrativetext = str_df[,c('STRID', 'narrative')]
narrative_counts = narrativetext %>% unnest_tokens(word, narrative) %>% count(STRID, word, sort = TRUE) %>%
  ungroup()
total_words <- narrative_counts %>% 
  group_by(STRID) %>% 
  summarize(total = sum(n))
narrative_counts <- left_join(narrative_counts, total_words)
View(narrative_counts)
narrative_counts <- narrative_counts %>%
  bind_tf_idf(word, STRID, n)
a = narrative_counts %>% mutate(word = word) %>% group_by(word) %>% summarise(avg_tf_idf = mean(tf_idf), count = n())
