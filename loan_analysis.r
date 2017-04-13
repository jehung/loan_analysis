library(ggplot2)
library(GGally)
library(dplyr)
library(reshape2)
library(gridExtra)

## TODO: IDEAS
## My questiosn to be answered:
## 1. What are the factors that influences the BorrowersRate (conjectured: FICO, Rating, Term, Job v. HomeOwnership)
## 2. What are the factors that influences the LendersYield (conjectured: FICNAD, Rating, Term)
## 3. What are the relationship between Loan status and Lender's Yield (are they normally distributed, boxplot)
## 4. What are the relationship between # of investors, lendersYield, Rating/FICO, and # of recommendations
## 5. What about impact of job or ownership's on loan status, borrowers rate, and status

## first subset the data for ease of handling and exploration
loan = read.csv('prosperLoanData.csv', header=TRUE)

## Make ProsperRatingNumeric a factor variable
loan$ProsperRatingNumeric = factor(loan$ProsperRatingNumeric, levels=c(0, 1, 2, 3, 4, 5, 6, 7))
loan$ProsperRatingAlpha = factor(loan$ProsperRatingAlpha, levels=c('N/A', 'HR', 'E', 'D', 'C', 'B', 'A', 'AA'))
loan$Term = factor(loan$Term, levels=c(12, 36, 60))


index = sample(nrow(loan), 1000)
cols = c('CreditGrade', 'Term', 'LoanStatus', 'BorrowerRate', 'LenderYield', 'EstimatedLoss', 'EstimatedReturn', 
         'ProsperRatingNumeric', 'ProsperRatingAlpha', 'ProsperScore', 'ListingCategory..numeric.',
         'EmploymentStatus', 'EmploymentStatusDuration', 'IsBorrowerHomeowner', 'ProsperPrincipalBorrowed',
         'LoanOriginalAmount', 'Recommendations', 'Investors', 'DebtToIncomeRatio', 'StatedMonthlyIncome', 'MonthlyLoanPayment')
loan_subset = loan[index, cols]

## Explore correlation between CreidtGrade and and ProsperRatingA
## Explore correlation between BorrowerRate and LendersYield
s1 = ggplot(data=loan, aes(x=BorrowerRate, y=LenderYield)) + geom_point()
s1
with(loan, cor.test(BorrowerRate, LenderYield))

## Explore distribution of ProsperRatingNumeric
table(loan$ProsperRatingNumeric)
numeric = ggplot(loan, aes(x=ProsperRatingNumeric)) + geom_bar()
numeric

## Explore distribution of ProsperRatingAlpha
table(loan$ProsperRatingAlpha)
alpha = ggplot(loan, aes(x=ProsperRatingAlpha)) + geom_bar()
alpha

## Explore distribution of BorrowerState
state = ggplot(loan, aes(x=BorrowerState)) + geom_bar() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
state

## Explore distribution of occupations status
employ_status = ggplot(loan, aes(x=EmploymentStatus)) + geom_bar() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
employ_status

## Explore distribution of Term
term = ggplot(loan, aes(x=Term)) + geom_bar() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
term

## Explore relationship between PropserRatingAlpha, LenderYield, and Term
## Major finding
rel = ggplot(data=loan_subset, aes(x=ProsperRatingAlpha, y=LenderYield)) + geom_boxplot() +
    facet_wrap(~Term)
rel
    
## Explore relationship between # Recommendations and # Investors, and Term
## Another major finding- more recommendations does not lead to more investors
## why? Is more recommendation a sign of bad credit?
rec = ggplot(data=loan, aes(x=Recommendations, y=Investors)) + geom_point() +
    facet_wrap(~Term) + scale_x_continuous(breaks=seq(0, 5, 1), limit=c(0,5))
rec

## Noticed that LendersYield are lower for high recommendations. Why?
## Is there a relationship between Recommendations and LendersYield?
## skipped
rec1 = ggplot(data=loan, aes(x=Recommendations, y=LenderYield)) + geom_point(alpha=0.1) +
    facet_wrap(~Term) 
    scale_x_continuous(breaks=seq(0, 5, 1), limit=c(0,5))
rec1

## One way to figure the above question is to see if Recommendations are correlated with CreditRatings
## Now here
recRating = ggplot(data=loan_subset, aes(x=ProsperRatingAlpha, y=Recommendations)) + geom_boxplot() +
    facet_wrap(~Term)
#+ facet_wrap(~Term) + scale_x_continuous(breaks=seq(0, 5, 1), limit=c(0,5))
recRating


## Explore if high recoomendations are associated with bad credit
## By investigating if higher BorrowerRate is related to lower ProsperRatingAlpha
rate_rating = ggplot(data=loan, aes(x=ProsperRatingAlpha, y=Recommendations)) + geom_boxplot()
rate_rating

## Is the above confounded by length of loan term?
## No, the above pattern holds for all temrs
### Major finding
rate_groups = group_by(loan, ProsperRatingAlpha)
rate_rec = summarise(rate_groups, 
                      recommendation_mean = mean(Recommendations),
                      recommendation_median = median(Recommendations),
                      recommendation_sd = sd(Recommendations))
rate_rec = arrange(rate_rec, ProsperRatingAlpha)

melted = melt(rate_rec, id='ProsperRatingAlpha')

insight1 = ggplot(melted, aes(ProsperRatingAlpha, y=value, color=variable, group=variable)) + geom_line() +
    geom_point() + ylab('Recommendations of Loan')
insight1

## A summary table by # of Recommendation, mean yield, and term
## Major finding 
rec_groups = group_by(loan, Recommendations)
rec_rate = summarise(rec_groups,
                     mean_yield = mean(LenderYield),
                     median_yield = median(LenderYield),
                     term12 = sum(Term == 12),
                     term36 = sum(Term == 36),
                     term60 = sum(Term == 60))
rec_rate = arrange(rec_rate, Recommendations)

rec_rate

insight2 = ggplot(rec_rate, aes(factor(Recommendations), y=mean_yield, group=1)) + geom_point() + geom_line() +
    xlab('Number of Recommendations') + ylab('Mean LenderYield')
insight2


## What happens when the credit rating is NA?



## Investigate the relationship between LendersYield and DebtIncome ratio
## Find correlation r - low
with(loan, cor.test(DebtToIncomeRatio, LenderYield))

debt_income_sub = ggplot(subset(loan_subset, !is.na(DebtToIncomeRatio)), aes(DebtToIncomeRatio, LenderYield)) + 
    geom_point(alpha=0.05)
debt_income_sub


debt_income = ggplot(subset(loan, !is.na(DebtToIncomeRatio)), aes(DebtToIncomeRatio, LenderYield)) + 
    geom_point(alpha=0.05)
debt_income


## use this graph
debt_income_log = ggplot(subset(loan_subset, !is.na(DebtToIncomeRatio)), aes(DebtToIncomeRatio, LenderYield)) + 
    geom_point(alpha=0.05) + scale_x_log10() + scale_y_log10() +
    facet_wrap(~ProsperRatingNumeric)
debt_income_log


## Investigate the relationship between LendersYield and Payment/Income ratio
## Find correlation r
loan['PaymentIncomeRatio'] = loan['MonthlyLoanPayment'] / loan['StatedMonthlyIncome']
loan['PaymentIncomeRatio'][loan['PaymentIncomeRatio']==Inf] = NA


with(subset(loan, !is.na(PaymentIncomeRatio)), cor.test(PaymentIncomeRatio, LenderYield))

pmt_income = ggplot(subset(loan, !is.na(PaymentIncomeRatio)), aes(PaymentIncomeRatio, LenderYield)) + 
    geom_point(alpha=0.05) + facet_wrap(~ProsperRatingNumeric) + scale_x_continuous(limit=c(0,1)) +
    scale_x_log10() + scale_y_log10()
    
pmt_income


## Conclude whether DebtIncome ratio is highly correlated with PaymentIncoem ratio
## Find correlation r
with(loan, cor.test(DebtToIncomeRatio, PaymentIncomeRatio))



