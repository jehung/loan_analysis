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
         'LoanOriginalAmount', 'Recommendations', 'Investors')
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
rel = ggplot(data=loan_subset, aes(x=ProsperRatingAlpha, y=LenderYield)) + geom_boxplot() +
    facet_wrap(~Term)
rel
    
## Explore relationship between # Recommendations and # Investors, and Term
rec = ggplot(data=loan, aes(x=Recommendations, y=Investors)) + geom_point() +
    facet_wrap(~Term) + 
    scale_x_continuous(breaks=seq(0, 5, 1), limit=c(0,5))
rec

## Noticed that LendersYield are lower for high recommendations. Why?
rec1 = ggplot(data=loan, aes(x=Recommendations, y=LenderYield)) + geom_point() +
    facet_wrap(~Term) 
    scale_x_continuous(breaks=seq(0, 5, 1), limit=c(0,5))
rec1

## Explore if high recoomendations are associated with bad credit
## By investigating if higher BorrowerRate is related to lower ProsperRatingAlpha
rate_rating = ggplot(data=loan, aes(x=ProsperRatingAlpha, y=Recommendations)) + geom_boxplot()
rate_rating

## Is the above confounded by length of loan term?
## No, the above pattern holds for all temrs
rate_groups = group_by(loan, ProsperRatingAlpha)
rate_rec = summarise(rate_groups, 
                      rec_mean = mean(Recommendations),
                      rec_median = median(Recommendations),
                      n =n())
rate_rec = arrange(rate_rec, ProsperRatingAlpha)

insight1 = ggplot(rate_rec, aes(ProsperRatingAlpha, y=rec_mean, group=1)) + geom_line() +
    ylab('Number of Recommendations')
insight1


## A summary table by # of Recommendation, mean yield, and term
rec_groups = group_by(loan, Recommendations)
rec_rate = summarise(rec_groups,
                     mean_yield = mean(LenderYield),
                     median_yield = median(LenderYield),
                     term12 = sum(Term == 12),
                     term36 = sum(Term == 36),
                     term60 = sum(Term == 60),
                     n =n())
rec_rate = arrange(rec_rate, Recommendations)


insight2 = ggplot(rec_rate, aes(Recommendations, y=term12, group=1)) + geom_line() + 
    scale_x_continuous(breaks=seq(0, 5, 1), limit=c(0, 5))
insight3 = ggplot(rec_rate, aes(Recommendations, y=term36, group=1)) + geom_line() +
    scale_x_continuous(breaks=seq(0, 5, 1), limit=c(0, 5))
insight4 = ggplot(rec_rate, aes(Recommendations, y=term60, group=1)) + geom_line() +
    scale_x_continuous(breaks=seq(0, 5, 1), limit=c(0, 5))

grid.arrange(insight2, insight3, insight4, ncol=3)


## What happens when the credit rating is NA?