library(ggplot2)
library(reshape2)
library(dplyr)
library(RColorBrewer)

# cohort analysis and charts

cohort_total <- data.frame(cohort=c('january', 'feburary', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december'),
                           month1=c(15000,0,0,0,0,0,0,0,0,0,0,0),
                           month2=c(60000,325000,0,0,0,0,0,0,0,0,0,0),
                           month3=c(67000,56000,133000,0,0,0,0,0,0,0,0,0),
                           month4=c(63000,35000,85000,420000,0,0,0,0,0,0,0,0),
                           month5=c(50000,45000,60000,80000,288000,0,0,0,0,0,0,0),
                           month6=c(51000,52000,55000,51000,58000,253000,0,0,0,0,0,0),
                           month7=c(51000,69000,48000,45000,42000,54000,272000,0,0,0,0,0),
                           month8=c(46000,85000,77000,41000,38000,37000,74000,352000,0,0,0,0),
                           month9=c(38000,42000,72000,41000,31000,30000,49000,107000,285000,0,0,0),
                           month10=c(39000,38000,45000,33000,34000,34000,46000,83000,69000,279000,0,0),
                           month11=c(38000,42000,31000,32000,26000,28000,43000,82000,51000,87000,282000,0),
                           month12=c(35000,35000,38000,45000,35000,32000,48000,44000,47000,52000,92000,500000))

View(cohort_total)

cohort_fig1 <- melt(cohort_total, id.vars = 'cohort')

colnames(cohort_fig1) <- c('cohort', 'month', 'revenue')

palette1 <- colorRampPalette(c('#fde0dd',
                                '#fcc5c0',
                                '#fa9fb5',
                                '#f768a1',
                                '#dd3497',
                                '#ae017e',
                                '#7a0177'))
                                
                                p1 <- ggplot(cohort_fig1, aes(x=month, y=revenue, group=cohort))
                                p1 <-p1 + geom_area(aes(fill = cohort)) +
                                  scale_fill_manual(values = palette1(nrow(cohort_total))) +
                                  ggtitle('Total revenue by Cohort')
                                
                                p1 + theme(panel.background = element_blank())
                                
                                cohort_customers <- data.frame(cohort=c('january', 'feburary', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december'),
                                                               month1=c(13000,0,0,0,0,0,0,0,0,0,0,0),
                                                               month2=c(2100,12000,0,0,0,0,0,0,0,0,0,0),
                                                               month3=c(1900,2000,11500,0,0,0,0,0,0,0,0,0),
                                                               month4=c(1800,1300,2400,13200,0,0,0,0,0,0,0,0),
                                                               month5=c(2000,1100,1400,3200,11100,0,0,0,0,0,0,0),
                                                               month6=c(700,900,1200,1600,1900,10300,0,0,0,0,0,0),
                                                               month7=c(560,900,1100,1300,1100,1900,12000,0,0,0,0,0),
                                                               month8=c(920,750,1000,1200,1100,1300,1900,11500,0,0,0,0),
                                                               month9=c(800,800,950,1100,1400,1250,1000,1200,11000,0,0,0),
                                                               month10=c(800,780,900,1050,1050,1200,700,1200,1500,14200,0,0),
                                                               month11=c(750,750,900,1200,1800,1180,800,1100,1150,3200,12300,0),
                                                               month12=c(740,700,870,1000,920,1300,640,1050,1025,1300,1600,12000))
                                
                                View(cohort_customers)
                                
                                cohort_fig2 <- melt(cohort_customers, id.vars = 'cohort')
                                colnames(cohort_fig2) <- c('cohort', 'month', 'no_of_clients')
                                
                                #View(cohort_fig2)
                                
                                num_pal <-colorRampPalette(c('#e0f3db',
                                                              '#ccebc5',
                                                              '#a8ddb5',
                                                              '#7bccc4',
                                                              '#4eb3d3',
                                                              '#2b8cbe',
                                                              '#0868ac'))
                                                              
                                                              pp <- ggplot(cohort_fig2, aes(x=month, y=no_of_clients, group=cohort))
                                                              pp <-pp + geom_area(aes(fill = cohort)) +
                                                                scale_fill_manual(values = num_pal(nrow(cohort_customers))) +
                                                                ggtitle('Clients by Cohort')
                                                              
                                                              pp + theme(panel.background = element_blank())
                                                              
                                                              #dividing the data frames
                                                              revenue_per_user <- cohort_total[,c(2:13)]/cohort_customers[,c(2:13)]
                                                              revenue_per_user[is.na(revenue_per_user)] <- 0
                                                              revenue_per_user <- cbind(cohort_total[,1], revenue_per_user)
                                                              
                                                              View(revenue_per_user)
                                                              
                                                              oranges <-colorRampPalette(c('#fff7bc',
                                                                                            '#fee391',
                                                                                            '#fec44f',
                                                                                            '#fe9929',
                                                                                            '#ec7014',
                                                                                            '#cc4c02',
                                                                                            '#993404'))
                                                                                            
                                                                                            cohort_rev_plot <- melt(revenue_per_user, id.vars = 'cohort_total[, 1]')
                                                                                            colnames(cohort_rev_plot) <- c('cohort', 'month', 'avg_revenue')
                                                                                            pc <- ggplot(cohort_rev_plot, aes(x=month, y=avg_revenue, group=cohort))
                                                                                            pc <-pc + geom_area(aes(fill = cohort)) +
                                                                                              scale_fill_manual(values = oranges(nrow(cohort_customers))) +
                                                                                              ggtitle('Average revenue per customer by Cohort')
                                                                                            
                                                                                            pc + theme(panel.background = element_blank())
                                                                                            
                                                                                            
# Retention Analysis
                                                                                            
cohort_users <- data.frame(cohort=c('january', 'feburary', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december'),
                           month1=c(133000,0,0,0,0,0,0,0,0,0,0,0),
                           month2=c(3400,10300,0,0,0,0,0,0,0,0,0,0),
                           month3=c(1100,3000,10500,0,0,0,0,0,0,0,0,0),
                           month4=c(2300,4300,6200,7000,0,0,0,0,0,0,0,0),
                           month5=c(4000,1100,1400,2400,9100,0,0,0,0,0,0,0),
                           month6=c(300,500,3200,2200,2900,14000,0,0,0,0,0,0),
                           month7=c(600,900,1100,1300,1400,1800,12000,0,0,0,0,0),
                           month8=c(900,1200,1000,1200,1100,1300,1800,13600,0,0,0,0),
                           month9=c(700,700,750,3400,2100,1330,1000,1400,12000,0,0,0),
                           month10=c(820,780,800,1100,1350,1200,900,1400,1800,15200,0,0),
                           month11=c(1000,750,900,1000,1000,1180,800,1100,1150,2000,12300,0),
                           month12=c(650,700,870,800,600,1300,500,1150,1250,1300,1800,25000))
                                                                                            
View(cohort_users)
                                                                                            
cohort_users1 <- cohort_users 
totcols <- ncol(cohort_users1)
for (i in 1:nrow(cohort_users1)) { 
  df <- cohort_users1[i,] 
  df <- df[ , !df[]==0] 
  partcols <- ncol(df)
                                                                                              
  if (partcols < totcols) df[, c((partcols+1):totcols)] <- 0
  cohort_users1[i,] <- df 
  }
                                                                                            
#retention
a <- cohort_users1[,c(2:13)]
b <- cohort_users1[,2]
retention <- apply(a, 2, function(a) a/b )
retention <- data.frame(cohort=(cohort_users1$cohort), retention)
View(retention)
retention <- retention[,-2]
                                                                                            
cohort_plot <- melt(retention, id.vars = 'cohort')
colnames(cohort_plot) <- c('cohort', 'month', 'retention')
cohort_plot <- filter(cohort_plot, retention != 0)
c <- ggplot(cohort_plot, aes(x=month, y=retention, group=cohort, colour=cohort))
c <-c + geom_line(size=2, alpha=0.5) +
  geom_point(size=3, alpha=1) +
  geom_smooth(aes(group=1), method = 'loess', size=3, colour='turquoise', se=FALSE) +
  labs(title='Cohorts Retention ratio')
                                                                                            
c + scale_colour_brewer(palette="Set3") + theme(panel.background = element_blank())
                                                                                                                    
#Third Month Retention Analysis
cohort_plot1 <- filter(cohort_plot, month=='month3') 
c <- ggplot(cohort_plot1, aes(x=cohort, y=retention, colour=cohort))
c <- c + geom_point(size=3) +
  geom_line(aes(group=1), size=2, alpha=0.9) +
  geom_smooth(aes(group=1), size=2, colour='turquoise', method = 'lm', se=FALSE) +
  labs(title="How does the users retain in their third month?")
                                                                                                                    
c + scale_colour_brewer(palette="Pastel2") + theme(panel.background = element_blank())
                                                                                            
# cycle plot
cohort_cycle_plot <- cohort_plot
cohort_cycle_plot <- mutate(cohort_cycle_plot, month_cohort = paste(month, cohort))
cp <- ggplot(cohort_cycle_plot, aes(x=month_cohort, y=retention, group=month, colour=month))
                                                                                            
d1 <- filter(cohort_cycle_plot, cohort=='august')
d2 <- filter(cohort_cycle_plot, cohort=='september')
                                                                                            
cp <- cp + geom_point(size=3) +
  geom_line(aes(group=month), size=2, alpha=1/2) +
  labs(title="Cycle plot of Cohorts Retention") +
  geom_line(data=d1, aes(group=1), colour='turquoise', size=2, alpha=0.6) +
  geom_line(data=d2, aes(group=1), colour='turquoise', size=2, alpha=0.6) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
                                                                                            
cp + scale_colour_brewer(palette="Spectral") + theme(panel.background = element_blank())
                                                                                            