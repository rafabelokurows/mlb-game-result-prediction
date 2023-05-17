pitchers = baseballr::bref_daily_pitcher(Sys.Date()-30,Sys.Date())

summary(lm(as.numeric(W)~.,data=na.omit(pitchers_all_season %>%
                                          select(-c(G,GDP,GS,SF,Pit,R,L,H,SV,SO.W,PO,LD,CS,SO9,
                                                    Str,SB,HR,StL,PU,IP,uBB_perc,uBB,SO_uBB,AB,X1B,
                                                    X2B,HBP,X3B,IBB,Age,bbref_id,Name,Level,Team,season,
                                                    BF,StS,GB.FB,ERA,BB,WHIP,BAbip)))))
teams %>% View()



#https://www.mlb.com/news/predictive-pitching-stats-key-to-draftkings-success/c-77392572
#add K/9 rate
#add flyball rate
#pitchers_all_season %>% ggplot(aes(x=WHIP)) +geom_histogram()

# Coefficients:
#   Estimate Std. Error t value        Pr(>|t|)
# (Intercept)  1.028568   0.456638   2.252        0.025050 *
#   ER          -0.143168   0.020384  -7.024 0.0000000000158 ***
#   BB           0.036676   0.016888   2.172        0.030704 *
#   SO           0.029404   0.013283   2.214        0.027646 *
#   ERA          0.187691   0.052235   3.593        0.000384 ***
#   BF           0.017836   0.004266   4.181 0.0000386037168 ***
#   StS          3.439512   1.810991   1.899        0.058539 .
# GB.FB       -0.749705   0.427970  -1.752        0.080885 .
# WHIP        -0.944003   0.352165  -2.681        0.007776 **
#   BAbip        2.520369   1.188907   2.120        0.034875 *
#   SO_perc     -3.480705   1.338725  -2.600        0.009806 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.7098 on 286 degrees of freedom
# Multiple R-squared:  0.5188,	Adjusted R-squared:  0.502
# F-statistic: 30.84 on 10 and 286 DF,  p-value: < 0.00000000000000022

# Call:
#   lm(formula = as.numeric(W) ~ ., data = na.omit(pitchers_all_season %>%
#                                                    select(-c(G, GDP, GS, SF, Pit, R, L, H, SV, SO.W, PO, LD,
#                                                              CS, SO9, Str, SB, HR, StL, PU, IP, uBB_perc, uBB, SO_uBB,
#                                                              AB, X1B, X2B, HBP, X3B, IBB, Age, bbref_id, Name, Level,
#                                                              Team, season, StS, GB.FB, BAbip, BB, WHIP, SO_perc))))
#
# Residuals:
#   Min      1Q  Median      3Q     Max
# -2.0415 -0.4661 -0.1479  0.4339  2.3981
#
# Coefficients:
#   Estimate Std. Error t value         Pr(>|t|)
# (Intercept)  0.070091   0.166176   0.422         0.673492
# ER          -0.127442   0.018870  -6.754 0.00000000007793 ***
#   SO           0.020008   0.006915   2.894         0.004096 **
#   ERA          0.119050   0.035691   3.336         0.000961 ***
#   BF           0.021287   0.002841   7.492 0.00000000000081 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.7189 on 292 degrees of freedom
# Multiple R-squared:  0.4961,	Adjusted R-squared:  0.4892
# F-statistic: 71.88 on 4 and 292 DF,  p-value: < 0.00000000000000022


#ggplot(data=pitchers_all_season,aes(group=W,x=SO9))+geom_boxplot()
#na.omit(pitchers %>% select(-c(bbref_id,Name,Team,season)))

# pitchers = baseballr::bref_daily_pitcher(Sys.Date()-30,Sys.Date())
# pitchers_all_season = baseballr::bref_daily_pitcher("2023-04-01",Sys.Date())

# %>%
#   mutate(Date2 = as.Date(parse_date_time(gsub("\\(1\\)|\\(2\\)", "",Date), "A, b d",locale="English")),
#          doubleheader = if_else( str_detect(Date,"\\(") ,1,0))
