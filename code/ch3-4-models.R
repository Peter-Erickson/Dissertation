### read in data frame

ch4_airings_reg_df <- readRDS("ch4regfile.rds")


## In this file, I prepare a ton of Regression Models for Chapter 4.  They are done in different sets.##

## The first two sets are logistic regression models, and the second sets are for OLS regression models.## 

# Set 1
###Logit Regression models with Election Cycle - Campaign Season FEs; include controls for percent vet; election type; party, and can-vet.  
log1_milimg <- glm(mil_img ~ percent_vet + party + can_vet + Elec_Cycle_Camp, 
                   family=binomial(link="logit"),
                   data=ch4_airings_reg_df)
log1_actdty_img <- glm(act_dty_img ~ percent_vet + party + can_vet + Elec_Cycle_Camp, 
                       family=binomial(link="logit"),
                       data=ch4_airings_reg_df)
log1_vet_img <- glm(vet_img ~ percent_vet + party + can_vet + Elec_Cycle_Camp, 
                    family=binomial(link="logit"),
                    data=ch4_airings_reg_df)
log1_cmbt_img <- glm(us_cmbt_img ~ percent_vet + party + can_vet + Elec_Cycle_Camp, 
                     family=binomial(link="logit"),
                     data=ch4_airings_reg_df)
log1_milhard_img <- glm(us_mil_hard_img ~ percent_vet + party + can_vet + Elec_Cycle_Camp, 
                        family=binomial(link="logit"),
                        data=ch4_airings_reg_df)
log1_appear <- glm(appearance ~ percent_vet + party + can_vet + Elec_Cycle_Camp, 
                   family=binomial(link="logit"),
                   data=ch4_airings_reg_df)
log1_part_act <- glm(partisan_act ~ percent_vet + party + can_vet + Elec_Cycle_Camp, 
                     family=binomial(link="logit"),
                     data=ch4_airings_reg_df)
log1_endorse <- glm(vet_fam_end ~ percent_vet + party + can_vet + Elec_Cycle_Camp, 
                    family=binomial(link="logit"),
                    data=ch4_airings_reg_df)
log1_attack <- glm(vet_fam_atk ~ percent_vet + party + can_vet + Elec_Cycle_Camp, 
                   family=binomial(link="logit"),
                   data=ch4_airings_reg_df)
log1_nonint <- glm(vet_fam_out_issues ~ percent_vet + party + can_vet + Elec_Cycle_Camp, 
                   family=binomial(link="logit"),
                   data=ch4_airings_reg_df)

###save RDS files for Set 1###
saveRDS(log1_milimg, "ch4_log1_milimg.rds")
saveRDS(log1_actdty_img, "ch4_log1_actdty_img.rds")
saveRDS(log1_vet_img, "ch4_log1_vet_img.rds")
saveRDS(log1_cmbt_img, "ch4_log1_cmbt_img.rds")
saveRDS(log1_milhard_img, "ch4_log1_milhard_img.rds")
saveRDS(log1_appear, "ch4_log1_appear.rds")
saveRDS(log1_part_act, "ch4_log1_part_act.rds")
saveRDS(log1_endorse, "ch4_log1_endorse.rds")
saveRDS(log1_attack, "ch4_log1_attack.rds")
saveRDS(log1_nonint, "ch4_log1_nonint.rds")




# Set 2 - Replace Airing Yr Fixed Effects With Polarization, and Hostile Cas Rrate.
log2_milimg <- glm(mil_img ~ percent_vet + party + can_vet + sen_polar + HostCasRateLagged, 
                   family=binomial(link="logit"),
                   data=ch4_airings_reg_df)
log2_actdty_img <- glm(act_dty_img ~ percent_vet + party + can_vet + sen_polar + HostCasRateLagged, 
                       family=binomial(link="logit"),
                       data=ch4_airings_reg_df)
log2_vet_img <- glm(vet_img ~ percent_vet + party + can_vet + sen_polar + HostCasRateLagged, 
                    family=binomial(link="logit"),
                    data=ch4_airings_reg_df)
log2_cmbt_img <- glm(us_cmbt_img ~ percent_vet + party + can_vet + sen_polar + HostCasRateLagged, 
                     family=binomial(link="logit"),
                     data=ch4_airings_reg_df)
log2_milhard_img <- glm(us_mil_hard_img ~ percent_vet + party + can_vet + sen_polar + HostCasRateLagged,
                        family=binomial(link="logit"),
                        data=ch4_airings_reg_df)
log2_appear <- glm(appearance ~ percent_vet + party + can_vet + sen_polar + HostCasRateLagged, 
                   family=binomial(link="logit"),
                   data=ch4_airings_reg_df)
log2_part_act <- glm(partisan_act ~ percent_vet + party + can_vet + sen_polar + HostCasRateLagged, 
                     family=binomial(link="logit"),
                     data=ch4_airings_reg_df)
log2_endorse <- glm(vet_fam_end ~ percent_vet + party + can_vet + sen_polar + HostCasRateLagged, 
                    family=binomial(link="logit"),
                    data=ch4_airings_reg_df)
log2_attack <- glm(vet_fam_atk ~ percent_vet + party + can_vet + sen_polar + HostCasRateLagged, 
                   family=binomial(link="logit"),
                   data=ch4_airings_reg_df)
log2_nonint <- glm(vet_fam_out_issues ~ percent_vet + party + can_vet + sen_polar + HostCasRateLagged, 
                   family=binomial(link="logit"),
                   data=ch4_airings_reg_df)

###save RDS files for Set 2###
saveRDS(log2_milimg, "ch4_log2_milimg.rds")
saveRDS(log2_actdty_img, "ch4_log2_actdty_img.rds")
saveRDS(log2_vet_img, "ch4_log2_vet_img.rds")
saveRDS(log2_cmbt_img, "ch4_log2_cmbt_img.rds")
saveRDS(log2_milhard_img, "ch4_log2_milhard_img.rds")
saveRDS(log2_appear, "ch4_log2_appear.rds")
saveRDS(log2_part_act, "ch4_log2_part_act.rds")
saveRDS(log2_endorse, "ch4_log2_endorse.rds")
saveRDS(log2_attack, "ch4_log2_attack.rds")
saveRDS(log2_nonint, "ch4_log2_nonint.rds")


### Set 3: OLS Regression Prep
ch4_airings_reg_df_mkt_yr <- ch4_airings_reg_df %>% group_by(Elec_Cycle_Camp, media_market, percent_vet) %>% summarise(
  mean_mil_img = mean(mil_img),
  mean_act_dty_img = mean(act_dty_img),
  mean_vet_img = mean(vet_img),
  mean_hard_img = mean(us_mil_hard_img),
  mean_cmbt_img = mean(us_cmbt_img),
  mean_appearance = mean(appearance),
  mean_part_act = mean(partisan_act),
  mean_endorse = mean(vet_fam_end),
  mean_attack = mean(vet_fam_atk),
  mean_out_issues = mean(vet_fam_out_issues)
) %>% mutate(
  house_polar = case_when(
    Elec_Cycle_Camp== "2000PRI" ~ 77.713,
    Elec_Cycle_Camp== "2000GEN" ~ 77.713,
    Elec_Cycle_Camp== "2004PRI" ~ 79.302,
    Elec_Cycle_Camp== "2004GEN" ~ 79.302,
    Elec_Cycle_Camp== "2008PRI" ~ 80.765,
    Elec_Cycle_Camp== "2008GEN" ~ 80.765,
    Elec_Cycle_Camp== "2012PRI" ~ 86.142,
    Elec_Cycle_Camp== "2012GEN" ~ 86.142,
    Elec_Cycle_Camp== "2016PRI" ~ 87.639,
    Elec_Cycle_Camp== "2016GEN" ~ 87.639,
  ),
  sen_polar = case_when(
    Elec_Cycle_Camp== "2000PRI" ~ 65.833,
    Elec_Cycle_Camp== "2000GEN" ~ 65.833,
    Elec_Cycle_Camp== "2004PRI" ~ 64.984,
    Elec_Cycle_Camp== "2004GEN" ~ 64.984,
    Elec_Cycle_Camp== "2008PRI" ~ 68.736,
    Elec_Cycle_Camp== "2008GEN" ~ 68.736,
    Elec_Cycle_Camp== "2012PRI" ~ 74.383,
    Elec_Cycle_Camp== "2012GEN" ~ 74.383,
    Elec_Cycle_Camp== "2016PRI" ~ 81.293,
    Elec_Cycle_Camp== "2016GEN" ~ 81.293,
  ),
  aff_polar = case_when(
    Elec_Cycle_Camp== "2000PRI" ~ 61.692,
    Elec_Cycle_Camp== "2000GEN" ~ 61.692,
    Elec_Cycle_Camp== "2004PRI" ~ 69.27,
    Elec_Cycle_Camp== "2004GEN" ~ 69.27,
    Elec_Cycle_Camp== "2008PRI" ~ 69.519,
    Elec_Cycle_Camp== "2008GEN" ~ 69.519,
    Elec_Cycle_Camp== "2012PRI" ~ 84.689,
    Elec_Cycle_Camp== "2012GEN" ~ 84.689,
    Elec_Cycle_Camp== "2016PRI" ~ 67.754,
    Elec_Cycle_Camp== "2016GEN" ~ 67.754,
  ),
  HostCasRate = case_when(
    Elec_Cycle_Camp== "2000PRI" ~ 1.23874924,
    Elec_Cycle_Camp== "2000GEN" ~ 1.23874924,
    Elec_Cycle_Camp== "2004PRI" ~ 21.92014883,
    Elec_Cycle_Camp== "2004GEN" ~ 52.0801226,
    Elec_Cycle_Camp== "2008PRI" ~ 61.97806503,
    Elec_Cycle_Camp== "2008GEN" ~ 25.1029256,
    Elec_Cycle_Camp== "2012PRI" ~ 31.31,
    Elec_Cycle_Camp== "2012GEN" ~ 21.4917871,
    Elec_Cycle_Camp== "2016PRI" ~ 1.750,
    Elec_Cycle_Camp== "2016GEN" ~ 1.229532,
  ),
  HostCasRateLagged = case_when(
    Elec_Cycle_Camp== "2000PRI" ~ 0,
    Elec_Cycle_Camp== "2000GEN" ~ 0,
    Elec_Cycle_Camp== "2004PRI" ~ 1.20,
    Elec_Cycle_Camp== "2004GEN" ~ 21.92014883,
    Elec_Cycle_Camp== "2008PRI" ~ 56.07,
    Elec_Cycle_Camp== "2008GEN" ~ 61.97806503,
    Elec_Cycle_Camp== "2012PRI" ~ 31.866,
    Elec_Cycle_Camp== "2012GEN" ~ 31.31,
    Elec_Cycle_Camp== "2016PRI" ~ 4.034,
    Elec_Cycle_Camp== "2016GEN" ~ 1.750,
  ),
  mil_prestige = case_when(
    Elec_Cycle_Camp== "2000PRI" ~ 64,
    Elec_Cycle_Camp== "2000GEN" ~ 64,
    Elec_Cycle_Camp== "2004PRI" ~ 82,
    Elec_Cycle_Camp== "2004GEN" ~ 75,
    Elec_Cycle_Camp== "2008PRI" ~ 69,
    Elec_Cycle_Camp== "2008GEN" ~ 71,
    Elec_Cycle_Camp== "2012PRI" ~ 78,
    Elec_Cycle_Camp== "2012GEN" ~ 75,
    Elec_Cycle_Camp== "2016PRI" ~ 72,
    Elec_Cycle_Camp== "2016GEN" ~ 73,
  ), 
  cong_prestige = case_when(
    Elec_Cycle_Camp== "2000PRI" ~ 24,
    Elec_Cycle_Camp== "2000GEN" ~ 24,
    Elec_Cycle_Camp== "2004PRI" ~ 29,
    Elec_Cycle_Camp== "2004GEN" ~ 30,
    Elec_Cycle_Camp== "2008PRI" ~ 14,
    Elec_Cycle_Camp== "2008GEN" ~ 12,
    Elec_Cycle_Camp== "2012PRI" ~ 12,
    Elec_Cycle_Camp== "2012GEN" ~ 13,
    Elec_Cycle_Camp== "2016PRI" ~ 8,
    Elec_Cycle_Camp== "2016GEN" ~ 9,
  ),
  prestige_gap = mil_prestige-cong_prestige,
  Elec_Cycle_Camp = as.factor(Elec_Cycle_Camp))

### Save OLS Regression File ###
saveRDS(ch4_airings_reg_df_mkt_yr, "ch4_OLS_data.rds")

##############Predicted Probability##########

In order to gauge the substantive importance of the level of trust in the military and how it impacts the likelihood that campaign advertisements feature military figures who in some way violate the principles of civil-military relations when they appear in these advertisements, we can transform the logistic regression results into predicted probabilities. Using results from Table \@ref(tab:ch4regression2), Figure \@ref(fig:pred-prob-ch4-sen) plots the predicted probability of a presidential campaign television ad featuring a military figure who engages in any behavior that undermines the principles of civil-military relations as the level of military prestige changes. I fix the level of polarization in the Senate and the lagged hostile troop casualty rate at their respective means over the period from 2000 - 2016. I hold the value of candidate-veteran at 0, denoting that the candidates portrayed in a hypothetical ad are not veterans, and hold the value of the party that the advertisement supports as the Republican Party. The vertical colored lines indicate the level of military prestige as measured by public opinion polling in certain points of time in contemporary US history [@noauthor_confidence_2020]. The color of the line indicates the party of the President who was in office at the time. 



I then conduct a similar graph, but alter a few of the variables. In particular, I use the average value of polarization in the House and change the status of the hypothetical candidates appearing in an advertisement to veterans.  I also change the party whom the hypothetical ad supports to Democrat instead of Republican. This graph is portrayed in Figure \@ref(fig:pred-prob-ch4-house).

```{r pred-prob-ch4-house, include=TRUE, echo=FALSE, warning=FALSE, message=FALSE, fig.cap="Predicted Probability of Military Figures Violating Central Principles of Civil-Military Relations in Presidential Campaign TV Ads", fig.height=4, fig.width=6}
predictions_ch4_house%>%
  ggplot() +
  aes(x = mil_prestige, y = .prob) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_smooth() + labs(
    x="Level of Military Prestige",
    y="Predicted Probability", caption="House Polarization Held Constant\nVeteran Candidate(s) Portrayed\nAd Supports Democratic Party") + 
  scale_y_continuous(breaks=seq(-.1, .5, .1)) + 
  geom_vline(xintercept=85, size=.3, colour="tomato") + 
  geom_vline(xintercept=64, size=.2, colour="dodgerblue") + 
  geom_vline(xintercept=82, size=.2, colour="tomato") +
  geom_vline(xintercept=58, size=.2, colour="dodgerblue") +
  geom_vline(xintercept=69, size=.2, colour="dodgerblue") +
  geom_text(aes(x=85, label="Feb 1991",y=.3), colour="tomato", angle=90, vjust=1, text=element_text(size=9)) + 
  geom_text(aes(x=64, label="1994",y=.2), colour="dodgerblue", angle=90, vjust=1, text=element_text(size=9)) + 
  geom_text(aes(x=82, label="2003",y=.3), colour="tomato", angle=90, vjust=-.2, text=element_text(size=9)) + 
  geom_text(aes(x=58, label="1975",y=.2), colour="dodgerblue", angle=90, vjust=-.2, text=element_text(size=9))  + 
  geom_text(aes(x=69, label="2021", y=.2), colour="dodgerblue", angle=90, vjust=1, text=element_text(size=9))
```

### Multiple Imputation Steps from a Previous Chapter ###


However, given that the logistic regression models portrayed a different result, namely, that the level of trust in the military does seem to shape the likelihood that an advertisement features a military figure who violates one of the principles of civil-military relations, I conduct multiple imputation for the missing data. 

The results are displayed in Table \@ref(tab:ch4mice).
```{r ch4mice-prep, include=FALSE, echo=FALSE, warning=F, message=F, error=FALSE}

## use multiple imputation to handle missing data
df_campaign_ads_raw_mice

# create a variable for candidate name
df_campaign_ads_raw_mice$candidate <- factor(sub("/.*$", "", df_campaign_ads_raw_mice$creative)) %>% print()

df_campaign_ads_raw_mice$party_reg <- factor(df_campaign_ads_raw_mice$party_reg, levels=c("DEMOCRAT","OTHER"))
table(df_campaign_ads_raw_mice$party_reg)

df_campaign_ads_raw_mice$prom_mil_fig <- factor(df_campaign_ads_raw_mice$prom_mil_fig, levels=setdiff(unique(df_campaign_ads_raw_mice$prom_mil_fig), c("")))

df_campaign_ads_raw_mice_ready <- (df_campaign_ads_raw_mice[,!sapply(df_campaign_ads_raw_mice, is.character)])

# goes through variables except count and l, made them factors, and changed 99 to 0
for (varname in setdiff(names(df_campaign_ads_raw_mice_ready), c("count", "l", "house_polar", "sen_polar", "aff_polar", "HostCasRateLagged", "mil_prestige", "prestige_gap", "party_reg" ))) {
  if (length(setdiff(unique(df_campaign_ads_raw_mice_ready[[varname]]), 99)) > 2)
    df_campaign_ads_raw_mice_ready[[varname]] <- factor(ifelse(df_campaign_ads_raw_mice_ready[[varname]]==99,0,df_campaign_ads_raw_mice_ready[[varname]]), levels=sort(setdiff(unique(df_campaign_ads_raw_mice_ready[[varname]]), 99)))
  else
    df_campaign_ads_raw_mice_ready[[varname]] <- as.integer(ifelse(df_campaign_ads_raw_mice_ready[[varname]]==99,0,df_campaign_ads_raw_mice_ready[[varname]]))
}

df_campaign_ads_raw_mice_ready$party_reg %>% print()

## conduct multiple  imputation
#make a prediction matrix first

prediction_matrix <- make.predictorMatrix(as.matrix(df_campaign_ads_raw_mice_ready), blocks = make.blocks( c("act_dty_img", "vet_img","us_mil_hard_img", "us_cmbt_img", "vet_fam", "vet_fam_end", "vet_fam_atk", "vet_fam_out_issues", "partisan_act"), partition="scatter"))

prediction_matrix[, "candidate"] <- -2

# change the variable candidate to a type of integer
df_campaign_ads_raw_mice_ready$candidate <- as.integer(df_campaign_ads_raw_mice_ready$candidate)

# put in variables you don't want to use for the imputation
#prediction_matrix[, c("")] <- 0

df_campaign_ads_raw_mice_imputed <- mice(data=df_campaign_ads_raw_mice_ready, method="2l.bin", predictorMatrix=prediction_matrix, maxit=10, m=20, blocks=as.list(rownames(prediction_matrix)))

# for MICE regressions using imputed data


mod_appearance_imp <- with(df_campaign_ads_raw_mice_imputed, glm(vet_fam ~ mil_prestige + house_polar + HostCasRateLagged + can_vet + party_reg , family=binomial(link="logit")))
imp_appearance <- summary(pool(mod_appearance_imp))

mod_anyviol_ch4_imp <- with(df_campaign_ads_raw_mice_imputed, glm(partisan_act ~ mil_prestige + sen_polar + HostCasRateLagged + can_vet + party_reg, family=binomial(link="logit")))
imp_anyviol <- summary(pool(mod_anyviol_ch4_imp))

mod_endorse_ch4_imp <- with(df_campaign_ads_raw_mice_imputed, glm(vet_fam_end ~ mil_prestige + house_polar + HostCasRateLagged + can_vet + party_reg, family=binomial(link="logit")))
imp_endorse <- summary(pool(mod_endorse_ch4_imp))

mod_atk_ch4_imp <- with(df_campaign_ads_raw_mice_imputed, glm(vet_fam_atk ~ mil_prestige + sen_polar + HostCasRateLagged + can_vet + party_reg, family=binomial(link="logit")))
imp_atk <- summary(pool(mod_atk_ch4_imp)) %>% print()

mod_int_ch4_imp <- with(df_campaign_ads_raw_mice_imputed, glm(vet_fam_out_issues ~ mil_prestige + house_polar + HostCasRateLagged + can_vet + party_reg, family=binomial(link="logit")))
imp_int <- summary(pool(mod_int_ch4_imp)) %>% print()

### Extra here ###

## One way is to use kable.  Not ideal, but perfectly acceptable. 
imp_int_df <- tibble(imp_int)

kable(imp_int_df, booktabs=T, caption="Multiple Imputation, DV: Log Odds of Addressing Outside Topic") %>%
  kable_styling(full_width = F, font_size=10.5, latex_options="striped") %>% 
  column_spec(1:2, width="5em") %>% 
  column_spec(3:6, width="5.5em")

```

```{r ch4mice, include=T, echo=FALSE, error=FALSE, warning=FALSE, message=FALSE}

models_imputed <- list(
  "Appear" = mod_appearance_imp,
  "Any Violation" = mod_anyviol_ch4_imp,
  "Endorse" = mod_endorse_ch4_imp, 
  "Attack" = mod_atk_ch4_imp,
  "Outside Topic" = mod_int_ch4_imp
)

cm <-c("mil_prestige"="Trust in Military", "house_polar" ="House Polarization", "sen_polar"="Senate Polarization", "HostCasRateLagged"="Casualty Rate (lagged)","can_vet"="Candidate-Veteran","party_regOTHER"="Party-Other", "(Intercept)"="Constant")

modelsummary(models_imputed, stars=c("*"=.1, "**"=.05, "***"=.01), coef_map = cm, format="latex", title = '(Multiple Imputation) Log Odds of a Presidential Campaign TV Ad Featuring Prominent Military Actors Engaging in Political Behaviors', notes='Here are some notes.')
```
