Data Cleaning
================
Daniela Quigee (dq2147)
12/03/2019

``` r
prop_NA_original_data = function(df) {

  df_return = df %>% select(year, everything()) %>% drop_na(year)
  
  variable_names = names(df_return)
  
  for (i in 2:length(variable_names))
  {
    df_return[[i]] = is.na(df_return[[i]])
  }
  
  df_return = df_return %>% 
    pivot_longer(
      -year,
      names_to = "values",
      values_to = "not_available") %>% 
    group_by(year, values) %>% 
    summarize(
      summeNA = sum(not_available),
      summeNotNA = sum(!not_available)
    ) %>% 
    mutate(
      proportionNA = summeNA / (summeNA + summeNotNA ),
      proportionNA = round(proportionNA, digits = 5)
    ) 
  
  df_return
}
```

``` r
df_Bronx = read_excel(path = "./raw_data/Bronx.xlsx", col_types = "text")
df_Queens = read_excel(path = "./raw_data/Queens.xlsx", col_types = "text")
df_Manhattan = read_excel(path = "./raw_data/Manhattan.xlsx", col_types = "text")
df_StatenIsland = read_excel(path = "./raw_data/Staten_Island.xlsx", col_types = "text")
df_Brooklyn = read_excel(path = "./raw_data/Brooklyn.xlsx", col_types = "text")
```

``` r
data_cleaning = function(df) {
df = df %>% 
    # Removing duplicate rows
    distinct() %>% 
    # Deleting unnecessary variables
    select(-sitecode, -sitetype, -sitetypenum,
           -survyear, -weight, -stratum, -PSU,
           -record, -qnobese, -qnowt, -sexid2, -sexpart2,
           -qnwater, -qnwater1, -qnwater2, -qnwater3,
           -qngenderexp, -qntypealcohol, -qnpropertydamage,
           -qndlype, -qnpa7day, -qnpa0day, -qnbk7day, -qnmilk3,
           -qnmilk2, -qnmilk1, -qnsoda3, -qnsoda2, -qnsoda1, 
           -qnveg3, -qnveg2, -qnveg1, -qnveg0, -qnfr3,
           -qnfr2, -qnfr1, -qnfr0, -qnowt, -qnobese, -qnbcnone,
           -qndualbc, -qnothhpl, -qnshparg, -qniudimp, -qntb2, 
           -qntb3, -qntb4) %>% 
     # Deleting unnecessary variables Part II
     select(-qndaycgr, -qnfrcgr, -qndayskl, -qnfrskl, -qndayevp, -qnfrevp, -qndaycig, 
            -qnfrcig, -qnnodnt, -qnbikehelmet, -qncigschool, -qnchewtobschool, -qnalcoholschool,
            -qnmarijuanaschool, -qncurrentcocaine, -qntaughtHIV, -qnmusclestrength, -qnsunscreenuse,
            -qnindoortanning, -qntransgender, -qn89, -qn88, -qn87, -qn86, -qn85, -qn83,
            -qn82, -qn81, -qn80, -qn79, -qn78, -qn77, -qn76, -qn75, -qn74,-qn73, -qn72, -qn71,
            -qn70, -qn69, -qn68, -race4, -stheight, -stweight, -bmi, -bmipct) %>% 
     # No data
     select(
       -qn20, -qn31, -qn34, -qn36, -qn39, -qn56, -qn84,
       -qnspeakenglish, -qnwheresleep, -qncurrentasthma, 
       -qnconcentrating, -qnsunburn, -qnwenthungry, 
       -qnfoodallergy, -qnfastfood, -qnsugardrink, 
       -qnenergydrink, -qnspdrk1, -qnspdrk2, -qnspdrk3, 
       -qnsportsdrink, -qncoffeetea, -qndietpop, -qntaughtbc,
       -qntaughtcondom, -qntaughtstd, -qntaughtsexed, 
       -qnprescription30d, -qnhallucdrug, -qncurrentmeth, 
       -qncurrentheroin, -qnhowmarijuana, -qnchokeself, 
       -qnbullygay, -qnbullygender, -qnbullyweight, 
       -qncelldriving, -qndrivemarijuana) %>%
    rename(
      borough = sitename) %>%
    mutate(
      borough = recode(borough,
                       "Borough of Bronx (NYG)" = "Bronx",
                       "Borough of Queens (NYJ)" = "Queens",
                       "Borough of Manhattan (NYI)" = "Manhattan",
                       "Borough of Staten Island (NYK)" = "Staten Island",
                       "Borough of Brooklyn (NYH)" = "Brooklyn"),
      borough = as.factor(borough)) %>%
    mutate(
      year = as.numeric(year)) %>%
    mutate(
      age = recode(age,
                   "1" = "12 years old or younger", 
                   "2" = "13 years old",
                   "3" = "14 years old",
                   "4" = "15 years old",
                   "5" = "16 years old",
                   "6" = "17 years old",
                   "7" = "18 years old or older"),
      age = as.factor(age)) %>%
    mutate(
      sex = recode(sex,
                   "1" = "female", 
                   "2" = "male"),
      sex = as.factor(sex)) %>%
    mutate(
      grade = recode(grade,
                 "1" = "9th grade", 
                 "2" = "10th grade", 
                 "3" = "11th grade", 
                 "4" = "12th grade", 
                 "5" = "ungraded or other grade"),
      grade = as.factor(grade)) %>%
    mutate(
      race7 = recode(race7,
                     "1" = "American Indian/Alaska Native", 
                     "2" = "Asian", 
                     "3" = "Black or African American", 
                     "4" = "Hispanic/Latino", 
                     "5" = "Native Hawaiian/Other Pacific Islander", 
                     "6" = "White",
                     "7" = "Multiple Races (Non-Hispanic)"),
      race7 = as.factor(race7)) %>%
    # Sexual identity
    mutate(
      q67 = recode(q67,
                   "1" = "heterosexual", 
                   "2" = "gay/lesbian", 
                   "3" = "bisexual", 
                   "4" = "not sure"),
      q67 = as.factor(q67)) %>%
    rename(sexual_identity = q67) %>%
    # Sexual identity 2
    mutate(
      sexid = recode(sexid, 
                     "1" = "heterosexual", 
                     "2" = "gay/lesbian", 
                     "3" = "bisexual", 
                     "4" = "not sure"),
      sexid = as.factor(sexid)) %>%
    rename(sexual_identity_2 = sexid) %>%
    # Sexual Contact
    mutate(
      q66 = recode(q66,
                   "1" = "no sexual contact", 
                   "2" = "females", 
                   "3" = "males", 
                   "4" = "females and males"),
      q66 = as.factor(q66)) %>% 
    rename(sexual_contact = q66) %>% 
    # Sexual Contact 2
    mutate(
      sexpart = recode(sexpart,
                       "1" = "never had sex", 
                       "2" = "opposite sex only", 
                       "3" = "same sex only", 
                       "4" = "both sexes"),
      sexpart = as.factor(sexpart)) %>% 
    rename(sexual_contact_2 = sexpart) %>% 
#####################################################################################
    # How often do you wear a seat belt when riding in a car driven by someone else?
    mutate(
      qn8 = recode(qn8,
                   "1" = "never/rarely",
                   "2" = "sometimes or more"),
      qn8 = as.factor(qn8)) %>% 
    rename(seatbelt_use = qn8) %>% 
##################################################################################### 
    # During the past 30 days, how many times did you ride in a car or other vehicle driven by someone who had been drinking alcohol?
    mutate(
      qn9 = recode(qn9,
                   "1" = "Yes",
                   "2" = "No"),
      qn9 = as.factor(qn9)) %>% 
    rename(riding_with_drinking_driver = qn9) %>% 
##################################################################################### 
    # During the past 30 days, how many times did you drive a car or other vehicle when you had been drinking alcohol?
    mutate(
      qn10 = recode(qn10,
                    "1" = "Yes",
                    "2" = "No/did not drive"),
      qn10 = as.factor(qn10),
      qn10 = fct_relevel(qn10, c("Yes", "No/did not drive"))) %>% 
    rename(drinking_and_driving = qn10) %>% 
##################################################################################### 
    # During the past 30 days, on how many days did you text or e-mail while driving a car or other vehicle?
    mutate(
      qn11 = recode(qn11,
                 "1" = "Yes",
                 "2" = "No/did not drive"),
      qn11 = as.factor(qn11),
      qn11 = fct_relevel(qn11, c("Yes", "No/did not drive"))) %>% 
    rename(texting_and_driving = qn11) %>% 
##################################################################################### 
    # During the past 30 days, on how many days did you carry a weapon such as a gun, knife, or club?
    mutate(
      qn12 = recode(qn12,
                    "1" = "Yes",
                    "2" = "No"),
      qn12 = as.factor(qn12),
      qn12 = fct_relevel(qn12, c("Yes", "No"))) %>% 
    rename(carring_weapon = qn12) %>%
##################################################################################### 
    # During the past 30 days, on how many days did you carry a weapon such as a gun, knife, or club on school property?
    mutate(
      qn13 = recode(qn13,
                    "1" = "Yes",
                    "2" = "No"),
      qn13 = as.factor(qn13)) %>% 
    rename(carring_weapon_school = qn13) %>% 
##################################################################################### 
    # During the past 12 months, on how many days did you carry a gun? (Do not count the days when you carried a gun only for hunting or for a sport, such as target shooting.)
    mutate(
      qn14 = recode(qn14,
                    "1" = "Yes",
                    "2" = "No"),
      qn14 = as.factor(qn14)) %>% 
    rename(gun_carrying_past_12_month = qn14) %>%
##################################################################################### 
    # During the past 30 days, on how many days did you not go to school because you felt you would be unsafe at school or on your way to or from school?
    mutate(
      qn15 = recode(qn15,
                    "1" = "Yes",
                    "2" = "No"),
      qn15 = as.factor(qn15),
      qn15 = fct_relevel(qn15, c("Yes", "No"))) %>% 
    rename(safety_concerns_at_school = qn15) %>%
##################################################################################### 
    # During the past 12 months, how many times has someone threatened or injured you with a weapon such as a gun, knife, or club on school property?
    mutate(
      qn16 = recode(qn16,
                    "1" = "Yes",
                    "2" = "No"),
      qn16 = as.factor(qn16),
      qn16 = fct_relevel(qn16, c("Yes", "No"))) %>% 
    rename(threatened_at_school = qn16) %>%
##################################################################################### 
    # During the past 12 months, how many times were you in a physical fight?
    mutate(
      qn17 = recode(qn17,
                    "1" = "Yes",
                    "2" = "No"),
      qn17 = as.factor(qn17),
      qn17 = fct_relevel(qn17, c("Yes", "No"))) %>% 
    rename(physical_fighting = qn17) %>%
##################################################################################### 
    # During the past 12 months, how many times were you in a physical fight on school property?
    mutate(
      qn18 = recode(qn18,
                    "1" = "Yes",
                    "2" = "No"),
      qn18 = as.factor(qn18)) %>% 
    rename(physical_fighting_at_school = qn18) %>%
##################################################################################### 
    # Have you ever been physically forced to have sexual intercourse when you did not want to?
    mutate(
      qn19 = recode(qn19,
                 "1" = "Yes",
                 "2" = "No"),
      qn19 = as.factor(qn19)) %>% 
    rename(forced_sexual_intercourse = qn19) %>%
##################################################################################### 
    # During the past 12 months, how many times did someone you were dating or going out with force you to do sexual things that you did not want to do? (Count such things as kissing, touching, or being physically forced to have sexual intercourse.)
    mutate(
      qn21 = recode(qn21,
                    "1" = "1 or more times",
                    "2" = "did not date/0 times"),
      qn21 = as.factor(qn21)) %>% 
    rename(sexual_dating_violence = qn21) %>%
##################################################################################### 
    # During the past 12 months, how many times did someone you were dating or going out with physically hurt you on purpose? (Count such things as being hit, slammed into something, or injured with an object or weapon.)
    mutate(
      qn22 = recode(qn22,
                    "1" = "1 or more times",
                    "2" = "did not date/0 times"),
      qn22 = as.factor(qn22)) %>% 
    rename(physical_dating_violence = qn22) %>%
##################################################################################### 
    # During the past 12 months, have you ever been bullied on school property?
    mutate(
      qn23 = recode(qn23,
                 "1" = "Yes",
                 "2" = "No"),
      qn23 = as.factor(qn23),
      qn23 = fct_relevel(qn23, c("Yes", "No"))) %>% 
    rename(bullying_at_school = qn23) %>%
##################################################################################### 
    # During the past 12 months, have you ever been electronically bullied? (Count being bullied through texting, Instagram, Facebook, or other social media.)
    mutate(
      qn24 = recode(qn24,
                    "1" = "Yes",
                    "2" = "No"),
      qn24 = as.factor(qn24),
      qn24 = fct_relevel(qn24, c("Yes", "No"))) %>% 
    rename(bullying_electronically = qn24) %>%
##################################################################################### 
    # During the past 12 months, did you ever feel so sad or hopeless almost every day for two weeks or more in a row that you stopped doing some usual activities?
    mutate(
      qn25 = recode(qn25,
                    "1" = "Yes",
                    "2" = "No"),
      qn25 = as.factor(qn25),
      qn25 = fct_relevel(qn25, c("Yes", "No"))) %>% 
    rename(sad_hopeless = qn25) %>%
##################################################################################### 
    # During the past 12 months, did you ever seriously consider attempting suicide?
    mutate(
      qn26 = recode(qn26, 
                    "1" = "Yes",
                    "2" = "No"),
      qn26 = as.factor(qn26),
      qn26 = fct_relevel(qn26, c("Yes", "No"))) %>% 
    rename(considered_suicide = qn26) %>% 
##################################################################################### 
    # During the past 12 months, did you make a plan about how you would attempt suicide?
    mutate(
      qn27 = recode(qn27,
                    "1" = "Yes",
                    "2" = "No"),
      qn27 = as.factor(qn27),
      qn27 = fct_relevel(qn27, c("Yes", "No"))) %>% 
    rename(made_suicide_plan = qn27) %>% 
##################################################################################### 
    # During the past 12 months, how many times did you actually attempt suicide?
    mutate(
      qn28 = recode(qn28,
                    "1" = "Yes",
                    "2" = "No"),
      qn28 = as.factor(qn28),
      qn28 = fct_relevel(qn28, c("Yes", "No"))) %>% 
    rename(attempted_suicide = qn28) %>% 
##################################################################################### 
    # If you attempted suicide during the past 12 months, did any attempt result in an injury, poisoning, or overdose that had to be treated by a doctor or nurse?
    mutate(
      qn29 = recode(qn29,
                    "1" = "Yes",
                    "2" = "No/did not attempt suicide"),
      qn29 = as.factor(qn29),
      qn29 = fct_relevel(qn29, c("Yes", "No/did not attempt suicide"))) %>% 
    rename(injurious_suicide_attempt = qn29) %>%
##################################################################################### 
    # Have you ever tried cigarette smoking, even one or two puffs?
    mutate(
      qn30 = recode(qn30,
                    "1" = "Yes",
                    "2" = "No"),
      qn30 = as.factor(qn30)) %>% 
    rename(ever_cigarette_use = qn30) %>% 
##################################################################################### 
    # During the past 30 days, on how many days did you smoke cigarettes?
    mutate(
      qn32 = recode(qn32,
                    "1" = "Yes",
                    "2" = "No"),
      qn32 = as.factor(qn32),
      qn32 = fct_relevel(qn32, c("Yes", "No"))) %>% 
    rename(current_cigarette_use = qn32) %>% 
##################################################################################### 
    # During the past 30 days, on the days you smoked, how many cigarettes did you smoke per day?
    mutate(
      qn33 = recode(qn33,
                    "1" = "11 or more cigarettes per day",
                    "2" = "no smoking/less than 11 cigarettes"),
      qn33 = as.factor(qn33),
      qn33 = qn33) %>% 
    rename(smoked_greater_10 = qn33) %>%
##################################################################################### 
    # During the past 30 days, on how many days did you use an electronic vapor product?
    mutate(
      qn35 = recode(qn35,
                    "1" = "Yes",
                    "2" = "No"),
      qn35 = as.factor(qn35),
      qn35 = fct_relevel(qn35, c("Yes", "No"))) %>% 
    rename(current_vaping = qn35) %>%
##################################################################################### 
    # During the past 30 days, on how many days did you use chewing tobacco, snuff, dip, snus, or dissolvable tobacco products, such as Redman, Levi Garrett, Beechnut, Skoal, Skoal Bandits, Copenhagen, Camel Snus, Marlboro Snus, General Snus, Ariva, Stonewall, or Camel Orbs? (Do not count any electronic vapor products.)
    mutate(
      qn37 = recode(qn37,
                    "1" = "Yes",
                    "2" = "No"),
      qn37 = as.factor(qn37),
      qn37 = fct_relevel(qn37, c("Yes", "No"))) %>% 
    rename(current_smokeless_tobacco_use = qn37) %>% 
#####################################################################################
    # During the past 30 days, on how many days did you smoke cigars, cigarillos, or little cigars?
    mutate(
      qn38 = recode(qn38,
                    "1" = "Yes",
                    "2" = "No"),
      qn38 = as.factor(qn38),
      qn38 = fct_relevel(qn38, c("Yes", "No"))) %>% 
    rename(current_cigar_use = qn38) %>% 
#####################################################################################
    # During your life, on how many days have you had at least one drink of alcohol?
    mutate(
      qn40 = recode(qn40,
                    "1" = "Yes",
                    "2" = "No"),
      qn40 = as.factor(qn40)) %>% 
    rename(ever_alcohol_use = qn40) %>%
#####################################################################################
    # How old were you when you had your first drink of alcohol other than a few sips?
    mutate(
      qn41 = recode(qn41,
                    "1" = "12 years or younger",
                    "2" = "never had a drink/13 years or older"),
      qn41 = as.factor(qn41)) %>% 
    rename(initiation_alcohol_use = qn41) %>%
#####################################################################################
    # During the past 30 days, on how many days did you have at least one drink of alcohol?
    mutate(
      qn42 = recode(qn42,
                    "1" = "Yes",
                    "2" = "No"),
      qn42 = as.factor(qn42),
      qn42 = fct_relevel(qn42, c("Yes", "No"))) %>% 
    rename(current_alcohol_use = qn42) %>%
#####################################################################################
    # During the past 30 days, how did you usually get the alcohol you drank?
    mutate(
      qn43 = recode(qn43,
                    "1" = "someone gave it to me",
                    "2" = "otherwise"),
      qn43 = as.factor(qn43)) %>% 
    rename(source_alcohol = qn43) %>%
#####################################################################################
    # During the past 30 days, on how many days did you have 4 or more drinks of alcohol in a row (if you are female) or 5 or more drinks of alcohol in a row (if you are male)?
    mutate(
      qn44 = recode(qn44,
                    "1" = "Yes",
                    "2" = "No"),
      qn44 = as.factor(qn44)) %>% 
    rename(currret_binge_drinking = qn44) %>%
#####################################################################################
    # During the past 30 days, what is the largest number of alcoholic drinks you had in a row?
    mutate(
      qn45 = recode(qn45,
                    "1" = "10 or more drinks",
                    "2" = "less than 10 drinks"),
      qn45 = as.factor(qn45)) %>% 
    rename(largest_n_drinks = qn45) %>%
#####################################################################################
    # During your life, how many times have you used marijuana?
    mutate(
      qn46 = recode(qn46,
                    "1" = "Yes",
                    "2" = "No"),
      qn46 = as.factor(qn46)) %>% 
    rename(ever_marijuana_use = qn46) %>%
#####################################################################################
    # How old were you when you tried marijuana for the first time?
    mutate(
      qn47 = recode(qn47,
                    "1" = "12 years or younger",
                    "2" = "never tried/13 years or older"),
      qn47 = as.factor(qn47)) %>% 
    rename(initiation_marijuana_use  = qn47) %>%
#####################################################################################
    # During the past 30 days, how many times did you use marijuana?
    mutate(
      qn48 = recode(qn48,
                    "1" = "Yes",
                    "2" = "No"),
      qn48 = as.factor(qn48),
      qn48 = fct_relevel(qn48, c("Yes", "No"))) %>% 
    rename(current_marijuana_use  = qn48) %>%
#####################################################################################
    # During your life, how many times have you used any form of cocaine, including powder, crack, or freebase
    mutate(
      qn49 = recode(qn49,
                    "1" = "Yes",
                    "2" = "No"),
      qn49 = as.factor(qn49)) %>% 
    rename(ever_cocaine_use  = qn49) %>%
#####################################################################################
    # During your life, how many times have you sniffed glue, breathed the contents of aerosol spray cans, or inhaled any paints or sprays to get high?
    mutate(
      qn50 = recode(qn50,
                    "1" = "Yes",
                    "2" = "No"),
      qn50 = as.factor(qn50)) %>% 
    rename(ever_inhalant_use  = qn50) %>%
#####################################################################################
    # During your life, how many times have you used heroin (also called smack, junk, or China White)?
    mutate(
      qn51 = recode(qn51,
                    "1" = "Yes",
                    "2" = "No"),
      qn51 = as.factor(qn51)) %>% 
    rename(ever_heroin_use  = qn51) %>%
#####################################################################################
    # During your life, how many times have you used methamphetamines (also called speed, crystal, crank, or ice)? A. 0 times
    mutate(
      qn52 = recode(qn52,
                    "1" = "Yes",
                    "2" = "No"),
      qn52 = as.factor(qn52)) %>% 
    rename(ever_methamphetamine_use  = qn52) %>%
#####################################################################################
    # During your life, how many times have you used ecstasy (also called MDMA)?
    mutate(
      qn53 = recode(qn53,
                    "1" = "Yes",
                    "2" = "No"),
      qn53 = as.factor(qn53)) %>% 
    rename(ever_ecstasy_use  = qn53) %>%
#####################################################################################
    # During your life, how many times have you used synthetic marijuana (also called K2, Spice, fake weed, King Kong, Yucatan Fire, Skunk, or Moon Rocks)?
    mutate(
      qn54 = recode(qn54,
                    "1" = "Yes",
                    "2" = "No"),
      qn54 = as.factor(qn54)) %>% 
    rename(ever_synthetic_marijuana_use  = qn54) %>%
#####################################################################################
    # During your life, how many times have you taken steroid pills or shots without a doctor's prescription?
    mutate(
      qn55 = recode(qn55,
                    "1" = "Yes",
                    "2" = "No"),
      qn55 = as.factor(qn55)) %>% 
    rename(ever_steroid_use  = qn55) %>%
#####################################################################################
    # During your life, how many times have you used a needle to inject any illegal drug into your body?
    mutate(
      qn57 = recode(qn57,
                    "1" = "Yes",
                    "2" = "No"),
      qn57 = as.factor(qn57)) %>% 
    rename(illegal_injected_drug_use  = qn57) %>%
#####################################################################################
    # During the past 12 months, has anyone offered, sold, or given you an illegal drug on school property?
    mutate(
      qn58 = recode(qn58,
                    "1" = "Yes",
                    "2" = "No"),
      qn58 = as.factor(qn58)) %>% 
    rename(illegal_drugs_at_school  = qn58) %>%
#####################################################################################
    # Have you ever had sexual intercourse?
    mutate(
      qn59 = recode(qn59,
                    "1" = "Yes",
                    "2" = "No"),
      qn59 = as.factor(qn59)) %>% 
    rename(ever_sexual_intercourse  = qn59) %>%
#####################################################################################
    # How old were you when you had sexual intercourse for the first time?
    mutate(
      qn60 = recode(qn60,
                    "1" = "12 years or younger",
                    "2" = "never sexual intercourse/ 13 years or older"),
      qn60 = as.factor(qn60)) %>% 
    rename(sex_before_13 = qn60) %>%
#####################################################################################
    # During your life, with how many people have you had sexual intercourse?
    mutate(
      qn61 = recode(qn61, 
                    "1" = "4 people or more",
                    "2" = "3 people or less"),
      qn61 = as.factor(qn61)) %>% 
    rename(multiple_sex_partner = qn61) %>%
#####################################################################################
    # During the past 3 months, with how many people did you have sexual intercourse?
    mutate(
      qn62 = recode(qn62,
                    "1" = "1 people or more",
                    "2" = "never sex/no sex during past 3 month"),
      qn62 = as.factor(qn62)) %>% 
    rename(current_sexual_activity = qn62) %>%
#####################################################################################
    # Did you drink alcohol or use drugs before you had sexual intercourse the last time?
    mutate(
      qn63 = recode(qn63,
                    "1" = "yes",
                    "2" = "no/not had sex yet"),
      qn63 = as.factor(qn63)) %>% 
    rename(alcohol_drugs_sex = qn63) %>%
#####################################################################################
    # The last time you had sexual intercourse, did you or your partner use a condom?
    mutate(
      qn64 = recode(qn64,
                    "1" = "yes",
                    "2" = "no/ not had sex yet"),
      qn64 = as.factor(qn64)) %>% 
    rename(condom_use = qn64) %>%
#####################################################################################
    # The last time you had sexual intercourse, what one method did you or your partner use to prevent pregnancy? (Select only one response.)
    mutate(
      qn65 = recode(qn65,
                    "1" = "Birth control",
                    "2" = "never sexual intercourse/nothing/other method"),
      qn65 = as_factor(qn65)) %>% 
    rename(birth_control_use = qn65) 
#####################################################################################

  df = rowid_to_column(df, "id")
}
```

``` r
# Combining Boroughs to one dataset
df_total = bind_rows(df_Bronx, df_Queens, df_Manhattan, df_StatenIsland, df_Brooklyn)

# How many NA? -> helps us determine which variables to exclude
prop_NA_all = prop_NA_original_data(df_total)

# Actual Data Cleaning
df_total = data_cleaning(df_total)

# How many NA in cleaned data?
prop_NA_clean = prop_NA_original_data(df_total)
```

``` r
write_xlsx(x = df_total, path = "./data/nyc_data.xlsx", col_names = TRUE)
save(df_total, file = "./data/nyc_data.RData")
```

The following sections get the percentage of NA and the distribution of
all response among all years and all question into the data cleaning
rmd, after the undesired column is removed, can get new result by simply
rerun the following codes.

``` r
  #### fill in missing values with "unknown" 
fill_NA = function(vector){
    as.factor(coalesce(as.character(vector),"unknown"))
  }
##This function take the df_total output and shown the amount of different response per column per year per borough
distrib = function(df=df_total){distribution=map_df(df,fill_NA)%>%mutate(id = as.numeric(as.character(id)),year = as.numeric(as.character(year)))%>%
  select(-"id")%>%
  pivot_longer(names_to = "question",values_to = "response",cols = 3:ncol(.))%>%
  count(borough,year,question,response)%>%group_by(borough,year,question,response)%>%
  summarise(sum(n))%>%left_join(x = . , y = df%>%group_by(borough,year)%>%summarise(n()))%>%
  mutate(respons_count = `sum(n)`, total_count =`n()`, perc = `sum(n)`/`n()`)%>%select(-"sum(n)",-"n()")}

distribution = distrib(df=df_total)
```

    ## Joining, by = c("borough", "year")

``` r
distribution
```

    ## # A tibble: 6,722 x 7
    ## # Groups:   borough, year, question [2,400]
    ##    borough  year question    response     respons_count total_count    perc
    ##    <fct>   <dbl> <chr>       <fct>                <int>       <int>   <dbl>
    ##  1 Bronx    2003 age         13 years old             2        1347 1.48e-3
    ##  2 Bronx    2003 age         14 years old           172        1347 1.28e-1
    ##  3 Bronx    2003 age         15 years old           326        1347 2.42e-1
    ##  4 Bronx    2003 age         16 years old           375        1347 2.78e-1
    ##  5 Bronx    2003 age         17 years old           313        1347 2.32e-1
    ##  6 Bronx    2003 age         18 years ol~           158        1347 1.17e-1
    ##  7 Bronx    2003 age         unknown                  1        1347 7.42e-4
    ##  8 Bronx    2003 alcohol_dr~ unknown                834        1347 6.19e-1
    ##  9 Bronx    2003 alcohol_dr~ no/not had ~           444        1347 3.30e-1
    ## 10 Bronx    2003 alcohol_dr~ yes                     69        1347 5.12e-2
    ## # ... with 6,712 more rows

``` r
##This table shown the percentage of missing value per column per year


naperc_df=map_df(df_total,fill_NA)%>%mutate(id = as.numeric(as.character(id)),year = as.numeric(as.character(year)))%>%
  select(-"id")%>%
  pivot_longer(names_to = "question",values_to = "response",cols = 3:ncol(.))%>%
  count(borough,year,question,response)%>%
  filter(response == "unknown")%>%
  group_by(year,question)%>%
  summarise(sum(n))%>%
  left_join(x = . , y = df_total%>%group_by(year)%>%summarise(n()))%>%
  mutate(naperc = `sum(n)`/`n()`,actrsp =-`sum(n)`+`n()` )
```

    ## Joining, by = "year"

``` r
   regression_variable<-c(
    # binary respone
    "current_vaping",
    # possible predictor
    "texting_and_driving",
    "carring_weapon",
    "sad_hopeless",
    "attempted_suicide",
    "injurious_suicide_attempt",
    "safety_concerns_at_school",
    "threatened_at_school",
    "physical_fighting",
    "bullying_at_school",
    "bullying_electronically",
    "sex",
    "age",
    "race7",
    "seatbelt_use")
   
   naperc_df%>%filter(year %in% c(2015,2017))
```

    ## # A tibble: 120 x 6
    ## # Groups:   year [2]
    ##     year question                `sum(n)` `n()`  naperc actrsp
    ##    <dbl> <chr>                      <int> <int>   <dbl>  <int>
    ##  1  2015 age                           36  8522 0.00422   8486
    ##  2  2015 alcohol_drugs_sex           7239  8522 0.849     1283
    ##  3  2015 attempted_suicide           1445  8522 0.170     7077
    ##  4  2015 birth_control_use           7309  8522 0.858     1213
    ##  5  2015 bullying_at_school           179  8522 0.0210    8343
    ##  6  2015 bullying_electronically      210  8522 0.0246    8312
    ##  7  2015 carring_weapon               377  8522 0.0442    8145
    ##  8  2015 carring_weapon_school        210  8522 0.0246    8312
    ##  9  2015 condom_use                  7338  8522 0.861     1184
    ## 10  2015 considered_suicide           283  8522 0.0332    8239
    ## # ... with 110 more rows
