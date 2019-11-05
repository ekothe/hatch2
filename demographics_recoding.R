library(dplyr)
library(readr)
library(tableone)

dat <- read_csv("data/dat_col_passed.csv")

dat <- as.tbl(dat)

#### Recode categorical variables
dat <- dat %>% 
  mutate(
    education = case_when(
      education == 1 ~ "Less than high school",
      education == 2 ~ "High school diploma or GED",
      education == 3 ~ "Some college",
      education == 4 ~ "Associate's degree",
      education == 5 ~ "Bachelor's degree",
      education == 6 ~ "Professional or Masters degree",
      education == 7 ~ "Doctorate"), 
    
    household_income = as.factor(case_when(
      household_income == 1  ~ "Less than $10,000",
      household_income == 2  ~ "$10,000 - $19,999",
      household_income == 3  ~ "$20,000 - $29,999",
      household_income == 4  ~ "$30,000 - $39,999",
      household_income == 5  ~ "$40,000 - $49,999",
      household_income == 6  ~ "$50,000 - $59,999",
      household_income == 7  ~ "$60,000 - $69,999",
      household_income == 8  ~ "$70,000 - $79,999",
      household_income == 9  ~ "$80,000 - $89,999",
      household_income == 10 ~ "$90,000 - $99,999",
      household_income == 11 ~ "$100,000 - $149,999",
      household_income == 12 ~ "More than $150,000",
      household_income == 13 ~ "Prefer not to say")
          ))

### Recode gender

dat$gender <- toupper(dat$gender)

dat <- dat %>% 
  mutate(
    gender = case_when(
      gender ==  "%" ~ "NA",
      gender ==  "40" ~ "NA",
      gender ==  "54" ~ "NA",
      gender ==  "AGENDER" ~ "Agender",
      gender ==  "AGENDER (WOMAN)" ~ "Agender",
      gender ==  "ANDROGYNOUS" ~ "Androgynous",
      gender ==  "APACHE HELICOPTER... JUST KIDDING. THERE ARE ONLY TWO. I AM A MALE." ~ "Male",
      gender ==  "ASIAN" ~ "NA",
      gender ==  "CIS FEMALE" ~ "Female",
      gender ==  "DEMALE"  ~ "Female",
      gender ==  "DEMIGIRL" ~ "Demigirl",
      gender ==  "EMALE"  ~ "Female",
      gender ==  "F"  ~ "Female",
      gender ==  "FAMELA"  ~ "Female",
      gender ==  "FEAMLE" ~ "Female",
      gender ==  "FEM"  ~ "Female",
      gender ==  "FEMAE"  ~ "Female",
      gender ==  "FEMAIIL" ~ "Female",
      gender ==  "FEMAIL" ~ "Female",
      gender ==  "FEMAILE" ~ "Female",
      gender ==  "FEMAKE" ~ "Female",
      gender ==  "FEMAL" ~ "Female",
      gender ==  "FEMAL3" ~ "Female",
      gender ==  "FEMALD" ~ "Female",
      gender ==  "FEMALE" ~ "Female",
      gender ==  "FEMALE (CISGENDER)" ~ "Female",
      gender ==  "FEMALE TO NON-BINARY" ~ "Non-binary",
      gender ==  "FEMALEE" ~ "Female",
      gender ==  "FEMALEP" ~ "Female",
      gender ==  "FEMALS" ~ "Female",
      gender ==  "FEMALW" ~ "Female",
      gender ==  "FEMENINA" ~ "Female",
      gender ==  "FEMININE" ~ "Female",
      gender ==  "FENALE" ~ "Female",
      gender ==  "FMALE" ~ "Female",
      gender ==  "FRMALE" ~ "Female",
      gender ==  "FTM" ~ "Male",
      gender ==  "G"  ~ "NA",
      gender ==  "GENDER" ~ "NA",
      gender ==  "GENDER IS A SOCIAL CONSTRUCT - I'M SEXUALLY FEMALE" ~ "Female",
      gender ==  "GIRL" ~ "Female",
      gender ==  "M" ~ "Male",
      gender ==  "MAE" ~ "Male",
      gender ==  "MAEL" ~ "Male",
      gender ==  "MAIL" ~ "Male",
      gender ==  "MAILL" ~ "Male",
      gender ==  "MAKE" ~ "Male",
      gender ==  "MALAE" ~ "Male",
      gender ==  "MALE" ~ "Male",
      gender ==  "MALE(SEX, GENDER IS A SILLY CONSTRUCT)" ~ "Male",
      gender ==  "MALE." ~ "Male",
      gender ==  "MALES" ~ "Male",
      gender ==  "MALR" ~ "Male",
      gender ==  "MAN" ~ "Male",
      gender ==  "MAN/MALE" ~ "Male",
      gender ==  "MAOE" ~ "Male",
      gender ==  "MASCULINO" ~ "Male",
      gender ==  "MSLR" ~ "Male",
      gender ==  "NB" ~ "Non-binary",
      gender ==  "NON-BINARY" ~ "Non-binary",
      gender ==  "NON BINARY" ~ "Non-binary",
      gender ==  "NONBINARY" ~ "Non-binary",
      gender ==  "TRANS MAN" ~ "Male",
      gender ==  "TRANSGENDER" ~ "Transgender (preferred gender not stated)",
      gender ==  "TRANSGENDER FEMALE" ~ "Female",
      gender ==  "TRANSGENDER MAN" ~ "Male",
      gender ==  "TRANSMALE" ~ "Male",
      gender ==  "TRANSMASCULINE" ~ "Male",
      gender ==  "TRANSSEXUAL MALE (FTM)" ~ "Male",
      gender ==  "WOMAN" ~ "Female")
  )


## Relevel the factor variables

dat <- dat %>% 
  mutate(household_income = factor(household_income, levels = c("Less than $10,000",
                                                                 "$10,000 - $19,999", 
                                                                 "$20,000 - $29,999", 
                                                                 "$30,000 - $39,999",
                                                                 "$40,000 - $49,999",
                                                                 "$50,000 - $59,999",
                                                                 "$60,000 - $69,999",
                                                                 "$70,000 - $79,999",
                                                                 "$80,000 - $89,999",
                                                                 "$90,000 - $99,999",
                                                                 "$100,000 - $149,999",
                                                                 "More than $150,000",
                                                                 "Prefer not to say")),
         education = factor(education, levels = c("Less than high school",
                                                  "High school diploma or GED",
                                                  "Some college",
                                                  "Associate's degree",
                                                  "Bachelor's degree",
                                                  "Professional or Masters degree",
                                                  "Doctorate")),
         
         gender = factor(gender, levels = c("Male",
                                            "Female",
                                            "Non-binary",
                                            "Agender",
                                            "Androgynous",
                                            "Demigirl",
                                            "Transgender (preferred gender not stated)",
                                            "NA"))) 
## Create demographics table          
CreateTableOne(vars = c("age", 
                        "education",
                        "gender",
                        "household_income", 
                        "ideology_general", 
                        "ideology_economic", 
                        "ideology_social"), 
               data = dat)

