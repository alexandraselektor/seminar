library(readr)
library(dplyr)
library(psych)
library(ggplot2)
library(reshape2)
library(lmtest)
library(sandwich)
library(car)
library(jtools)
library(scales)
library(broom)
library(corrplot)
library(margins)
library(ggeffects)
library(stringi)
library(tidyr)


#===============================================================================
# יבוא הנתונים
#===============================================================================

df <- read_csv("~/Desktop/נתונים סמנריון.csv")

#===============================================================================
# שינוי שמות העמודות בטבלה מעברית לאנגלית וסינון עמודות לא רלוונטיות
#===============================================================================

df <- df |>
  rename(
    timestamp = 1,
    age = 2,
    local_pref_a = 3,
    brand_var_a = 4,
    quality_imp_a = 5,
    price_inf_a = 6,
    gov_action_a = 7,
    validity_check = 8,
    social_act = 9,
    ref_benefit = 10,
    ref_need = 11,
    local_damage_fear = 12,
    price_drop_belief = 13,
    quality_impact_ref = 14,
    gov_action_c = 15,
    local_pref_c = 16,
    quality_imp_c = 17,
    brand_var_c = 18,
    price_inf_c = 19,
    gender = 20,
    status = 21,
    education = 22,
    religion = 23,
    sector = 24,
    native_lang = 25,
    region = 26,
    salary = 27,
    employment = 28
  ) |> select(-1) 


#===============================================================================
# סינון תקינות שאלון
#===============================================================================

df <- df |>
  filter(validity_check == 3)

#===============================================================================
# בדיקת na
#===============================================================================

sum(!complete.cases(df))

colSums(is.na(df))

# 12 ערכים חסרים

df <- df |>
  mutate(
    # שינוי ערכים חסרים בפרקים א׳-ג׳ ל-3 בסולם
    across(
      c(ref_need, quality_impact_ref, gov_action_c, local_pref_c, 
        quality_imp_c, brand_var_c, price_inf_c),
      ~ ifelse(is.na(.), 3, .))) |>
  mutate(
    # הכנסת הגיל החסר לקבוצת הביניים
    age = ifelse(is.na(age), "41-55", age),
    
    # סטטוס אישי לערך אחר
    status = ifelse(is.na(status), "אחר", status),
    
    # השכלה לערך אחר
    education = ifelse(is.na(education), "אחר", education),
    
    # שכר לקבוצת השכר הממוצעת בישראל
    salary = ifelse(is.na(salary), "10,001-16,000 ש\"ח", salary)
  )

colSums(is.na(df))

#===============================================================================
# בדיקת היפוך היגדים שליליים 
#===============================================================================

df_coded <- df |>
  mutate(
    local_damage_fear_rev = 6 - local_damage_fear,
  )


#===============================================================================
# Cronbach's alpha
#===============================================================================

# משתנה כללי בעד הרפורמה
cronbach_alpha1 <- df_coded |> 
  select(local_damage_fear_rev, ref_need, ref_benefit, price_drop_belief, quality_impact_ref)

psych::alpha(cronbach_alpha1)
alpha_results1 <- psych::alpha(cronbach_alpha1)
print(alpha_results1)
#result: alpha 0.39

# שיפור המשתנה
cronbach_alpha2 <- df |> 
  select(price_drop_belief, ref_benefit)

psych::alpha(cronbach_alpha2)
alpha_results2 <- psych::alpha(cronbach_alpha2)
print(alpha_results2)
#result: alpha 0.6 

# אלפא נמוכה - לא נחבר את המשתנים

#===============================================================================
# קידוד 
#===============================================================================

df_coded <- df |>
  mutate(
    
    #===============================================================================
    # קידוד חלק א׳
    #===============================================================================
    
    local_pref_a_neg = ifelse(local_pref_a %in% c(1, 2), 1, 0),
    local_pref_a_neu = ifelse(local_pref_a == 3, 1, 0),
    local_pref_a_pos = ifelse(local_pref_a %in% c(4, 5), 1, 0),
    
    brand_var_a_neg = ifelse(brand_var_a %in% c(1, 2, 3), 1, 0),
    brand_var_a_pos = ifelse(brand_var_a %in% c(4, 5), 1, 0),
    
    quality_imp_a_neg = ifelse(quality_imp_a %in% c(1, 2, 3), 1, 0),
    quality_imp_a_pos = ifelse(quality_imp_a %in% c(4, 5), 1, 0),
    
    price_inf_a_neg = ifelse(price_inf_a %in% c(1, 2, 3), 1, 0),
    price_inf_a_pos = ifelse(price_inf_a %in% c(4, 5), 1, 0),

    gov_action_a_neg = ifelse(gov_action_a %in% c(1, 2), 1, 0),
    gov_action_a_pos = ifelse(gov_action_a %in% c(3, 4, 5), 1, 0),
    
    #===============================================================================
    # קידוד חלק ב׳
    #===============================================================================
    
    social_act_neg = ifelse(social_act %in% c(1, 2), 1, 0),
    social_act_neu = ifelse(social_act == 3, 1, 0),
    social_act_pos = ifelse(social_act %in% c(4, 5), 1, 0),

    ref_benefit_neg = ifelse(ref_benefit %in% c(1, 2), 1, 0),
    ref_benefit_neu = ifelse(ref_benefit == 3, 1, 0),
    ref_benefit_pos = ifelse(ref_benefit %in% c(4, 5), 1, 0),

    ref_need_neg = ifelse(ref_need %in% c(1, 2), 1, 0),
    ref_need_neu = ifelse(ref_need == 3, 1, 0),
    ref_need_pos = ifelse(ref_need %in% c(4, 5), 1, 0),
    
    local_damage_fear_neg = ifelse(local_damage_fear %in% c(1, 2), 1, 0),
    local_damage_fear_neu = ifelse(local_damage_fear == 3, 1, 0),
    local_damage_fear_pos = ifelse(local_damage_fear %in% c(4, 5), 1, 0),
    
    price_drop_belief_neg = ifelse(price_drop_belief %in% c(1, 2), 1, 0),
    price_drop_belief_neu = ifelse(price_drop_belief == 3, 1, 0),
    price_drop_belief_pos = ifelse(price_drop_belief %in% c(4, 5), 1, 0),

    quality_impact_ref_neg = ifelse(quality_impact_ref %in% c(1, 2), 1, 0),
    quality_impact_ref_neu = ifelse(quality_impact_ref == 3, 1, 0),
    quality_impact_ref_pos = ifelse(quality_impact_ref %in% c(4, 5), 1, 0),
    
    #===============================================================================
    # קידוד חלק ג׳
    #===============================================================================

    local_pref_c_neg = ifelse(local_pref_c %in% c(1, 2), 1, 0),
    local_pref_c_neu = ifelse(local_pref_c == 3, 1, 0),
    local_pref_c_pos = ifelse(local_pref_c %in% c(4, 5), 1, 0),
    
    brand_var_c_neg = ifelse(brand_var_c %in% c(1, 2, 3), 1, 0),
    brand_var_c_pos = ifelse(brand_var_c %in% c(4, 5), 1, 0),
    
    quality_imp_c_neg = ifelse(quality_imp_c %in% c(1, 2, 3), 1, 0),
    quality_imp_c_pos = ifelse(quality_imp_c %in% c(4, 5), 1, 0),
    
    price_inf_c_neg = ifelse(price_inf_c %in% c(1, 2, 3), 1, 0),
    price_inf_c_pos = ifelse(price_inf_c %in% c(4, 5), 1, 0),
    
    gov_action_c_neg = ifelse(gov_action_c %in% c(1, 2), 1, 0),
    gov_action_c_pos = ifelse(gov_action_c %in% c(3, 4, 5), 1, 0),
    )
    #===============================================================================
    # קידוד חלק ד׳
    #===============================================================================
    
df_coded <- df_coded |>
  mutate(
    
    age_coded = case_when(
      age == "18-30" ~ 24,
      age == "31-40" ~ 35.5,
      age %in% c("41-55", "מעל 56") ~ 62.4
    ),

  
    gender_female = ifelse(gender %in% c("נקבה"), 1, 0),
    gender_male = ifelse(gender %in% c("זכר","מעדיף לא לומר"), 1, 0),
    
    status_married = ifelse(status %in% c("נשוי.אה"), 1, 0),
    status_unmarried = ifelse(status %in% c("רווק.ה","אחר","פרודים","אלמן.ה","גרוש.ה"), 1, 0),

    education = case_when(
      grepl("תואר", education) ~ "אקדמאי",
      TRUE ~ "לא אקדמאי"
    ),
    education = factor(education, levels = c("לא אקדמאי", "אקדמאי")),
    
    education_high = ifelse(education %in% c("אקדמאי"), 1, 0),
    education_low = ifelse(education %in% c("לא אקדמאי"), 1, 0),
    
    religion_jewish = ifelse(religion %in% c("יהודי/יהודיה"), 1, 0),
    religion_non_jewish = ifelse(religion %in% c("נוצרי/נוצריה", "מוסלמי/מוסלמית", "אחר", "לא יודע, מסרב.ת לענות"), 1, 0),
    
    sector_religious = ifelse(sector %in% c("מסורתי.ת - דתי.ת", "דתי.ת", "חרדי.ת"), 1, 0),
    sector_non_religious = ifelse(sector %in% c("לא דתי.ת - חילוני.ת", "לא רלוונטי"), 1, 0),
    
    native_lang_russian = ifelse(native_lang %in% c("רוסית"), 1, 0),
    native_lang_hebrew = ifelse(native_lang %in% c("עברית"), 1, 0),
    native_lang_other = ifelse(native_lang %in% c("אחר", "אנגלית"), 1, 0),
    
    region_north = ifelse(region %in% c("צפון"), 1, 0),
    region_other = ifelse(region %in% c("מרכז", "אחר", "דרום"), 1, 0),
    
    salary = case_when(
      salary %in% c("לא היו הכנסות", "עד 3,000 ש\"ח", "3,001-4,000 ש\"ח", "4,001-5,000 ש\"ח", "5,001-6,000 ש\"ח", "6,001-7,500 ש\"ח", "7,501-10,000 ש\"ח") ~ "מתחת לממוצע",
      salary == "10,001-16,000 ש\"ח" ~ "סביב הממוצע",
      salary %in% c("16,001-21,000 ש\"ח", "מעל 21,000 ש\"ח") ~ "מעל לממוצע",
      TRUE ~ NA_character_
    ),
    salary = factor(salary, levels = c("מתחת לממוצע", "סביב הממוצע", "מעל לממוצע")),
    
    salary_below_avg = ifelse(salary %in% c("מתחת לממוצע"), 1, 0),
    salary_avg = ifelse(salary %in% c("סביב הממוצע"), 1, 0),
    salary_above_avg = ifelse(salary %in% c("מעל לממוצע"), 1, 0),
    
    employment_student = ifelse(employment %in% c("סטודנט.ית"), 1, 0),
    employment_yes = ifelse(employment %in% c("שכיר.ה", "עצמאי.ת"), 1, 0),
    employment_no = ifelse(employment %in% c("בגמלאות (פנסיה)", "מובטל.ת", "אחר"), 1, 0)
    
  )


#===============================================================================
# שינוי מיקום עמודות
#===============================================================================

df_coded <- df_coded |>
  
  relocate(age_coded, .after = age) |>
  
  relocate(local_pref_a_neg, .after = local_pref_a) |>
  relocate(local_pref_a_neu, .after = local_pref_a_neg) |>
  relocate(local_pref_a_pos, .after = local_pref_a_neu) |>
  
  relocate(brand_var_a_neg, .after = brand_var_a) |>  
  relocate(brand_var_a_pos, .after = brand_var_a_neg) |>
  
  relocate(quality_imp_a_neg, .after = quality_imp_a) |>
  relocate(quality_imp_a_pos, .after = quality_imp_a_neg) |>
  
  relocate(price_inf_a_neg, .after = price_inf_a) |>
  relocate(price_inf_a_pos, .after = price_inf_a_neg) |>
  
  relocate(gov_action_a_neg, .after = gov_action_a) |>
  relocate(gov_action_a_pos, .after = gov_action_a_neg) |>
  
  
  
  relocate(social_act_neg, .after = social_act) |>
  relocate(social_act_neu, .after = social_act_neg) |>
  relocate(social_act_pos, .after = social_act_neu) |>
  
  relocate(ref_benefit_neg, .after = ref_benefit) |>  
  relocate(ref_benefit_neu, .after = ref_benefit_neg) |>  
  relocate(ref_benefit_pos, .after = ref_benefit_neu) |>  
  
  relocate(ref_need_neg, .after = ref_need) |>
  relocate(ref_need_neu, .after = ref_need_neg) |>
  relocate(ref_need_pos, .after = ref_need_neu) |>
  
  relocate(local_damage_fear_neg, .after = local_damage_fear) |>
  relocate(local_damage_fear_neu, .after = local_damage_fear_neg) |>
  relocate(local_damage_fear_pos, .after = local_damage_fear_neu) |>
  
  relocate(price_drop_belief_neg, .after = price_drop_belief) |>
  relocate(price_drop_belief_neu, .after = price_drop_belief_neg) |>
  relocate(price_drop_belief_pos, .after = price_drop_belief_neu) |>
  
  relocate(quality_impact_ref_neg, .after = quality_impact_ref) |>
  relocate(quality_impact_ref_neu, .after = quality_impact_ref_neg) |>
  relocate(quality_impact_ref_pos, .after = quality_impact_ref_neu) |>
 
   
  
  relocate(local_pref_c_neg, .after = local_pref_c) |>
  relocate(local_pref_c_neu, .after = local_pref_c_neg) |>
  relocate(local_pref_c_pos, .after = local_pref_c_neu) |>
  
  relocate(brand_var_c_neg, .after = brand_var_c) |>  
  relocate(brand_var_c_pos, .after = brand_var_c_neg) |>
  
  relocate(quality_imp_c_neg, .after = quality_imp_c) |>
  relocate(quality_imp_c_pos, .after = quality_imp_c_neg) |>
  
  relocate(price_inf_c_neg, .after = price_inf_c) |>
  relocate(price_inf_c_pos, .after = price_inf_c_neg) |>
  
  relocate(gov_action_c_neg, .after = gov_action_c) |>
  relocate(gov_action_c_pos, .after = gov_action_c_neg) |>

  relocate(gender_female, .after = gender) |>
  relocate(gender_male, .after = gender_female) |>
  
  relocate(status_married, .after = status) |>
  relocate(status_unmarried, .after = status_married) |>
  
  relocate(education_high, .after = education) |>
  relocate(education_low, .after = education_high) |>
  
  relocate(religion_jewish, .after = religion) |>
  relocate(religion_non_jewish, .after = religion_jewish) |>
  
  relocate(sector_religious, .after = sector) |>
  relocate(sector_non_religious, .after = sector_religious) |>
  
  relocate(native_lang_hebrew, .after = native_lang) |>
  relocate(native_lang_russian, .after = native_lang_hebrew) |>
  relocate(native_lang_other, .after = native_lang_russian) |>
  
  relocate(region_north, .after = region) |>
  relocate(region_other, .after = region_north) |>
  
  relocate(salary_above_avg, .after = salary) |>
  relocate(salary_avg, .after = salary_above_avg) |>
  relocate(salary_below_avg, .after = salary_avg) |>
  
  relocate(employment_student, .after = employment) |>  
  relocate(employment_yes, .after = employment_student) |>
  relocate(employment_no, .after = employment_yes) 
  
  
df_coded_clean <- df_coded |>
  select(-c(local_pref_a, brand_var_a, quality_imp_a, price_inf_a, gov_action_a)) |>
  select(-c(validity_check, social_act, ref_benefit, ref_need, local_damage_fear, price_drop_belief, quality_impact_ref)) |>
  select(-c(local_pref_c, brand_var_c, quality_imp_c, price_inf_c, gov_action_c)) |>
  select(-c(age, gender, status, education, religion, sector, native_lang, region, salary, employment)) 

#===============================================================================
# השערה 1
#===============================================================================

# תרשים אתנוצנטריות לפי גיל
df_plot <- df_coded_clean |>
  mutate(
    pref_category = case_when(
      local_pref_a_pos == 1 ~ "יבויח",
      local_pref_a_neu == 1 ~ "ילרטיינ",
      local_pref_a_neg == 1 ~ "ילילש"
    ),
    pref_category = factor(pref_category, levels = c("ילילש", "ילרטיינ", "יבויח"))
  ) |> 
  select(age_coded, pref_category) 

ggplot(df_plot, aes(x = as.factor(age_coded), fill = pref_category)) +
  geom_bar(position = "fill", color = "white") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_fill_manual(values = c("ילילש" = "#F8766D", "ילרטיינ" = "#D3D3D3", "יבויח" = "#00BFC4"),
                    labels = c("ףידעמ אל", "ילרטיינ", "ףידעמ")) +
  labs(title = "ליג תוצובק יפל םיימוקמ םירצומ תפדעה",
       x = " (עצוממ)ליג תוצובק",
       y = "ליג תצובקמ זוחא",
       fill = "הפדעה תמר") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) 


# מבחן חי בריבוע אתנוצנטריות מול גיל
df_for_chisq <- df_coded_clean |>
  mutate(
    local_pref_category = case_when(
      local_pref_a_pos == 1 ~ "Positive",
      local_pref_a_neu == 1 ~ "Neutral",
      local_pref_a_neg == 1 ~ "Negative"
    )
  )  |> select (1,63)

cross_table <- table(df_for_chisq$age_coded, df_for_chisq$local_pref_category)
print(cross_table)

chisq_test_result <- chisq.test(cross_table)
print(chisq_test_result)
# p-value = 0.08763 .

# רגרסיה לוג׳ית עם כל המשתנים הדמוגרפים
model_logit_ethno_all <- glm(local_pref_a_pos ~ social_act_pos + age_coded + gender_male + status_married + education_low + religion_non_jewish + sector_religious + native_lang_other + native_lang_russian + region_north + salary_above_avg + salary_below_avg + employment_student + employment_no, data = df_coded_clean, family = binomial(link = "logit"))
summary(model_logit_ethno_all)
lrtest(model_logit_ethno_all)
# Chisq = 0.02981 *, sector_religious (p-value = 0.0178 *)

# שיפור המודל 1
model_logit_ethno_1 <- glm(local_pref_a_pos ~ social_act_pos + status_married + sector_religious + region_north , data = df_coded_clean, family = binomial(link = "logit"))
summary(model_logit_ethno_1)
lrtest(model_logit_ethno_1)
# Chisq = 0.003136 **, social_act_pos (p-value = 0.19760), status_married (p-value = 0.42931), sector_religious (p-value = 0.00267 **), region_north (p-value = 0.03981 *)

# שיפור המודל סופית
model_logit_ethno_2 <- glm(local_pref_a_pos ~ social_act_pos + sector_religious + region_north , data = df_coded_clean, family = binomial(link = "logit"))
summary(model_logit_ethno_2)
lrtest(model_logit_ethno_2)
# Chisq = 0.001585 **, sector_religious (p-value = 0.00148 **), region_north (p-value = 0.03021 *), social_act_pos (p-value = 0.20843)

lrtest(model_logit_ethno_all,model_logit_ethno_2)
# Chisq = 0.5103 


# מבחן מולטיקוליניאריות
vif(model_logit_ethno_2)


df_summary <- df_coded |>
  group_by(age_coded) |>
  summarise(
    mean_pref = mean(local_pref_a, na.rm = TRUE),
    se_pref = sd(local_pref_a, na.rm = TRUE) / sqrt(n()) 
  )

# חישוב ההשפעות השוליות הממוצעות (AME) 
margins_model <- margins(model_logit_ethno_2)
summary(margins_model)

# יחסי סיכויים (משמעות כלכלית)
exp(coef(model_logit_ethno_2))
# הסיכוי של צרכן מהמגזר הדתי/מסורתי להעדיף
# תוצרת מקומית הוא גבוה כמעט פי 3 מאשר הסיכוי  
# של קבוצת הייחוס (החילונים/אחר), בהינתן ששאר המשתנים זהים

#===============================================================================
# השערה 2
#===============================================================================
# העדפת מוצר מקומי
mean_a <- mean(df$local_pref_a, na.rm = TRUE)
mean_c <- mean(df$local_pref_c, na.rm = TRUE)

cat("ממוצע עוצמת ההעדפה למקומי (חלק א' - לפני רפורמה):", round(mean_a, 2), "\n")
cat("ממוצע עוצמת ההעדפה למקומי (חלק ג' - תחת רפורמה):", round(mean_c, 2), "\n")
cat("--------------------------------------------------\n")

wilcox_result <- wilcox.test(df$local_pref_a, 
                             df$local_pref_c, 
                             paired = TRUE)
print(wilcox_result) # p-value = 0.7744

# העדפת מגוון מוצרים
mean_a <- mean(df$brand_var_a, na.rm = TRUE)
mean_c <- mean(df$brand_var_c, na.rm = TRUE)

cat("ממוצע עוצמת ההעדפה למגוון מוצרים (חלק א' - לפני רפורמה):", round(mean_a, 2), "\n")
cat("ממוצע עוצמת ההעדפה למגוון מוצרים (חלק ג' - תחת רפורמה):", round(mean_c, 2), "\n")
cat("--------------------------------------------------\n")

wilcox_result <- wilcox.test(df$brand_var_a, 
                             df$brand_var_c, 
                             paired = TRUE)
print(wilcox_result) # p-value = 0.1558

# חשיבות איכות
mean_a <- mean(df$quality_imp_a, na.rm = TRUE)
mean_c <- mean(df$quality_imp_c, na.rm = TRUE)

cat("ממוצע חשיבות איכות המוצר (חלק א' - לפני רפורמה):", round(mean_a, 2), "\n")
cat("ממוצע חשיבות איכות המוצר (חלק ג' - תחת רפורמה):", round(mean_c, 2), "\n")
cat("--------------------------------------------------\n")

wilcox_result <- wilcox.test(df$quality_imp_a, 
                             df$quality_imp_c, 
                             paired = TRUE)
print(wilcox_result) # p-value = 0.003515

# השפעת מחיר
mean_a <- mean(df$price_inf_a, na.rm = TRUE)
mean_c <- mean(df$price_inf_c, na.rm = TRUE)

cat("ממוצע השפעת מחיר המוצר (חלק א' - לפני רפורמה):", round(mean_a, 2), "\n")
cat("ממוצע השפעת מחיר המוצר (חלק ג' - תחת רפורמה):", round(mean_c, 2), "\n")
cat("--------------------------------------------------\n")

wilcox_result <- wilcox.test(df$price_inf_a, 
                             df$price_inf_c, 
                             paired = TRUE)
print(wilcox_result) # p-value = 0.8893

# פעילות ממשלתית
mean_a <- mean(df$gov_action_a, na.rm = TRUE)
mean_c <- mean(df$gov_action_c, na.rm = TRUE)

cat("ממוצע עמדה כלפי פעילות ממשלתית (חלק א' - לפני רפורמה):", round(mean_a, 2), "\n")
cat("ממוצע עמדה כלפי פעילות ממשלתית (חלק ג' - תחת רפורמה):", round(mean_c, 2), "\n")
cat("--------------------------------------------------\n")

wilcox_result <- wilcox.test(df$gov_action_a, 
                             df$gov_action_c, 
                             paired = TRUE)
print(wilcox_result) # p-value = 0.8706

#===============================================================================

# מבחן מקנמר
table_mcnemar <- table(
  Before_Reform = df_coded$local_pref_a_pos, 
  After_Reform = df_coded$local_pref_c_pos
)
cat("טבלת המעברים (0 = לא מעדיף מקומי, 1 = מעדיף מקומי):\n")
print(table_mcnemar)
mcnemar_result <- mcnemar.test(table_mcnemar)
print(mcnemar_result)

# בדיקה של כמה שינו את דעתם מאתנוצנטרי לקוסמופוליטי
df_coded_clean <- df_coded_clean |>
  mutate(
    Switched_to_Import = ifelse(df_coded$local_pref_a_pos == 1 & df_coded$local_pref_c_pos == 0, 1, 0),
    Switched_to_Local = ifelse(local_pref_a_pos == 0 & local_pref_c_pos == 1, 1, 0)  )
table(df_coded_clean$Switched_to_Import)
table(df_coded_clean$Switched_to_Local)

# מה מסביר מעבר מהעדפת מקומי להעדפת יבוא
model_switchers_to_Import <- glm(Switched_to_Import ~ price_drop_belief_pos + ref_benefit_pos + quality_impact_ref_pos + gov_action_a_pos,
                                data = df_coded_clean,
                                family = "binomial")

summary(model_switchers_to_Import)
lrtest(model_switchers_to_Import)

# מה מסביר מעבר מהעדפת מקומי להעדפת יבוא שיפור
model_switchers_to_Import2 <- glm(Switched_to_Import ~ ref_benefit_pos + ref_benefit_neu + gov_action_a_pos,
                                 data = df_coded_clean,
                                 family = "binomial")

summary(model_switchers_to_Import2)
vif(model_switchers_to_Import2)
lrtest(model_switchers_to_Import2)

# מה מסביר מעבר מהעדפת יבוא להעדפת מקומי
model_switchers_to_local <- glm(Switched_to_Local ~ local_damage_fear_pos + quality_impact_ref_pos + region_north + sector_religious,
                                  data = df_coded_clean,
                                  family = "binomial")
  
summary(model_switchers_to_local)
lrtest(model_switchers_to_local)

# מה מסביר מעבר מהעדפת יבוא להעדפת מקומי שיפור
model_switchers_to_local2 <- glm(Switched_to_Local ~ local_damage_fear_pos + region_north + sector_religious,
                                data = df_coded_clean,
                                family = "binomial")

summary(model_switchers_to_local2)
vif(model_switchers_to_local2)
lrtest(model_switchers_to_local2)


margins_model_switchers_to_local <- margins(model_switchers_to_local2)
summary(margins_model_switchers_to_local)

margins_model_switchers_to_Import <- margins(model_switchers_to_Import2)
summary(margins_model_switchers_to_Import)


#===============================================================================
# השערה 3
#===============================================================================

df_coded_clean <- df_coded_clean |>
  mutate(
    import_pref_c_pos = local_pref_c_neg, 
    goal_group = ifelse(price_drop_belief_pos == 1 & quality_impact_ref_pos == 1, "Goal (Both)", "Others")
  )

model_logit_h3 <- glm(import_pref_c_pos ~ price_drop_belief_pos * quality_impact_ref_pos, 
                      family = "binomial", 
                      data = df_coded_clean)
summary(model_logit_h3)
vif(model_logit_h3)
lrtest(model_logit_h3)


#===============================================================================
# גרפים למצגת
#===============================================================================


plot_data <- tidy(model_logit_ethno_2, exponentiate = TRUE, conf.int = TRUE) |>
  filter(term != "(Intercept)") |> 
  mutate(
    term_heb = case_when(
      term == "sector_religious" ~ "יתרוסמ/יתד רזגמ",
      term == "region_north" ~ "ןופצ רוזא",
      term == "social_act_pos" ~ "תיתרבח תוברועמ"
    )
  )

ggplot(plot_data, aes(x = estimate, y = reorder(term_heb, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 1.2) + 
  
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, linewidth = 1, color = "#2C3E50") + 
  
  geom_point(size = 5, color = "#E74C3C") + 
  
  labs(
    title = "תימוקמ תרצות תפדעהל יוכיסה לע םינתשמה תעפשה",
    subtitle = "Religious Sector p-value = 0.00148 **, North Region p-value = 0.03021 *",
    x = " ךמס יחוורו םייוכיס סחי",
    y = ""
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 9.5, color = "blue"),
    axis.text.y = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

# 1. הרצת המודל (בדיוק כפי שהרצתן בעבודה)
model_interaction <- glm(import_pref_c_pos ~ price_drop_belief_pos * quality_impact_ref_pos, 
                         family = binomial(link = "logit"), 
                         data = df_coded_clean)
summary(model_interaction)

# 2. חילוץ ההסתברויות המנובאות (Predicted Probabilities)
# הפונקציה הזו מחשבת את ההסתברות ליבוא עבור כל שילוב של מחיר ואיכות
predictions <- ggpredict(model_interaction, terms = c("price_drop_belief_pos", "quality_impact_ref_pos"))
ggplot(predictions, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(linewidth = 1.2) + # שרטוט הקווים
  geom_point(size = 3) +       # הוספת נקודות בקצוות
  
  annotate("text", x = 0.5, y = 0.8, label = "Interaction p-value = 0.344", 
           size = 4.5, color = "red3", fontface = "italic") +
  # הגדרת הצבעים והתוויות של קבוצות האיכות עם היפוך טקסט
  scale_color_manual(values = c("0" = "#C5A059", "1" = "#003366"), 
                     labels = c(stri_reverse("איכות לא משתפרת )0("), 
                                stri_reverse("איכות משתפרת )1("))) +
  
  # עיצוב ציר ה-Y כאחוזים בין 0 ל-100%
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  
  # הגדרת הטקסטים של ציר ה-X עם היפוך טקסט
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c(stri_reverse("לא יורד/נייטרלי )0("), 
                                stri_reverse("מחיר יורד )1("))) +
  
  # כותרות הגרף עם היפוך טקסט
  labs(
    title = stri_reverse("אפקט האינטראקציה: הקשר בין מחיר ואיכות לבין העדפת יבוא"),
    subtitle = stri_reverse("קווים מקבילים מעידים על היעדר אינטראקציה מובהקת"),
    color = stri_reverse(":תפיסת שיפור באיכות"),   
    x = stri_reverse("תפיסת ירידת מחירים בעקבות הרפורמה"),
    y = stri_reverse("הסתברות מנובאת להעדפת יבוא")
  ) +
  
  # עיצוב נקי ואקדמי
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )







mean_scores_quick <- df %>%
  summarise(across(
    .cols = c(local_pref_a, local_pref_c, brand_var_a, brand_var_c, quality_imp_a, quality_imp_c, price_inf_a, price_inf_c, gov_action_a, gov_action_c), 
    .fns = ~ mean(.x, na.rm = TRUE)
  ))

df_graph <- data.frame(
  Variable = c("פעילות ממשלתית", "השפעת מחיר", "חשיבות איכות", "העדפת מגוון", "העדפה מקומית"),
  Pre_Exposure = c(1.73, 4.28, 4.53, 4.22, 3.59),
  Post_Exposure = c(1.73, 4.28, 4.42, 4.15, 3.59)
)

# 2. סידור הנתונים למבנה "ארוך"
df_long <- df_graph %>%
  pivot_longer(
    cols = c(Pre_Exposure, Post_Exposure), 
    names_to = "Condition", 
    values_to = "Mean_Score"
  ) %>%
  mutate(Condition = case_when(
    Condition == "Pre_Exposure" ~ "טרום-חשיפה",
    Condition == "Post_Exposure" ~ "פוסט-חשיפה"
  ))

# --- הוספת stri_reverse להפיכת הטקסט בעברית ---

# הפיכת הטקסטים בעמודות עצמן
df_long$Variable <- stri_reverse(df_long$Variable)
df_long$Condition <- stri_reverse(df_long$Condition)

# הגדרת סדר המשתנים בציר ה-X והמקרא (הפכנו גם את המילים כאן כדי שיתאימו לנתונים החדשים)
df_long$Variable <- factor(df_long$Variable, levels = stri_reverse(c("פעילות ממשלתית", "השפעת מחיר", "חשיבות איכות", "העדפת מגוון", "העדפה מקומית")))
df_long$Condition <- factor(df_long$Condition, levels = stri_reverse(c("טרום-חשיפה", "פוסט-חשיפה")))

# 3. יצירת הגרף
ggplot(df_long, aes(x = Variable, y = Mean_Score, fill = Condition)) +
  geom_col(position = "dodge", width = 0.7) + 
  scale_y_continuous(breaks = 1:5) + 
  coord_cartesian(ylim = c(1, 5)) +  
  # הגדרת הצבעים - היינו צריכים להפוך את השמות גם כאן
  scale_fill_manual(values = setNames(c("#2c7bb6", "blue4"), stri_reverse(c("טרום-חשיפה", "פוסט-חשיפה")))) +
  
  # הפיכת הכותרות של הגרף וציר ה-Y
  labs(
    title = stri_reverse("פילוג עמדות כללי - לפי משתנה"),
    x = "", 
    y = stri_reverse("ממוצע )סולם 1-5("),
    fill = "" 
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    axis.text.x = element_text(size = 12, color = "black"), 
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "top", 
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_blank() 
  )