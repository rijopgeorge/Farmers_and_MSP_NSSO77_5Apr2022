# install.packages("pacman")
# library(pacman)
pacman::p_load(
  readr,
  dplyr,
  ggplot2,
  ggthemes,
  patchwork,
  broom,
  socviz,
  coefplot,
  aod,
  sampleSelection,
  #googlesheets4,
  #survey,
  #srvyr,
  plotly
)
options(scipen = 1)

#Importing Data
#Unit level data of 77th round National Sample Survey (Land and Livestock Holding of Households and Situation Assessment of Agricultural Households) is taken from the site of National Sample Survey Organisation (NSSO). The text files are hosted at https://mospi.gov.in/web/mospi/download-tables-data/-/reports/view/templateFour/25302?q=TBDCAT. The readme file is available at https://mospi.gov.in/documents/213904//1254572//1628937829275_README77331_V1.pdf//98f73b41-f73c-0ae6-bc1c-c08c51d8026a. NSSO shares the data as 18*2 fixed width text files, named Level 1 through 18. An introduction to the 77th round of National Sample Survey can be found at https://mospi.gov.in/documents/213904/0/ins.0.0%2C77%2CCh3.pdf and https://mospi.gov.in/documents/213904/0/ins.0.0%2C77%2CCh4.pdf. Files are also saved offline, but online links are provided for import for the ease of collaborative working.   


#Level01
V1L01_url <- "https://mospi.gov.in/documents/213904/1253413/r77s331v1L01.txt/d5524418-3bf0-3c54-3225-a9890dfb20fe?t=1628927696632" #"/home/rpg/LKYSPP DataViz 12Jan2022/Project on nsso data 18Mar2022/Visit1 text files/r77s331v1L01"
V1L01_var_widths <- c(3,5,2,3,1,1,3,2,2,2,1,4,1,2,1,2,5,2,1,1,1,4,4,4,6,6,3,1,1,1,1,1,49,3,10)
V1L01_var_names <- c("V1L01Q01_centre_code_round","V1L01Q02_fsu_serial_no","V1L01Q03_round","V1L01Q04_schedule","V1L01Q05_sample","V1L01Q06_sector","V1L01Q07_nss_region","V1L01Q08_district","V1L01Q09_stratum","V1L01Q10_sub_stratum","V1L01Q11_sub_round","V1L01Q12_fod_sub_region","V1L01Q12_second_stage_stratum_no","V1L01Q12_sample_hhld_no","V1L01Q12_visit_number","V1L01Q13_level","V1L01Q14_filler","V1L01Q15_informant_slno","V1L01Q16_response_code","V1L01Q17_survey_code","V1L01Q18_reason_for_substitution_casualty__code","V1L01Q19_employee_code","V1L01Q20_employee_code","V1L01Q21_employee_code","V1L01Q22_date_of_survey","V1L01Q23_date_of_despatch","V1L01Q24_time_to_canvass_in_minutes","V1L01Q25_no_of_investigators_fi_aso_in_the_team","V1L01Q26_remarks_in_block_17_18","V1L01Q27_remarks_in_block17_18","V1L01Q28_remarks_elsewhere_in_sch","V1L01Q29_remarks_elsewhere_in_sch","V1L01Q30_blank","V1L01Q31_nsc","V1L01Q32_multiplier")
V1L01 <- read_fwf(V1L01_url,fwf_widths(V1L01_var_widths,V1L01_var_names))
V1L01$HHID <- paste0(V1L01$V1L01Q02_fsu_serial_no,V1L01$V1L01Q12_second_stage_stratum_no,V1L01$V1L01Q12_sample_hhld_no)
V1L01$State_code <- substring(V1L01$V1L01Q07_nss_region,1,2)

#Adding the state codes here, instead of saving it as a separate csv, as it is a master table, and may not be required to change
State_codes_master <- tibble(
  State_code = c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36"),
  State_name = c("Jammu & Kashmir","Himachal Pradesh","Punjab","Chandigarh","Uttarakhand","Haryana","Delhi","Rajasthan","Uttar Pradesh","Bihar","Sikkim","Arunachal Pradesh","Nagaland","Manipur","Mizoram","Tripura","Meghalaya","Assam","West Bengal","Jharkhand","Odisha","Chhattisgarh","Madhya Pradesh","Gujarat","Daman & Diu","D & N Haveli","Maharashtra","Andhra Pradesh","Karnataka","Goa","Lakshadweep","Kerala","Tamil Nadu","Puducherry","A & N Islands","Telangana")
)
V1L01 <- left_join(V1L01,State_codes_master, by="State_code")
#View(V1L01)

#Level02
V1L02_url <- "https://mospi.gov.in/documents/213904/1253413/r77s331v1L02.txt/d5afc977-f9e0-30e0-d7ee-76e82ed523c1?t=1628927699000"
V1L02_var_widths <- c(33,2,3,2,1,1,3,2,1,2,2,2,2,10,10,10,1,39,3,10)
V1L02_var_names <- c("V1L02Q01_common_id","V1L02Q02_level","V1L02Q03_filler","V1L02Q03_person_srl_no","V1L02Q04_relation_to_head","V1L02Q04_gender","V1L02Q05_age","V1L02Q05_highest_level_of_education","V1L02Q06_whether_attended_training_in_agriculture","V1L02Q07_principal_activity_status_code","V1L02Q08_principal_activity_nic_08_code","V1L02Q09_subsidiary_economic_activity_status_code","V1L02Q10_subsidiary_economic_activity_nic_08_code","V1L02Q11_wages_&_salary_earnings_rs","V1L02Q12_earning_from_pension_remittances_rs_","V1L02Q13_income_from_rent_of_leased_out_land_rs_","V1L02Q14_whether_associated_with_the_household_operational_holding_during_the_ay_2018_19","V1L02Q15_blank","V1L02Q16_nsc","V1L02Q17_multiplier")
V1L02 <- read_fwf(V1L02_url,fwf_widths(V1L02_var_widths,V1L02_var_names))
V1L02$HHID <- paste0(substring(V1L02$V1L02Q01_common_id,4,8),
                     substring(V1L02$V1L02Q01_common_id,30,32))
V1L02$V1L02Q05_age = as.numeric(V1L02$V1L02Q05_age)

V1L02 <- V1L02 %>%
  filter(V1L02Q04_relation_to_head=="1") %>%
  filter(V1L02Q05_age >= 18) %>%
  filter(V1L02Q04_gender == "1" | V1L02Q04_gender == "2") %>%
  mutate(V1L02Q04_gender=recode(V1L02Q04_gender,"1"="Male","2"="Female")) 
#View(V1L02)

Education_codes_master <- tibble(
  Education_code = c("01","02","03","04","05","06","07","08","10","11","12"),
  Education_collapsed = c("Illiterate","Primary","Primary","Primary","Secondary","Secondary","Secondary","Secondary","Secondary","Tertiary","Tertiary")
)

V1L02 <- left_join(V1L02,Education_codes_master,by=c(
  "V1L02Q05_highest_level_of_education"="Education_code"
))
#View(V1L02)

V1_merged <- merge(V1L01,V1L02,by="HHID")



#Level03
V1L03_url <- "https://mospi.gov.in/documents/213904/1253413/r77s331v1L03.txt/4c7dfd38-2198-34a2-7eeb-d12d6991a525?t=1628927701223"
V1L03_var_widths <- c(33,2,5,3,1,1,1,10,10,10,10,10,1,1,1,1,1,1,1,1,1,1,1,1,18,3,10)
V1L03_var_names <- c("V1L03Q01_common_id","V1L03Q02_level","V1L03Q03_filler","V1L03Q04_household_size","V1L03Q05_religion_code","V1L03Q06_social_group_code","V1L03Q07_household_classification_code","V1L03Q08_usual_consumer_expenditure_in_a_month_for_household_purposes_out_of_purchase_a_","V1L03Q09_imputed_value_of_usual_consumption_in_a_month_from_home_grown_stock_b_","V1L03Q10_imputed_value_of_usual_consumption_in_a_month_from_wages_in_kind,_free_collection,_gifts,_etc_c_","V1L03Q11_expenditure_on_purchase_of_household_durable_during_last_365_days_d_","V1L03Q12_usual_monthly_consumer_expenditure_e_[a+b+c+_d_12_]","V1L03Q13_value_of_agricultural_production_from_self_employment_activities_during_the_last_365_days_code_","V1L03Q14_dwelling_unit_code","V1L03Q15_type_of_structure","V1L03Q16_whether_any_of_the_household_member_has_bank_account","V1L03Q17_whether_any_of_the_household_member_possesses_mgnreg_job_card","V1L03Q18_whether_undertook_any_work_under_mgnreg_during_the_last_365_days","V1L03Q19_whether_any_of_the_household_member_is_a_member_of_registered_farmers’_organisation","V1L03Q20_whether_the_household_possesses_any_kisan_credit_card","V1L03Q21_whether_the_household_possess_soil_health_card","V1L03Q22_whether_fertilizer,_etc_applied_to_field_as_per_recommendations_of_soil_health_card","V1L03Q23_whether_the_household_possess_animal_health_card_nakul_swasthya_patra","V1L03Q24_whether_the_household_insured_any_crop_under_pm_fasal_bima_yojana_during_last_365_days","V1L03Q25_blank","V1L03Q26_nsc","V1L03Q27_multiplier")
V1L03 <- read_fwf(V1L03_url,fwf_widths(V1L03_var_widths,V1L03_var_names))
V1L03$HHID <- paste0(substring(V1L03$V1L03Q01_common_id,4,8),substring(V1L03$V1L03Q01_common_id,30,32))

V1_merged <- merge(V1_merged,V1L03, by="HHID")
#View(V1_merged)



#Level04
V1L04_url <- "https://mospi.gov.in/documents/213904/1253413/r77s331v1L04.txt/9e9b10a8-2020-8397-eb9c-706804223cb1?t=1628927703289"
V1L04_var_widths <- c(33,2,3,2,6,1,6,6,6,6,6,6,2,1,6,1,2,1,6,6,6,6,6,3,10)
V1L04_var_names <- c("V1L04Q01_common_id","V1L04Q02_level","V1L04Q03_filler","V1L04Q04_srl_no","V1L04Q05_area_of_land_0_00_acre","V1L04Q06_whether_used_for_any_agricultural_production_during_july_december_2018","V1L04Q07_land_used_for_shifting_jhum_cultivation","V1L04Q08_land_other_than_the_land_used_for_shifting_jhum_cultivation","V1L04Q09_only_for_farming_of_animals","V1L04Q10_both_for_growing_of_crop_and_farming_of_animals","V1L04Q11_other_agricultural_uses","V1L04Q12_other_land_not_used_for_agriculture_purpose","V1L04Q13_major_type_of_crop_grown_animal_farming_undertaken_code","V1L04Q14_whether_any_part_of_the_land_was_rrigated","V1L04Q15_area_of_land_irrigated_0_00_acre","V1L04Q16_source_of_irrigation_major_source","V1L04Q17_source_of_irrigation_2nd_major_source","V1L04Q18_tenure_of_lease_for_major_part_of_the_land_code_","V1L04Q19_area_of_land_by_terms_of_lease_for_fixed_money","V1L04Q20_area_of_land_by_terms_of_lease_for_fixed_produce","V1L04Q21_area_of_land_by_terms_of_lease_for_share_of_produce","V1L04Q22_area_of_land_by_terms_of_lease_under_no_specified_terms_from_relatives__to_relatives","V1L04Q23_area_of_land_by_terms_of_lease_under_other_terms","V1L04Q24_nsc","V1L04Q25_multiplier")
V1L04 <- read_fwf(V1L04_url,fwf_widths(V1L04_var_widths,V1L04_var_names))
V1L04$HHID <- paste0(substring(V1L04$V1L04Q01_common_id,4,8),substring(V1L04$V1L04Q01_common_id,30,32))
#View(V1L04)
#Finding Simpson index of diversity for each HHID
Simpson_index <- V1L04 %>%
  filter(V1L04Q06_whether_used_for_any_agricultural_production_during_july_december_2018=="1") %>%
  group_by(HHID) %>%
  mutate(Gross_cropped_area = sum(as.numeric(V1L04Q05_area_of_land_0_00_acre))) %>%
  mutate(Proportional_area_for_specific_crop_squared = ((as.numeric(V1L04Q05_area_of_land_0_00_acre)/Gross_cropped_area))^2) %>%
  group_by(HHID) %>%
  summarise(Simpson_index_score = 1-sum(Proportional_area_for_specific_crop_squared))

# View(Simpson_index)
  
  

#Level16
V1L16_url <- "https://mospi.gov.in/documents/213904/1253413/r77s331v1L16.txt/8f07554e-5231-d611-9286-5f0716f35efb?t=1628927725689"
V1L16_var_widths <- c(33,2,4,1,4,1,1,1,1,10,10,1,57,3,10)
V1L16_var_names <- c("V1L16Q01_common_id","V1L16Q02_level","V1L16Q03_filler","V1L16Q04_serial_no_of_crop_as_in_col_1_block_6","V1L16Q05_crop_code_as_in_col_2_of_block_6","V1L16Q06_unit_code_as_in_col_3_of_block_6","V1L16Q07_aware_of_msp_of_this_crop","V1L16Q08_agency_procures_this_crop_at_msp","V1L16Q09_sell_to_any_of_the_agencies","V1L16Q10_quantity_sold","V1L16Q11_sell_rate_rs_0_00_","V1L16Q12_reason_code","V1L16Q13_blank","V1L16Q14_nsc","V1L16Q15_multiplier")
V1L16 <- read_fwf(V1L16_url,fwf_widths(V1L16_var_widths,V1L16_var_names))
V1L16$HHID <- paste0(substring(V1L16$V1L16Q01_common_id,4,8),
                     substring(V1L16$V1L16Q01_common_id,30,32))
V1L16 <- V1L16 %>%
  mutate(V1L16Q07_aware_of_msp_of_this_crop = recode(
    V1L16Q07_aware_of_msp_of_this_crop, "2"=0
  ))
V1L16$V1L16Q07_aware_of_msp_of_this_crop = as.numeric(
  V1L16$V1L16Q07_aware_of_msp_of_this_crop)    
    
#L16Q07_aware_of_msp_of_this_crop,"1"="Yes","2"="No")) 
#View(V1L16)
#Adding the crop codes here, instead of saving it as a separate csv, as it is a master table, and may not be required to change. Source is 
Crop_codes_master <-tibble(
  Crop_code =c("0101","0102","0103","0104","0105","0106","0107","0108","0188","0201","0202","0203","0204","0205","0206","0207","0208","0288","0401","0402","0488","0501","0502","0503","0504","0505","0506","0507","0508","0509","0510","0511","0512","0513","0514","0515","0516","0517","0518","0519","0588","0601","0602","0603","0604","0605","0606","0607","0608","0609","0610","0611","0612","0613","0614","0615","0616","0617","0618","0619","0620","0621","0622","0623","0624","0625","0626","0627","0628","0629","0630","0688","0701","0702","0703","0704","0705","0706","0788","0801","0802","0803","0804","0805","0806","0807","0808","0809","0810","0811","0812","0813","0814","0815","0816","0817","0818","0819","0820","0821","0822","0823","0888","0901","1001","1002","1003","1004","1005","1006","1007","1008","1009","1010","1011","1012","1088","1101","1102","1103","1104","1188","1201","1288","1301","1302","1388","1401","1402","1403","1488","1501","1502","1503","1588","1601","1602","1603","1604","1605","1688","1701","1702","1703","1704","1788","1801","1802","1803","1804","1888","1901","1902","1988","5999","9999"),
  Crop_name = c("Paddy","Jowar","Bajra","Maize","Ragi","Wheat","Barley","Small Millets","Other Cereals","Gram","Tur (Arhar)","Urad","Moong","Masur","Horse Gram","Beans (Pulses)","Peas (Pulses)","Other Pulses","Sugarcane","Palmvriah","Other Sugar Crops","Pepper (Black)","Chillies","Ginger","Turmeric","Cardamom (Small)","Cardamom (Large)","Betel Nuts (Areca Nuts)","Garlic","Coriander","Tamarind","Cumin Seed","Fennel I Anise Seed","Nutmeg","Fenugreek","Cloves","Cinnamon","Cocoa","Kacholam","Betelvine","Other Condi & Spices","Mangoes","Orange And Kinu","Mosambi","Lemon I Acid Lime","Other Citrus Fruits","Banana","Table Grapes","Wine Grapes (Black)","Apple","Pear","Peaches","Plum","Kiwi Fruit","Chiku","Papaya","Guava","Almond","Walnut","Cashew Nuts","Apricot","Jackfruit","Lichi","Pineapple","Watermelon","Muskmelon","Bread Fruits","Ber","Be!","Mulberry (Sahatoot)","Aonla (Amla)","Other Fruits","Potato","Tapioca (Cassava)","Sweet Potato","Yam","Elephant Foot Yam","Colocasia/Arum","Other Tuber Crop","Onion","Carrot","Radish","Beetroot","Turnip (Shalgam)","Tomato","Spinach","Amaranths (Chaulai)","Cabbage","Other Leafy Vegetable","Brinjal","Peas (Vegetable) (Green)","Lady'S Finger (Bhindi)","Cauliflower","Cucumber","Bottle Gourd (Lauki)","Pumpkin","Bitter Gourd","Other Gourds","Guar Bean S (Cluster Beans)","Beans (Green)","Drumstick","Green Chillies","Other Vegetables","Other Food Crop","Ground Nut","Castor Seed","Sesamum (Til)","Rapeseed & Mustard","Linseed","Coconut","Sunflower","Safflower","Soyabean","Niger Seed","Oil Palm","Toria","Other Oilseeds","Cotton","Jute","Mesta","Sunhemp","Other Fibres","Indigo","Other Dyes & Tan Materials","Opium","Tobacco","Other Drugs & Narcotics","Guar","Oats","Green Manures","Other Fodder Crops","Tea","Coffee","Rubber","Other Plantation Crops","Orchids","Rose","Gladiolus","Carnation","Marigold","Other Flowers","Asgandh","Isabgol","Sena","Moosli","Other Medicinal Plant","Lemon Grass","Mint","Menthol","Eucalyptus","Other Aromatic Plants","Canes","Bamboos","Other Non-Food Crops","Others","All"),
  Crop_category = c("1","1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","2","2","4","4","4","5","5","5","5","5","5","5","5","5","5","5","5","5","5","5","5","5","5","5","5","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","6","7","7","7","7","7","7","7","8","8","8","8","8","8","8","8","8","8","8","8","8","8","8","8","8","8","8","8","8","8","8","8","8","10","10","10","10","10","10","10","10","10","10","10","10","10","11","11","11","11","11","12","12","13","13","13","14","14","14","14","15","15","15","15","16","16","16","16","16","16","17","17","17","17","17","18","18","18","18","18","19","19","19","NA","NA"),
  Crop_category_name = c("Cereals","Cereals","Cereals","Cereals","Cereals","Cereals","Cereals","Cereals","Cereals","Pulses","Pulses","Pulses","Pulses","Pulses","Pulses","Pulses","Pulses","Pulses","Sugar Crops","Sugar Crops","Sugar Crops","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Condiments and Spices","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Fruits","Tuber Crops","Tuber Crops","Tuber Crops","Tuber Crops","Tuber Crops","Tuber Crops","Tuber Crops","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Vegetables","Oilseeds","Oilseeds","Oilseeds","Oilseeds","Oilseeds","Oilseeds","Oilseeds","Oilseeds","Oilseeds","Oilseeds","Oilseeds","Oilseeds","Oilseeds","Fibres","Fibres","Fibres","Fibres","Fibres","Dyes & Tanning materials","Dyes & Tanning materials","Drugs & narcotics","Drugs & narcotics","Drugs & narcotics","Fodder crops","Fodder crops","Fodder crops","Fodder crops","Plantation Crops","Plantation Crops","Plantation Crops","Plantation Crops","Flower crops","Flower crops","Flower crops","Flower crops","Flower crops","Flower crops","Medicinal plants","Medicinal plants","Medicinal plants","Medicinal plants","Medicinal plants","Aromatic plants","Aromatic plants","Aromatic plants","Aromatic plants","Aromatic plants","Other non-food crops","Other non-food crops","Other non-food crops","NA","NA"),
  Crop_msp_status = c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","No","No","Yes","Yes","Yes","Yes","Yes","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","Yes","No","Yes","Yes","No","Yes","Yes","Yes","Yes","Yes","No","Yes","No","Yes","Yes","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No")
  )

#View(V1L16)
#View(Crop_codes_master)
V1L16 <- left_join(V1L16,Crop_codes_master, c("V1L16Q05_crop_code_as_in_col_2_of_block_6"="Crop_code"))


V1_merged <- merge(V1_merged,V1L16, by="HHID")
#View(V1_merged)
V1_merged <- merge(V1_merged,Simpson_index,by="HHID")
#View(V1_merged)



#Preparing data for regression
V1_merged_subset_one <- V1_merged %>%
  select(HHID,
         State_name,
         V1L02Q04_gender,
         V1L02Q05_age,
         Education_collapsed,
         V1L16Q07_aware_of_msp_of_this_crop,
         Simpson_index_score
         ) %>%
  group_by(HHID,
           State_name,
           V1L02Q04_gender,
           V1L02Q05_age,
           Education_collapsed,
           Simpson_index_score
           ) %>%
  summarise(
    V1L16Q07_aware_of_msp_of_this_crop=max(V1L16Q07_aware_of_msp_of_this_crop)
    )
  
#View(V1_merged_subset_one)



#Summary statistics / visualisations of the data

# 
# #Histogram of age
# ggplot(data=V1_merged_subset_one) +
#   geom_histogram(aes(V1L02Q05_age, fill=V1L02Q04_gender),colour="white",na.rm=TRUE)+
#   scale_y_continuous() +
#   scale_x_continuous(breaks=seq(20, 100, by=10))+
#   labs(title = "A histogram of the age of the head of households of respondents",
#        caption = "source: NSSO data",
#        x="Age -->",
#        y="The number of respondents",
#        fill="") +
#   scale_colour_brewer(palette = "RdPu")+
#   theme_minimal(base_size = 10)+
#   theme(legend.position = "bottom",
#         axis.title.x = element_text(colour = "#848494"),
#         axis.title.y = element_text(colour = "#848494")
#         #text=element_text(color="#0a02f0"),axis.text=element_text(color="#aeb3a4")
#   )
# 
# 
# #MSP awareness and education
# ggplot(data=V1_merged_subset_one) +
#   geom_col(aes(x=Education_collapsed,
#                y=V1L16Q07_aware_of_msp_of_this_crop,
#                fill=V1L02Q04_gender),na.rm=TRUE)+
#   #scale_y_continuous() +
#   #scale_x_continuous(breaks=seq(20, 100, by=10))+
#   labs(title = "Education profile of the respondents",
#        caption = "source: NSSO data",
#        x="Highest level of education",
#        y="The number of respondents",
#        fill="") +
#   scale_colour_brewer(palette = "PuOr")+
#   theme_minimal(base_size = 10)+
#   theme(legend.position = "bottom",
#         axis.title.x = element_text(colour = "#848494"),
#         axis.title.y = element_text(colour = "#848494")
#         #text=element_text(color="#0a02f0"),axis.text=element_text(color="#aeb3a4")
#   )
# 


#Attempting an OLS regression with Simpson Index Score as the dependent variable
OLS_regression_one <- lm(Simpson_index_score ~
                           V1L16Q07_aware_of_msp_of_this_crop +
                               V1L02Q04_gender +
                               V1L02Q05_age +
                               Education_collapsed,
                             data=V1_merged_subset_one)

summary(OLS_regression_one)
OLS_regression_one_output <- tidy(OLS_regression_one, conf.int = T) %>%
  mutate_if(is.numeric, round, digits = 2)

OLS_regression_one_chart <- coefplot(OLS_regression_one, 
                                     sort = "magnitude", intercept = FALSE) +
  theme_minimal(base_size = 10) +
  labs(title = "OLS Regression of Simpson's Index Score on MSP awareness")

OLS_regression_one_chart




#Attempting an OLS regression with Simpson Index Score as the dependent variable, and state added as control
OLS_regression_two <- lm(Simpson_index_score ~
                           V1L16Q07_aware_of_msp_of_this_crop +
                           V1L02Q04_gender +
                           V1L02Q05_age +
                           State_name +
                           Education_collapsed,
                         data=V1_merged_subset_one)

summary(OLS_regression_two)
OLS_regression_two_output <- tidy(OLS_regression_two, conf.int = T) %>%
  mutate_if(is.numeric, round, digits = 2)
OLS_regression_two_output$term <- prefix_strip(OLS_regression_two_output$term,"State_name")
#OLS_regression_two_output
#View(OLS_regression_two_output)
#write.csv(OLS_regression_two_output,"RegTwo.csv", row.names = FALSE)
OLS_regression_two_chart <- coefplot(OLS_regression_two, 
                                     sort = "magnitude",
                                     intercept = FALSE) +
  theme_minimal(base_size = 10) +
  labs(title = "OLS Regression of Simpson's Index Score on MSP awareness", 
       subtitle = "State added as a control")

OLS_regression_two_chart



#Attempting heckman selection model using the sampleSelection package. The methodology is taken from here https://m-clark.github.io/models-by-example/heckman-selection.html
Heckman_selection_one <- selection(V1L16Q07_aware_of_msp_of_this_crop ~ 
                                     Education_collapsed + 
                                     V1L02Q05_age,
                                   Simpson_index_score ~ 
                                     V1L02Q04_gender + 
                                     V1L02Q05_age + 
                                     Education_collapsed, 
                                   data = V1_merged_subset_one )
#summary(Heckman_selection_one)
Heckman_selection_one_output <- tidy(Heckman_selection_one, conf.int = T) %>%
  mutate_if(is.numeric, round, digits = 2)
#View(Heckman_selection_one_output)
#write.csv(Heckman_selection_one_output,"Heckman_one.csv", row.names = FALSE)


#Preparing a second set of data for regression
V1_merged_subset_two <- V1_merged %>%
  arrange() %>%
  select(HHID,
         State_name,
         V1L02Q04_gender,
         V1L02Q05_age,
         Education_collapsed,
         V1L16Q07_aware_of_msp_of_this_crop,
         Simpson_index_score
  ) %>%
  group_by(HHID,
           State_name,
           V1L02Q04_gender,
           V1L02Q05_age,
           Education_collapsed,
           Simpson_index_score
  ) %>%
  summarise(
    V1L16Q07_aware_of_msp_of_this_crop=max(V1L16Q07_aware_of_msp_of_this_crop)
  )
