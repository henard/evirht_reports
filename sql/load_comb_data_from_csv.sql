LOAD DATA LOCAL INFILE '/media/sf_share/Documents/Select/Thrive/In/TOL Individual Profile Data Sample for SSS2.csv' INTO TABLE comb_data
  FIELDS TERMINATED BY ','
  LINES TERMINATED BY '\r\n'
  IGNORE 1 LINES
(Organisation,
Account_ID,
Child_ID,
Status,
@var1,
School_Year,
Age,
Gender,
Profile_ID,
@var2,
Profile_Type,
Dev_Stage,
Being_Special,
Having_Needs_Met,
Being_Safe,
Explore_Safely,
Exploring_and_Experimenting,
Experiencing_Options,
Expressing_a_view,
Learning_about_Cause_and_Effect,
Feeling_Thinking_and_Problem_solving,
Developing_an_identity,
Fantasy_and_Reality,
Power_with_Responsibility,
Motivation_for_Developing_Skills,
Morals_and_Values,
Understanding_the_need_for_Rules,
Establishing_Independence,
Connecting_with_Peers,
Exploring_Identity_and_Gender,
Initiating_Engaging_and_Doing,
Overall_Score)
set
Child_DOB = STR_TO_DATE(@var1, '%d/%m/%Y'),
Completed_Date = STR_TO_DATE(@var2, '%d/%m/%Y %h:%i');

