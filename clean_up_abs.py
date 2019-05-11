# This script is used to clean the data obtained from the ABS. The goals is to make the data set as small as possible and allowing for easy joining with other data sets. Clean up activities such as:
# - dropping columns with duplicate data; an example of duplication is a state code and state columns (they're perfectly correlated)
# - removing the parentheses and their contents from the LGA names, e.g. Albury (C) → Albury. This allows for easier joining with other data sets
# - removing rows with "Total" values, which are aggregations of a group of values. Grouped values can be calculated as needed in the EDA

import pandas as pd
import os
import re

root = r"D:\OneDrive - UTS\36103\AT2\Data"
dir_country_of_birth = os.path.join(root, 'Country of Birth')
dir_age = os.path.join(root, 'Age')
dir_employment = os.path.join(root, 'Employment')
dir_labour = os.path.join(root, 'Labour')
os.chdir(root)
os.getcwd()

# =============================================================================
# Regex for removing parentheses and the content
# =============================================================================
pattern = r"\s*\(.*\)\s*"

# =============================================================================
# Join the batches for
# ABS_C16_T07_LGA
# Country of Birth
# =============================================================================
dir_country_of_birth = os.path.join(root, 'Country of Birth')
os.listdir(dir_country_of_birth)

batch_paths = [os.path.join(dir_country_of_birth, filename) for filename in os.listdir(dir_country_of_birth) if 'csv' in filename and 'ABS' in filename]

df_country_of_birth = pd.DataFrame()
for path in batch_paths:
    df_country_of_birth = pd.concat([df_country_of_birth, pd.read_csv(path)])
    
df_country_of_birth.head()
df_country_of_birth.info()

# Clean up

# Drop unused columns
drop_cols = ['BPLP_2016', 
             'STATE',
             'State',
             'REGIONTYPE',
             'Geography Level',
             'TIME',
             'Flag Codes',
             'Flags']
df_country_of_birth.drop(drop_cols, axis = 'columns', inplace = True)
df_country_of_birth.info()

# Remove the rows where Birthplace of Person == "Total"
df_country_of_birth.drop(df_country_of_birth[df_country_of_birth['Birthplace of Person'] == "Total"].index,
                         inplace = True)
df_country_of_birth.info()

# Remove the rows where YARRP_2016 == "Tot"
df_country_of_birth.drop(df_country_of_birth[df_country_of_birth['YARRP_2016'] == "TOT"].index,
                         inplace = True)
df_country_of_birth.info()

# Remove the parentheses and contents
df_country_of_birth['Region'] = df_country_of_birth['Region'].str.replace(pat = pattern, repl = '', regex = True)

# Make headers all lower case and replace space with underscore
df_country_of_birth.columns = [column.replace(' ', '_').lower() for column in df_country_of_birth.columns]

# Save to disk
df_country_of_birth.to_csv(os.path.join(dir_country_of_birth, 'country_of_birth.csv'),
                           index = False)

# =============================================================================
# Clean up 
# Census 2016, Age by Sex (LGA)
# =============================================================================
df_age = pd.read_csv(os.path.join(dir_age, 'ABS_C16_G43_LGA_10052019094928026.csv'))
df_age.info()
drop_cols_age = ['AGE', 
                 'SEX_ABS',
                 'LFSP_C16',
                 'STATE',
                 'REGIONTYPE',
                 'Geography Level',
                 'TIME',
                 'Flag Codes',
                 'Flags']

# Drop unused columns
df_age.drop(drop_cols_age, axis = 'columns', inplace = True)
df_age.info()

# Drop where Age == "Total"
df_age.drop(df_age[df_age['Age'] == "Total"].index, inplace = True)
df_age.info()

# Remove the parentheses and contents from the LGA names
df_age['Region'] = df_age['Region'].str.replace(pat = pattern, repl = '', regex = True)

# Save to disk
df_age.to_csv(os.path.join(dir_age, 'age.csv'),
                           index = False)

# =============================================================================
# Clean up Employment
# Census 2016, G51 Industry of employment by age by sex (LGA)
# =============================================================================
df = pd.read_csv(os.path.join(dir_employment, 'ABS_C16_G51_LGA_10052019142250807.csv'))

df.info()
drop_cols = ['AGE', 
                 'SEX_ABS',
                 'INDP_C16',
                 'STATE',
                 'State',
                 'REGIONTYPE',
                 'Geography Level',
                 'TIME',
                 'Flag Codes',
                 'Flags']

# Drop unused columns
df.drop(drop_cols, axis = 'columns', inplace = True)
df.info()

# Drop where Age == "Total"
col_name_to_filter = 'Industry of Employment'
col_value_to_filter = 'Total'
df.drop(df[df[col_name_to_filter] == col_value_to_filter].index, inplace = True)
df.info()

# Remove the parentheses and contents from the LGA names
df['Region'] = df['Region'].str.replace(pat = pattern, repl = '', regex = True)

# Save to disk
output_filename = 'industry_employment.csv'
df.to_csv(os.path.join(dir_employment, output_filename),
                           index = False)

# Copy variable
df_ind_emp = df

# =============================================================================
# Clean up Labour
# Census 2016, G43 Labour force status by age by sex (LGA) 
# =============================================================================
df = pd.read_csv(os.path.join(dir_labour, 'ABS_C16_G43_LGA_10052019100820914.csv'))

df.info()
drop_cols = ['AGE', 
             'SEX_ABS',
             'LFSP_C16',
             'STATE',
             'State',
             'REGIONTYPE',
             'Geography Level',
             'TIME',
             'Flag Codes',
             'Flags']

# Drop unused columns
df.drop(drop_cols, axis = 'columns', inplace = True)
df.info()

# Drop where Labour force status contains "Total"
col_name_to_filter = 'Labour force status'
col_value_to_filter = 'Total'

mask = ~df[col_name_to_filter].str.contains(col_value_to_filter)
df = df[mask]

# index_to_drop = df[df[col_name_to_filter] == col_value_to_filter].index
#df.drop(index_to_drop, inplace = True)

df.info()

# Remove the parentheses and contents from the LGA names
df['Region'] = df['Region'].str.replace(pat = pattern, repl = '', regex = True)

# Save to disk
output_filename = 'labour.csv'
dir_output = dir_labour

df.to_csv(os.path.join(dir_output, output_filename),
                           index = False)

# Copy variable
df_labour = df

