# -*- coding: utf-8 -*-
"""

@author: Runjin Li

"""

import numpy as np
import pandas as pd
import os
from sklearn.linear_model import LinearRegression
from linearmodels import PanelOLS
from linearmodels import RandomEffects
import statsmodels.api as sm

# Set path.
path1 = r'C:\Users\linst\GitHub\adb-winter-2022\code\analysis'
path2 = r'C:\Users\linst\GitHub\adb-winter-2022\data\final'

# Load data.
merged = 'merged_data.csv'
df_adb = pd.read_csv(os.path.join(path2, merged))
df_adb.head()
print(df_adb)

# Fixed Effect Regression.
multi = df_adb.set_index(['year','iso']).sort_index()
multi.reset_index()
# Climate.
climate_vars = ["CCH", "AIR"]
climate = sm.add_constant(df_adb[climate_vars])
Reg_climate = PanelOLS(df_adb['P1: State Legitimacy'], climate, time_effects=True)
Reg_climate2 = PanelOLS(df_adb['P1: State Legitimacy'], climate, entity_effects=True)
Reg_climate
Reg_climate2

# Migration.
migration_vars = ["migrant_stock", 
                  "estimated_refugee_stock_incl_asylum_seekers_both_sexes", 
                  "disaster_stock_displacementr_raw", 
                  "conflict_stock_displacement_raw"]
migration = sm.add_constant(df_adb[migration_vars])
Reg_migration = PanelOLS(df_adb['P1: State Legitimacy'], migration, time_effects=True)
Reg_migration2 = PanelOLS(df_adb['P1: State Legitimacy'], migration, entity_effects=True)
Reg_migration
Reg_migration2

# Governance.
governance_vars = ["value.Rule of Law: Estimate", 
                   "value.Government Effectiveness: Estimate", "corruption_control"]
governance = sm.add_constant(df_adb[governance_vars])
Reg_governance = PanelOLS(df_adb['P1: State Legitimacy'], governance, time_effects=True)
Reg_governance2 = PanelOLS(df_adb['P1: State Legitimacy'], governance, entity_effects=True)
Reg_governance
Reg_governance2