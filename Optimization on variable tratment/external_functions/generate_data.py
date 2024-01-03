"""
Functions that generates data 
made to know the true underlying relations
"""

import pandas as pd
import numpy as np

def generate_data(n=1000,
                  n_kat = 3,
                  m_num = 2,
                  m_kat = 2):
    """
    Function to generate data
    output:
    table with 
    - "y" target column
    - "X_(...)" numeric explanatory varable
    - "X_k_(...)" kategoric explanatory varable
    
    """

    data = pd.DataFrame()
    
    y = pd.Series([0]*n)
    for x in range(m_num):
        data["X_"+str(x)] = np.random.normal(size = n)
        y = y+data["X_"+str(x)]
    
    
    for x in range(m_kat):
        data["X_k_"+str(x)] = np.random.normal(size = n)
        tmp = pd.cut(data["X_k_"+str(x)], n_kat)
        tmp = pd.DataFrame(tmp).merge(tmp.sort_values().drop_duplicates().reset_index(drop = True).reset_index(), how = "left")["index"]
        y = y+tmp*3
    
    y = y+np.random.normal(size = n, scale=0.1)
    data["y"] = y
    
    return data