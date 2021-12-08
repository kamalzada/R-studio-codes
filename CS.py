# -*- coding: utf-8 -*-
"""
Created on Sat Dec  4 12:41:37 2021

@author: Vusal Kamalzada
"""

import pandas as pd
import streamlit as st
import plotly.express as px
from PIL import Image
import os as os
import pandas as pd
import numpy as np
from sklearn.impute import SimpleImputer
import imblearn

main_image = Image.open("banner.jpg")  
dsa_image = Image.open("logo.png")
icon = Image.open("icon.png")

st.set_page_config(layout = "wide", page_title = "Week 7 CS", page_icon=icon)
st.title("Streamlit CaseStudy")

st.sidebar.image(dsa_image, use_column_width="always")
menu = st.sidebar.selectbox("Select page:", ["Homepage", "Data Preparation", "Data Modeling"])
        