import eikon as ek  # the Eikon Python wrapper package
import numpy as np  # NumPy
import pandas as pd  # pandas
import cufflinks as cf  # Cufflinks
import configparser as cp
import pandas as pd
import datetime as dt
import dateutil.relativedelta
import eikon as ek

# Connect to the Eikon API
# This requires a local version of the API proxy running.
# [Eikon API information can be found here](https://developers.refinitiv.com/eikon-apis/eikon-data-api)
# 
# * For students use: 
# * Username: qmseikon3@qub.ac.uk
# * password: tY69e66x6WXX
# 
# 
# * For staff use:
# * Username: qmseikon2@qub.ac.uk         
# * Password: QmsStaffMember185

# Step on turn on API then connect using API

ek.set_app_id("7d75f6e45b9143ae80d3456648399a429e9391f0")
