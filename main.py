import pandas as pd

# Load data from CSV file
data = pd.read_csv('data/data_90.csv')
soil_moi = data[['soil_moi']]
print(soil_moi)

import matplotlib.pyplot as plt


plt.plot(soil_moi.index, soil_moi['soil_moi'])
plt.xlabel('Time')
plt.ylabel('Soil Moisture (%)')
plt.title('Soil moisture over time')
plt.show()

import pyEDM as edm
soil_moi = pd.DataFrame(soil_moi)
# Ensure the 'soil_moi' column is numeric
soil_moi['soil_moi'] = pd.to_numeric(soil_moi['soil_moi'], errors='coerce')
# check if the data is numeric
print(soil_moi.dtypes)

# Create a simplex projection
simplex_rs=edm.Simplex(
    dataFrame = soil_moi,
    lib = '1 50',
    pred = '51 90',
    E = 3,
    Tp = 1,
    columns = 'soil_moi',
    target = 'soil_moi',
    showPlot = True,
    noTime = True
)
# print the results of the simplex projection
print(simplex_rs)
# Create an S-Map projection
smap_rs = edm.SMap(
    dataFrame = soil_moi,
    lib = '1 50',
    pred = '51 90',
    E = 3,
    Tp = 1,
    theta = 3,
    columns = 'soil_moi',
    target = 'soil_moi',
    showPlot = True,
    noTime = True
)
# Print the results of the S-Map projection
print(smap_rs)
