import pandas as pd
import datatable as dt
import time

start_time = time.time()

### 2019 FIRST QUARTER
df1 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Siniestros2019Literales1T.csv")
df1.sort('fecha Siniestro Acto')
df1_pandas = df1.to_pandas()
df1_pandas = df1_pandas[df1_pandas['Identificador CIS'] != 0]
df1_pandas['WeekNum'] = df1_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df1_pandas['Year'] = 2019
df1_pandas['AgeGroup'] = pd.cut(df1_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df1_grouped = df1_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df1, df1_pandas]]

### 2019 SECOND QUARTER
df2 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Siniestros2019Literales2T.csv")
df2.sort('fecha Siniestro Acto')
df2_pandas = df2.to_pandas()
df2_pandas = df2_pandas[df2_pandas['Identificador CIS'] != 0]
df2_pandas['WeekNum'] = df2_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df2_pandas['AgeGroup'] = pd.cut(df2_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df2_pandas['Year'] = 2019
df2_grouped = df2_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df2, df2_pandas]]

### 2019 THIRD QUARTER
df3 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Siniestros2019Literales3T.csv")
df3.sort('fecha Siniestro Acto')
df3_pandas = df3.to_pandas()
df3_pandas = df3_pandas[df3_pandas['Identificador CIS'] != 0]
df3_pandas['WeekNum'] = df3_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df3_pandas['Year'] = 2019
df3_pandas['AgeGroup'] = pd.cut(df3_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df3_grouped = df3_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df3, df3_pandas]]

### 2019 FOURTH QUARTER
df4 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Siniestros2019Literales4T.csv")
df4.sort('fecha Siniestro Acto')
df4_pandas = df4.to_pandas()
df4_pandas = df4_pandas[df4_pandas['Identificador CIS'] != 0]
df4_pandas['WeekNum'] = df4_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df4_pandas['Year'] = 2019
df4_pandas['AgeGroup'] = pd.cut(df4_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df4_grouped = df4_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df4, df4_pandas]]

### 2020 (MONTHLY)
df5 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2020/SiniestrosUAC2020ENE.csv")
df5.sort('fecha Siniestro Acto')
df5_pandas = df5.to_pandas()
df5_pandas = df5_pandas[df5_pandas['Identificador CIS'] != 0]
df5_pandas['WeekNum'] = df5_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df5_pandas['Year'] = 2020
df5_pandas['AgeGroup'] = pd.cut(df5_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df5_grouped = df5_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df5, df5_pandas]]

df6 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2020/SiniestrosUAC2020FEB.csv")
df6.sort('fecha Siniestro Acto')
df6_pandas = df6.to_pandas()
df6_pandas = df6_pandas[df6_pandas['Identificador CIS'] != 0]
df6_pandas['WeekNum'] = df6_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df6_pandas['Year'] = 2020
df6_pandas['AgeGroup'] = pd.cut(df6_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df6_grouped = df6_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df6, df6_pandas]]

df7 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2020/SiniestrosUAC2020MAR.csv")
df7.sort('fecha Siniestro Acto')
df7_pandas = df7.to_pandas()
df7_pandas = df7_pandas[df7_pandas['Identificador CIS'] != 0]
df7_pandas['WeekNum'] = df7_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df7_pandas['Year'] = 2020
df7_pandas['AgeGroup'] = pd.cut(df7_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df7_grouped = df7_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df7, df7_pandas]]

df8 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2020/SiniestrosUAC2020ABR.csv")
df8.sort('fecha Siniestro Acto')
df8_pandas = df8.to_pandas()
df8_pandas = df8_pandas[df8_pandas['Identificador CIS'] != 0]
df8_pandas['WeekNum'] = df8_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df8_pandas['Year'] = 2020
df8_pandas['AgeGroup'] = pd.cut(df8_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df8_grouped = df8_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df8, df8_pandas]]

df9 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2020/SiniestrosUAC2020MAY.csv")
df9.sort('fecha Siniestro Acto')
df9_pandas = df9.to_pandas()
df9_pandas = df9_pandas[df9_pandas['Identificador CIS'] != 0]
df9_pandas['WeekNum'] = df9_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df9_pandas['Year'] = 2020
df9_pandas['AgeGroup'] = pd.cut(df9_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df9_grouped = df9_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df9, df9_pandas]]

df10 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2020/SiniestrosUAC2020JUN.csv")
df10.sort('fecha Siniestro Acto')
df10_pandas = df10.to_pandas()
df10_pandas = df10_pandas[df10_pandas['Identificador CIS'] != 0]
df10_pandas['WeekNum'] = df10_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df10_pandas['Year'] = 2020
df10_pandas['AgeGroup'] = pd.cut(df10_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df10_grouped = df10_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df10, df10_pandas]]

df11 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2020/SiniestrosUAC2020JUL.csv")
df11.sort('fecha Siniestro Acto')
df11_pandas = df11.to_pandas()
df11_pandas = df11_pandas[df11_pandas['Identificador CIS'] != 0]
df11_pandas['WeekNum'] = df11_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df11_pandas['Year'] = 2020
df11_pandas['AgeGroup'] = pd.cut(df11_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df11_grouped = df11_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df11, df11_pandas]]

df12 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2020/SiniestrosUAC2020AGO.csv")
df12.sort('fecha Siniestro Acto')
df12_pandas = df12.to_pandas()
df12_pandas = df12_pandas[df12_pandas['Identificador CIS'] != 0]
df12_pandas['WeekNum'] = df12_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df12_pandas['Year'] = 2020
df12_pandas['AgeGroup'] = pd.cut(df12_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df12_grouped = df12_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df12, df12_pandas]]

df13 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2020/SiniestrosUAC2020SEP.csv")
df13.sort('fecha Siniestro Acto')
df13_pandas = df13.to_pandas()
df13_pandas = df13_pandas[df13_pandas['Identificador CIS'] != 0]
df13_pandas['WeekNum'] = df13_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df13_pandas['Year'] = 2020
df13_pandas['AgeGroup'] = pd.cut(df13_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df13_grouped = df13_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df13, df13_pandas]]

df14 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2020/SiniestrosUAC2020OCT.csv")
df14.sort('fecha Siniestro Acto')
df14_pandas = df14.to_pandas()
df14_pandas = df14_pandas[df14_pandas['Identificador CIS'] != 0]
df14_pandas['WeekNum'] = df14_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df14_pandas['Year'] = 2020
df14_pandas['AgeGroup'] = pd.cut(df14_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df14_grouped = df14_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df14, df14_pandas]]

df15 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2020/SiniestrosUAC2020NOV.csv")
df15.sort('fecha Siniestro Acto')
df15_pandas = df15.to_pandas()
df15_pandas = df15_pandas[df15_pandas['Identificador CIS'] != 0]
df15_pandas['WeekNum'] = df15_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df15_pandas['Year'] = 2020
df15_pandas['AgeGroup'] = pd.cut(df15_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df15_grouped = df15_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df15, df15_pandas]]

df16 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2020/SiniestrosUAC2020DIC.csv")
df16.sort('fecha Siniestro Acto')
df16_pandas = df16.to_pandas()
df16_pandas = df16_pandas[df16_pandas['Identificador CIS'] != 0]
df16_pandas['WeekNum'] = df16_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df16_pandas['Year'] = 2020
df16_pandas['AgeGroup'] = pd.cut(df16_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df16_grouped = df16_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df16, df16_pandas]]

### 2021 (MONTHLY)
df17 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2021/SiniestrosUAC2021ENE.csv")
df17.sort('fecha Siniestro Acto')
df17_pandas = df17.to_pandas()
df17_pandas = df17_pandas[df17_pandas['Identificador CIS'] != 0]
df17_pandas['WeekNum'] = df17_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df17_pandas['Year'] = 2021
df17_pandas['AgeGroup'] = pd.cut(df17_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df17_grouped = df17_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df17, df17_pandas]]

df18 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2021/SiniestrosUAC2021FEB.csv")
df18.sort('fecha Siniestro Acto')
df18_pandas = df18.to_pandas()
df18_pandas = df18_pandas[df18_pandas['Identificador CIS'] != 0]
df18_pandas['WeekNum'] = df18_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df18_pandas['Year'] = 2021
df18_pandas['AgeGroup'] = pd.cut(df18_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df18_grouped = df18_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df18, df18_pandas]]

df19_1 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2021/SiniestrosUAC2021MAR1.txt")
df19_1.sort('fecha Siniestro Acto')
df19_1_pandas = df19_1.to_pandas()
df19_1_pandas = df19_1_pandas[df19_1_pandas['Identificador CIS'] != 0]
df19_1_pandas['fecha Siniestro Acto'] = pd.to_datetime(df19_1_pandas['fecha Siniestro Acto'], format='%d/%m/%Y %H:%M')
df19_1_pandas['WeekNum'] = df19_1_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df19_1_pandas['Year'] = 2021
df19_1_pandas['AgeGroup'] = pd.cut(df19_1_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df19_1_grouped = df19_1_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df19_1, df19_1_pandas]]

df19_2 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2021/SiniestrosUAC2021MAR2.txt")
df19_2.sort('fecha Siniestro Acto')
df19_2_pandas = df19_2.to_pandas()
df19_2_pandas = df19_2_pandas[df19_2_pandas['Identificador CIS'] != 0]
df19_2_pandas['fecha Siniestro Acto'] = pd.to_datetime(df19_2_pandas['fecha Siniestro Acto'], format='%d/%m/%Y %H:%M')
df19_2_pandas['WeekNum'] = df19_2_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df19_2_pandas['Year'] = 2021
df19_2_pandas['AgeGroup'] = pd.cut(df19_2_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df19_2_grouped = df19_2_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df19_2, df19_2_pandas]]

df20 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2021/SiniestrosUAC2021ABR.csv")
df20.sort('fecha Siniestro Acto')
df20_pandas = df20.to_pandas()
df20_pandas = df20_pandas[df20_pandas['Identificador CIS'] != 0]
df20_pandas['WeekNum'] = df20_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df20_pandas['Year'] = 2021
df20_pandas['AgeGroup'] = pd.cut(df20_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df20_grouped = df20_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df20, df20_pandas]]

df21 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2021/SiniestrosUAC2021MAY.csv")
df21.sort('fecha Siniestro Acto')
df21_pandas = df21.to_pandas()
df21_pandas = df21_pandas[df21_pandas['Identificador CIS'] != 0]
df21_pandas['WeekNum'] = df21_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df21_pandas['Year'] = 2021
df21_pandas['AgeGroup'] = pd.cut(df21_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df21_grouped = df21_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df21, df21_pandas]]

df22 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2021/SiniestrosUAC2021JUN.csv")
df22.sort('fecha Siniestro Acto')
df22_pandas = df22.to_pandas()
df22_pandas = df22_pandas[df22_pandas['Identificador CIS'] != 0]
df22_pandas['WeekNum'] = df22_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df22_pandas['Year'] = 2021
df22_pandas['AgeGroup'] = pd.cut(df22_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df22_grouped = df22_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df22, df22_pandas]]

df23 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2021/SiniestrosUAC2021JUL.csv")
df23.sort('fecha Siniestro Acto')
df23_pandas = df23.to_pandas()
df23_pandas = df23_pandas[df23_pandas['Identificador CIS'] != 0]
df23_pandas['WeekNum'] = df23_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df23_pandas['Year'] = 2021
df23_pandas['AgeGroup'] = pd.cut(df23_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df23_grouped = df23_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df23, df23_pandas]]

df24 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2021/SiniestrosUAC2021AGO.csv")
df24.sort('fecha Siniestro Acto')
df24_pandas = df24.to_pandas()
df24_pandas = df24_pandas[df24_pandas['Identificador CIS'] != 0]
df24_pandas['WeekNum'] = df24_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df24_pandas['Year'] = 2021
df24_pandas['AgeGroup'] = pd.cut(df24_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df24_grouped = df24_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df24, df24_pandas]]

df25 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2021/SiniestrosUAC2021SEP.csv")
df25.sort('fecha Siniestro Acto')
df25_pandas = df25.to_pandas()
df25_pandas = df25_pandas[df25_pandas['Identificador CIS'] != 0]
df25_pandas['WeekNum'] = df25_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df25_pandas['Year'] = 2021
df25_pandas['AgeGroup'] = pd.cut(df25_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df25_grouped = df25_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df25, df25_pandas]]

df26 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2021/SiniestrosUAC2021OCT.csv")
df26.sort('fecha Siniestro Acto')
df26_pandas = df26.to_pandas()
df26_pandas = df26_pandas[df26_pandas['Identificador CIS'] != 0]
df26_pandas['WeekNum'] = df26_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df26_pandas['Year'] = 2021
df26_pandas['AgeGroup'] = pd.cut(df26_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df26_grouped = df26_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df26, df26_pandas]]

df27_1 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2021/SiniestrosUAC2021NOV1.txt")
df27_1.sort('fecha Siniestro Acto')
df27_1_pandas = df27_1.to_pandas()
df27_1_pandas = df27_1_pandas[df27_1_pandas['Identificador CIS'] != 0]
df27_1_pandas['fecha Siniestro Acto'] = pd.to_datetime(df27_1_pandas['fecha Siniestro Acto'], format='%d/%m/%Y %H:%M')
df27_1_pandas['WeekNum'] = df27_1_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df27_1_pandas['Year'] = 2021
df27_1_pandas['AgeGroup'] = pd.cut(df27_1_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df27_1_grouped = df27_1_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df27_1, df27_1_pandas]]

df27_2 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2021/SiniestrosUAC2021NOV2.txt")
df27_2.sort('fecha Siniestro Acto')
df27_2_pandas = df27_2.to_pandas()
df27_2_pandas = df27_2_pandas[df27_2_pandas['Identificador CIS'] != 0]
df27_2_pandas['fecha Siniestro Acto'] = pd.to_datetime(df27_2_pandas['fecha Siniestro Acto'], format='%d/%m/%Y %H:%M')
df27_2_pandas['WeekNum'] = df27_2_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df27_2_pandas['Year'] = 2021
df27_2_pandas['AgeGroup'] = pd.cut(df27_2_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df27_2_grouped = df27_2_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df27_2, df27_2_pandas]]

df28 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Año 2021/SiniestrosUAC2021DIC.csv")
df28.sort('fecha Siniestro Acto')
df28_pandas = df28.to_pandas()
df28_pandas = df28_pandas[df28_pandas['Identificador CIS'] != 0]
df28_pandas['WeekNum'] = df28_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df28_pandas['Year'] = 2021
df28_pandas['AgeGroup'] = pd.cut(df28_pandas['Número Edad'], pd.IntervalIndex.from_tuples([(0, 17), (17, 39), (39, 59), (59, 200)]))
df28_grouped = df28_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo", "AgeGroup"], observed=True)["Unidades Acto"].sum()
del [[df28, df28_pandas]]

### MERGE THE FILES AND GET TIME SERIES FORMAT.
df_append = pd.concat([df1_grouped, df2_grouped, df3_grouped, df4_grouped, df5_grouped, df6_grouped, df7_grouped,
                       df8_grouped, df9_grouped, df10_grouped, df11_grouped, df12_grouped, df13_grouped, df14_grouped,
                       df15_grouped, df16_grouped, df17_grouped, df18_grouped, df19_1_grouped, df19_2_grouped, 
                       df20_grouped, df21_grouped, df22_grouped, df23_grouped, df24_grouped, df25_grouped, 
                       df26_grouped, df27_1_grouped, df27_2_grouped, df28_grouped])
del [[df1_grouped, df2_grouped, df3_grouped, df4_grouped, df5_grouped, df6_grouped, df7_grouped,
                       df8_grouped, df9_grouped, df10_grouped, df11_grouped, df12_grouped, df13_grouped, df14_grouped,
                       df15_grouped, df16_grouped, df17_grouped, df18_grouped, df19_1_grouped, df19_2_grouped, 
                       df20_grouped, df21_grouped, df22_grouped, df23_grouped, df24_grouped, df25_grouped, 
                       df26_grouped, df27_1_grouped, df27_2_grouped, df28_grouped]]

### EXPORT DATA IN TIME SERIES FORMAT
df_append.to_csv(r'/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/MAPFRE_Weekly_data.csv', index=True, header=True)
print("--- %s seconds ---" % (time.time() - start_time))