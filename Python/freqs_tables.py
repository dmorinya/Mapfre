import pandas as pd
import datatable as dt
import time

start_time = time.time()

### 2019 FIRST QUARTER
df1 = dt.fread("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Siniestros2019Literales1T.csv")
df1.sort('fecha Siniestro Acto')
df1_pandas = df1.to_pandas()
df1_pandas['WeekNum'] = df1_pandas['fecha Siniestro Acto'].dt.isocalendar().week
df1_pandas['Year'] = 2019
#df1_pandas['AgeGroup'] = pd.cut(df1_pandas['Número Edad'], [0, 1, 2, 3, 4])
df1_grouped = df1_pandas.groupby(["Year", "WeekNum", "Código Especialidad Realizadora Acto", "Código Especialidad Acto", "Código Grupo Acto", "Identificador Acto Médico", "Código Provincia Realizador Acto", "Código Sexo"], as_index=False)["Unidades Acto"].sum()
list(df1_grouped.columns)
del [[df1, df1_pandas]]

### FREQUENCY TABLES
pr = df1_pandas[["Código Especialidad Realizadora Acto","Nombre Especialidad Realizadora Acto"]]
pr2 = pr.drop_duplicates(subset=["Código Especialidad Realizadora Acto","Nombre Especialidad Realizadora Acto"], keep='last')
pr2.to_csv(r'/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Table1.csv', index=True, header=True)

proveta = df1_pandas["Código Especialidad Realizadora Acto"].value_counts()
proveta.to_csv(r'/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Data/Table2.csv', index=True, header=True)

### UROLOGY
proveta = df1_pandas[(df1_pandas["Código Especialidad Realizadora Acto"]==41) & (df1_pandas["Código Especialidad Acto"]==41)]
proveta["Nombre Grupo Acto"].value_counts()
proveta["Identificador Acto Médico"].value_counts()
pr = df1_grouped[(df1_grouped["Código Especialidad Realizadora Acto"]==41) & (df1_grouped["Código Especialidad Acto"]==41)]
pr["Código Grupo Acto"].value_counts()
pr3 = df1_grouped[(df1_grouped["Código Especialidad Realizadora Acto"]==41) & (df1_grouped["Código Especialidad Acto"]==41) & (df1_grouped["Código Grupo Acto"]=="10100")]
pr3["Identificador Acto Médico"].value_counts()