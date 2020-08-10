import pandas as pd
location="C:\\Users\\vaishakh g d\Desktop\\"
df_genre=pd.read_csv(location+"genre.csv",header=None,delimiter=";")
df_genre.columns=["tid","genre","year"]
df_actor=pd.read_csv(location+"actor.csv",header=None,delimiter=";")
df_actor.columns=["tid","actor"]

df_outer_1 = pd.merge(left=df_actor,right=df_genre, left_on='tid',right_on='tid', how='outer')
df_inner_1 = pd.merge(left=df_actor,right=df_genre, left_on='tid',right_on='tid', how='inner')

df_outer_1_1 = df_outer_1[df_outer_1.year.astype(str).str.contains("2014")]
df_inner_1_1 = df_inner_1[df_inner_1.year.astype(str).str.contains("2014")]

d1=pd.DataFrame([])
d2=pd.DataFrame([])

for i in ["2015","2016","2017","2018"]:
    global df_outer_1_1
    global df_inner_1_1
    d1 = df_outer_1[df_outer_1.year.astype(str).str.contains(i)]
    d2 = df_inner_1[df_inner_1.year.astype(str).str.contains(i)]
    df_outer_1_1=df_outer_1_1.append(d1)
    df_inner_1_1=df_inner_1_1.append(d2)





#pd.merge(left=survey_sub,right=species_sub, left_on='species_id', right_on='species_id')

print(df_genre.shape)
print(df_genre.columns)

print(df_actor.shape)
print(df_actor.columns)

print("--------------------------------------------------------------------")
print(df_outer_1_1.shape)
print(df_outer_1_1.columns)
print(df_outer_1_1["year"].unique().tolist())
print("--------------------------------------------------------------------")
print(df_inner_1_1.shape)
print(df_inner_1_1.columns)
print("--------------------------------------------------------------------")



df_outer_1_1["combined"]=df_outer_1_1['year'].astype(str)+','+df_outer_1_1['actor'].astype(str)+','+df_outer_1_1['genre']
df_inner_1_1["combined"]=df_inner_1_1['year'].astype(str)+','+df_inner_1_1['actor'].astype(str)+','+df_inner_1_1['genre']

df_outer_x=df_outer_1_1["combined"]
df_inner_x=df_inner_1_1["combined"]

'''
df_outer_1_1.to_parquet("df_outer_x.parquet",compression="snappy")

df_outer_x.to_csv("df_outer_x.csv")

df_inner_x.to_parquet("df_inner_x.parquet",compression="snappy")

df_inner_x.to_csv("df_inner_x.csv")
'''




df_outer_1_1.to_parquet("df_outer_x.parquet",compression="snappy")

df_outer_1_1.to_csv("df_outer_x.csv")

df_inner_1_1.to_parquet("df_inner_x.parquet",compression="snappy")

df_inner_1_1.to_csv("df_inner_x.csv")


print("done")
