path="C:\\Users\\vaishakh g d\\Desktop\\ItemList.csv"
import pandas as pd
import re
df=pd.read_csv(path,sep='$%^', names=["combined"])
#df.columns=["combined"]
print(df.shape)
#df_filtered=df[df["combined"].astype(str).apply(lambda x: "?" in x)]
#df[~df.C.str.contains("XYZ")]
#df_filtered=df[df["combined"].str.contains("?")]
patternDel = "(\\?)"
filter = df.combined.str.contains(patternDel)
df_filtered=df[~filter]
#df_filtered=df.query('combined == "?"')
print(df_filtered.shape)
df_filtered.to_csv("loafernan.csv",index=False)
print("ok")

