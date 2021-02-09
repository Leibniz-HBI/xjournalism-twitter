# To add a new cell, type '# %%'
# To add a new markdown cell, type '# %% [markdown]'
# %%
import nltk
from nltk.sentiment.vader import SentimentIntensityAnalyzer as SIA
sia = SIA()


# %%
sia.polarity_scores("This restaurant was great, but I'm not sure if I'll go there again.")


# %%
import pandas as pd
df = pd.read_csv("all_tweets.csv")
print(df.head())


# %%
df["score"] = [sia.polarity_scores(t)['compound'] for t in df.text]


# %%
print(df.head(50))


# %%
df.to_csv("all_tweets_scored.csv")


