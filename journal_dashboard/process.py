import pandas as pd

alt = pd.read_csv("cleaned_altmetrics.csv", sep=";", header=0)
print(alt.columns)
# Delete unnecessary columns
del alt['title']
del alt['details_page']
del alt['badge_url']
del alt['publication_date']
del alt['doi']

alt = alt.groupby(['print_issn', 'journal_name'])['print_issn', 'journal_name'].agg({"instances": 'size'}).join(alt.groupby(['print_issn', 'journal_name']).sum())
alt.to_csv("simplified_alt.csv", sep=";", doublequote=True)
