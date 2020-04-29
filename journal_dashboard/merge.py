import pandas as pd

# Combine Add journal names to mend_doi to make lookup easier/quicker
biblio = pd.read_csv("biblio_data.csv", sep=";", header=0)
mend_doi = pd.read_csv("mendeley_doi.csv", sep=';', header=0)

#print(biblio[biblio["issn1"] == "2197-8646"]['journal_name'])
b = biblio[biblio["issn1"] == "2197-8646"]
print(type(b))
name = b["journal_name"]
print(type(name))
"""
for index, row in mend_doi.iterrows():
    issn = row['issn']
    row = biblio[biblio["issn1"] == issn]
    name = row['journal_name']
    print('-----------------')
    print(issn)
    print(name)
    print('=================')
"""

"""
mend_doi = pd.read_csv("mendeley_doi.csv", sep=';', header=0)
print(mend_doi.columns)
del mend_doi['year']
del mend_doi['doi']
del mend_doi['publisher']
del mend_doi['title']
del mend_doi['discipline']
del mend_doi['electronic']
print(mend_doi.columns)

mend_doi = mend_doi.groupby('issn')['issn'].agg({"instances": 'size'}).join(mend_doi.groupby('issn').sum())
print(mend_doi.columns)

mend_doi.to_csv("simplified_mend_doi.csv", sep=";", doublequote=True)
"""

"""
alt = pd.read_csv("cleaned_altmetrics.csv", sep=";", header=0)
print(alt.columns)
# Delete unnecessary columns
del alt['title']
del alt['details_page']
del alt['badge_url']
del alt['publication_date']
del alt['doi']

#alt = alt.groupby(['print_issn', 'journal_name']).sum()
#alt = alt.groupby(['print_issn', 'journal_name']).size()
alt = alt.groupby(['print_issn', 'journal_name'])['print_issn', 'journal_name'].agg({"instances": 'size'}).join(alt.groupby(['print_issn', 'journal_name']).sum())

alt.to_csv("simplified_alt.csv", sep=";", doublequote=True)
"""


"""
biblio = pd.read_csv("biblio_data.csv", sep=";", header=0)
mend_geo = pd.read_csv("mendeley_country.csv", sep=';', header=0)
mend_status = pd.read_csv("mendeley_status.csv", sep=';', header=0)
mend_doi = pd.read_csv("mendeley_doi.csv", sep=';', header=0)
mend_dis = pd.read_csv("mendeley_discipline.csv", sep=';', header=0)


doi_biblio = mend_doi.merge(biblio, how='left', left_on='issn', right_on='issn1', suffixes=('_mendeley', '_biblio'))
doi_geo = mend_doi.merge(mend_geo, left_on='id', right_on='id_doi', suffixes=('_doi_', '_country'))
doi_status = mend_doi.merge(mend_status, left_on='id', right_on='id_doi', suffixes=('_doi', '_status'))
doi_dis = mend_doi.merge(mend_dis, left_on='id', right_on='id_doi', suffixes=('_doi', '_discipline'))

doi_geo = doi_biblio.merge(mend_geo, left_on='id', right_on='id_doi', suffixes=('_doi_', '_country'))
doi_status = doi_geo.merge(mend_status, left_on='id_doi_', right_on='id_doi', suffixes=('_doi_', '_status'))
doi_dis = doi_status.merge(mend_dis, left_on='id_doi_', right_on='id_doi', suffixes=('_doi_', '_discipline'))


print(len(doi_biblio))
print(len(doi_status))
print(len(doi_dis))

doi_dis.to_csv('merged.csv')
"""
