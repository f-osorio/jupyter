    https://mybinder.org/v2/gh/f-osorio/jupyter/master?urlpath=shiny/journal_dashboard/
    https://notebooks.gesis.org/binder/v2/gh/f-osorio/jupyter/original?urlpath=shiny/journal_dashboard/

- spider graph with comparison of 2-5 journals - dimensions: Impact Factor, Altmetric score, # Mendeley readers, SJR, Handelsblatt ranking (zero = no data available), citations 
- spider graph for each journal (available in Bohdan's dataset): Impact Factor, Cites, Handelsblatt VWL/BWL A, B..., SJR, Altmetric Score (zero = no data available), if possible in grey a spider-graph in the background with the average of all journals in the dataset. 
- bubble chart: all journals, closeness of bubbles determined by types of readers (top academic status per journal), bubble size = #citations, bubble color=impact factor 
- a visualisation that tells the user whether the journal is better than average of comparable journals or worse – see for example badges in Impactstory: https://profiles.impactstory.org/u/0000-0001-6728-7745 
- It seems, that for all comparisons, reader information (academic status, country, discipline) is a good filter option 
- Is it possible to somehow visualize how much data is available per data point (maybe colours that become more transparent/light the less data is available)? With this I mean to transport the information that, for example, the average is built from 500 articles from which we had data available but there should be data for 1500 articles (but the data is missing). This might be similar to the stability intervall from https://www.journalindicators.com/indicators 
- overview over journals A+ to D and "not present in the ranking at all", like the one top left here: https://public.tableau.com/profile/bohdan2982#!/vizhome/ZBWJournal_ranking/Dashboard1 but (if possible) with more journals and possibly VWL + BWL in one chart 
- Three main categories for journal comparison: 
    - based on bibliometric indicators (citation, IF, SJR and other available bibliometric indicators) 
    - based on altmetric indicators (altmetric score, mendeley saves) 
    - Mix indicators (combination of the other two above) 
- ~map chart - with comparison of 2-5 journals based on readership information - Country from Mendeley. Using this usecase the users can compare journals and their users country where these journals are read from. For example, Economic journals has been read more from readers from Germany whereas Academy of Management Journal from UK, etc.
Because of a large number of countries - take only the first country that has more readers than other countries per journal.~
- ~Bar chart - visualization of Social Media sources (twitter, news, facebook, blogs and policy documents) that will allow comparison between 2-5 journals.~
- ~Bubble chart - Mendeley readership information for Academic Status for each journal and possibility to compare with other journals~
- Bubble chart - Mendeley readership information for Discipline
