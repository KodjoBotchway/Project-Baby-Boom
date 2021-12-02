import pandas as pd
import spacy

#reading in data
data=pd.read_csv('/Users/adwoaboadi-asamoah/Desktop/programming/project.csv')
dat= data['0'].to_list()   
listi=' '.join(map(str,dat))   
        

# Load SpaCy model
nlp = spacy.load("en_core_web_sm")
doc = nlp(listi)

entities = []
labels = []

for ent in doc.ents:
    entities.append(ent)
    labels.append(ent.label_)

# creating a dataframe for named entities and labels    
df = pd.DataFrame({'Entities':entities,'Labels':labels})

# named entities of interest put into dataframes
loc=df[df['Labels']=='LOC']
person=df[df['Labels']=='PERSON']
org=df[df['Labels']=='ORG']
date=df[df['Labels']=='DATE']

# creating csv files from dataframes


loc.to_csv('/Users/adwoaboadi-asamoah/Desktop/programming/loc.csv')
person.to_csv('/Users/adwoaboadi-asamoah/Desktop/programming/person.csv')
org.to_csv('/Users/adwoaboadi-asamoah/Desktop/programming/org.csv')
date.to_csv('/Users/adwoaboadi-asamoah/Desktop/programming/date.csv')



