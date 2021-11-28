### This would be the code to run sentimental analysis and named entity recognition on the articles for information
import selenium
import pandas as pd
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException

import nltk
from nltk.stem import PorterStemmer
import matplotlib.pyplot as plt
from nltk import word_tokenize, sent_tokenize
from nltk.corpus import stopwords
from nltk.stem import LancasterStemmer, WordNetLemmatizer, PorterStemmer
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import LatentDirichletAllocation
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score, plot_confusion_matrix

from nltk import word_tokenize, pos_tag, ne_chunk
from nltk.chunk import conlltags2tree, tree2conlltags

driver = webdriver.Firefox(executable_path= "K:\Softs\geckodriver.exe")
article_url = "https://www.rand.org/blog/2021/04/an-early-look-at-the-impact-of-the-covid-19-pandemic.html"
driver.get(article_url)

# Extraction of paragraphs
path = '//div[@class="body-text"]/p'
all = driver.find_elements_by_xpath(path)

text = []
for i in all:
    text.append(i.text)

driver.quit()

### Create a Term-Document Matrix
# Remove stop words
stop = stopwords.words('english')
text = pd.DataFrame(text)
textcol = text[0]
textcol = textcol.apply(lambda x: " ".join(x for x in x.split() if x not in stop))

# Remove numerical values
patternnum = '\b[0-9]+\b'
textcol = textcol.str.replace(patternnum,'')

# Remove punctuation
patternpunc = '[^\w\s]'
textcol = textcol.str.replace(patternpunc,'')

# Convert to lowercase
textcol = textcol.apply(lambda x: " ".join(x.lower() for x in x.split()))

# Stem the words
porstem = PorterStemmer()
textcol = textcol.apply(lambda x: " ".join([porstem.stem(word) for word in x.split()]))

# Convert data into a document matrix
vectorizer = CountVectorizer()
tokens = pd.DataFrame(vectorizer.fit_transform(textcol).toarray(), columns=vectorizer.get_feature_names())
tokens.columns
print(tokens.columns.tolist())

# LDA
vectorizer = CountVectorizer(max_df=0.8, min_df=4, stop_words='english')
tweet_values = textcol.values.astype('U') #convert Panda values to unicode
doc_term_matrix = vectorizer.fit_transform(tweet_values)
doc_term_matrix.shape
LDA = LatentDirichletAllocation(n_components=4, random_state=35)
LDA.fit(doc_term_matrix)

first_topic = LDA.components_[0]
top_topic_words = first_topic.argsort()[-10:]

for i,topic in enumerate(LDA.components_):
    print(f'Top 10 words for topic #{i}:')
    print([vectorizer.get_feature_names()[i] for i in topic.argsort()[-10:]])
    print('\n')

topic_values = LDA.transform(doc_term_matrix)
topic_values.shape
text['topic'] = topic_values.argmax(axis=1)
text.head()