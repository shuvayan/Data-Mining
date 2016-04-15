# -*- coding: utf-8 -*-
"""
Created on Wed Apr 13 16:49:46 2016

@author: shuvayan.das
"""

import pandas as pd
survey = pd.read_csv("D:/Text Analytics/response.csv")


from bs4 import BeautifulSoup

#Initialize the Beautiful Soup object on a single survey response:

survey1 = BeautifulSoup(survey["col1"][8])

#Print the raw review and then the output of get_text

print survey["col1"][2]
print survey1.get_text()

#Dealing with Punct,Numbers and StopWords

import re
#Use regular expressions to do a find and replace:
letters_only = re.sub("[^a-zA-Z]",
                     " ",
                     survey1.get_text())
print letters_only

#Now we convert all to lower case
lower_case = letters_only.lower()
words = lower_case.split() 

#Now remove English stopwords:

from nltk.corpus import stopwords

#Remove stop words from "words"
words = [w for w in words if not w in stopwords.words("english")]
print words

#Putting it all together:Create a function so that these can be done for all the survey reponses.

def survey_to_words(raw_response):
    # get_text for each response
    survey_text = BeautifulSoup(raw_response).get_text()
    #remove non-letters:
    letters_only = re.sub("[^a-zA-Z]"," ",survey_text)
    # convert to lower case and split into individual words:
    words = letters_only.lower().split()
    #In python searching a set is much faster than searching a list,so convert the stop words to a set
    stops = set(stopwords.words("english"))
    #remove stop words:
    meaningful_words = [w for w in words if not w in stops]
    #Join the words back into one string separated by space.
    return (" ".join(meaningful_words))
 
#clean_response = survey_to_words(doc_df["EXP1"][2])
#print clean_response

#Now we need to iterate through all the responses:
#Remove all the empty responses:
survey = survey[pd.notnull(survey['col1'])]
#Initialize an empty list to hold the clean responses:
clean_survey_responses = []

#Now loop through all the responses:
for d in survey["col1"]:
    #Call the function for each response and add the result to clean_survey_response
    clean_survey_responses.append(survey_to_words(d))
    

####Tokenize:
import nltk.data
tokenizer = nltk.data.load('tokenizers/punkt/english.pickle')

term_vec = []

for d in survey["col1"]:
    term_vec.append(nltk.word_tokenize(d))

import gensim
from gensim.models import Word2Vec   
model = Word2Vec(sentences, min_count=10)    
    
    
 
   
