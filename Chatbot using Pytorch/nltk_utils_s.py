# -*- coding: utf-8 -*-
"""
Created on Mon Nov 30 16:09:24 2020

@author: user
"""

import numpy as np
import nltk
#nltk.download('punkt') #if any error
from nltk.stem.porter import PorterStemmer
stemmer = PorterStemmer()

#tokenizing the sentences
def tokenize(sentence):
    return nltk.word_tokenize(sentence)

#Applying stemming and converting to lower case
def apply_stemming(word):
    return stemmer.stem(word.lower())

#Applying word embeddings
def bag_of_words(tokenized_sentence, words):
    # stem each word
    sentence_words = [apply_stemming(word) for word in tokenized_sentence]
    # initialize bag with 0 for each word
    bag_words = np.zeros(len(words), dtype=np.float32)
    for i, word in enumerate(words):
        if word in sentence_words: 
            bag_words[i] = 1

    return bag_words