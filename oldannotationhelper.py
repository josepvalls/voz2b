import  vozbase
import settings
import voz
import logging
import os
import util
from bs4 import BeautifulSoup
import formatter
from nltk.tree import Tree,ParentedTree
import csv
import collections
import pickle
import narrativehelper
import re
import verbmanager

logger = logging.getLogger(__name__)

def add_functions(last_func,key_to_role,document_id,verbs,ENTITY_ROLE6,taxonomy_labels,narrative,offset,length,role_counts):
    for function in last_func.split():
        if function.startswith('?'):
            location = narrativehelper.NarrativeFunctionLocation('IMPLICIT',[])
            function = function[1:]
        else:
            location = narrativehelper.NarrativeFunctionLocation('ACTUAL',[])
        people = [i.split(')')[0].split('(')[1].split(',') for i in verbs]
        people = set(util.flatten(people))
        for character in people:
            role = key_to_role.get((document_id,character),['NA']*8)[ENTITY_ROLE6]
            role_counts[taxonomy_labels.index(role)]+=1
        verbs_ = [i.split('(')[0] for i in verbs]

        narrative.add_function(0,offset,length,function,[location])
        narrative.function_list[-1].role_counts = role_counts
        narrative.function_list[-1]._verbs = [verbmanager.Verb(0,0,0,voz.Token(0,0,0,'VB',i,i),None,{}) for i in verbs_]

def load_old_annotations_into_document(document_id):
    text = ''
    for line in open('stories/old-annotations/GoogleDocsGroundTruth.csv').readlines():
        line = line.split('\t')
        if line[0] and int(line[0])==document_id:
            text += line[7].strip()+' \n'

    properties = {}
    properties = dict({'source':'load_old_annotations_into_document'}, **properties)
    document = voz.Document(text,[],properties,document_id)
    narrative = narrativehelper.Narrative(document)
    last_func = None
    offset = 0
    text = ''
    verbs = []
    import entitymanager
    taxonomy_labels = entitymanager.taxonomy_dict[entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES].labels
    role_counts = [0 for _ in taxonomy_labels]

    key_to_role = csv.reader(open(settings.STY_FILE_PATH+settings.STY_KEY_TO_ROLE,'rU'))
    ENTITY_TYPE = 3
    ENTITY_ROLE3 = 4
    ENTITY_ROLE6 = 6
    ENTITY_ROLES = 7
    ENTITY_SYMBOL = 2
    ENTITY_GROUP = 8

    key_to_role = dict([((int(j[0]),j[ENTITY_GROUP]),j) for j in key_to_role if j[0].isdigit()])


    for line in open('stories/old-annotations/GoogleDocsGroundTruth.csv').readlines():
        line = line.split('\t')
        if line[0] and int(line[0])==document_id:
            functions = line[5]
            text += line[7].strip()+'\n'
            verbs.extend(line[6].split())
            if last_func is None:
                last_func = functions
            elif last_func == functions:
                pass
            else:
                length = len(text) - offset
                offset = len(text)
                add_functions(last_func,key_to_role,document_id,verbs,ENTITY_ROLE6,taxonomy_labels,narrative,offset,length,role_counts)
                last_func = functions
                verbs = []
    add_functions(last_func,key_to_role,document_id,verbs,ENTITY_ROLE6,taxonomy_labels,narrative,offset,length,role_counts)


    narrative.document = document
    document.narrative = narrative
    return document