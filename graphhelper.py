import logging
import settings
import stanfordhelper
import voz
import verbmanager
import collections
import sys
import os
import re

def doc_to_graph(doc,filter_segment=None):
    assert isinstance(doc,voz.Document)
    all_coref_mentions = set()
    nodes = []
    for i in doc.get_all_mentions(filter_only_independent=True):
        if not i.predictions.coref:
            i.predictions.coref = i.id
        if i.predictions.coref not in all_coref_mentions:
            all_coref_mentions.add(i.predictions.coref)
            nodes.append(i)
    edges = collections.defaultdict(list)
    if filter_segment is None:
        verbs = doc.get_all_verbs()
    else:
        verbs = []
        for sentence in doc.sentences:
            if sentence.annotations.scene == filter_segment:
                verbs += sentence.verbs
    for verb in verbs:
        assert isinstance(verb,verbmanager.Verb)
        subjects = [i.predictions.coref for i in verb.get_subjects()] or [-1]
        objects = [i.predictions.coref for i in verb.get_objects()] or [-1]
        for i in subjects:
            for j in objects:
                edges[(i,j)].append(verb)
    return (nodes,edges)
def print_adj_matrix(graph,include_na=True,print_labels=True,verb_glue=';',field_separator='\t'):
    nodes, vertices = graph
    for i_i,i in enumerate(nodes):
        print i_i, i.predictions.coref, i.predictions.type, i.predictions.role, i, i.tokens[0]._parent_sentence.idx, i.tokens[0].idx
    nodes_ = [i.predictions.coref for i in nodes]
    if include_na:
        nodes_.append(-1)
    if print_labels:
        print field_separator,
        for sub in nodes_:
            print sub, field_separator,
        print
    for sub in nodes_:
        if print_labels:
            print sub,field_separator,
        for obj in nodes_:
            print verb_glue.join([str(i.get_text()) for i in vertices.get((sub,obj),[])]), field_separator,
        print

def annotate_segments(doc,segments):
    sentence = 0
    for i, text in enumerate(segments):
        consume = len(re.sub(r"[^\w]",'',text).lower())
        while consume > 0 and sentence < len(doc.sentences):
            doc.sentences[sentence].annotations.scene = i
            consume -= len(re.sub(r"[^\w]",'',doc.sentences[sentence].get_text()).lower())
            sentence += 1

def demo():
    logging.basicConfig(level=logging.DEBUG)
    settings.SERIALIZE_PRETTY_JSON_BY_DEFAULT = True
    file_path = "/Users/josepvalls/voz2/data/"
    story_file = "TestInput.txt"
    doc = stanfordhelper.create_document_from_raw_text(open(file_path+story_file).read())
    print doc
    print_adj_matrix(doc_to_graph(doc))

def main():
    print sys.argv
    segments = []
    files = []
    path = '.'
    if len(sys.argv)>=2:
        path = sys.argv[1]
        if not path.endswith('/'):
            path += '/'
        if not os.path.isdir(path):
            print "provide a folder with the txt files"
            sys.exit()
    for fname in sorted(os.listdir(path)):
        if not fname.lower().endswith('.txt'): continue
        files.append(fname)
        segments.append(re.sub(r"\".*?\"","QUOTE",open(path+fname).read()))
    text = '\n'.join(segments)
    doc = stanfordhelper.create_document_from_raw_text(text)
    print doc
    print_adj_matrix(doc_to_graph(doc))
    annotate_segments(doc,segments)
    for i,fname in enumerate(files):
        print fname
        print_adj_matrix(doc_to_graph(doc,filter_segment=i))




if __name__ == '__main__':
    main()