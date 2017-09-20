import narrativehelper

def segment_story(story_data,do_segment='prep',do_mode='text'):
    phases = []
    if do_segment == 'prep':
        rows = []
        phase = 0
        for row in story_data:
            function, text, annotations = row
            function_group = narrativehelper.NarrativeFunction.translate_function(function)
            if phase == 0 and function_group not in 'A,a,depart'.split(','):
                rows.append(row)
            elif phase == 0 and function_group in 'A,a,depart'.split(','):
                phase = 1
                phases.append(rows)
                rows = []
                rows.append(row)
            elif phase == 1 and function_group in 'A,a,depart'.split(','):
                rows.append(row)
            elif phase == 1 and function_group not in 'A,a,depart'.split(','):
                phase = 2
                phases.append(rows)
                rows = []
                rows.append(row)
            elif phase == 2 and function_group in 'B,C,D,E,F,G,H,I,J,K'.split(','):
                rows.append(row)
            elif phase == 2 and function_group not in 'B,C,D,E,F,G,H,I,J,K'.split(','):
                phase = 3
                phases.append(rows)
                rows = []
                rows.append(row)
            elif phase == 3:
                rows.append(row) # return,Pr,Rs,o,L,M,N,Q,Ex,T,U,W
        phases.append(rows)
    if do_mode=='text':
        return [[([j[0] for j in i],[j[1] for j in i])] for i in phases]
    else:
        return phases

def find_offsets(text,anno_word,anno_offset):
    token,word,extra = (anno_word+'//').split('/')[0:3]
    token = token.strip()
    word = word.strip()
    extra = extra.strip()
    if not token: return None
    if not word: word = token
    wordi = 1
    wordn = False
    if word and word[-1] in '123456789':
        wordi = int(word[-1])
        word=word[0:-1]
    if word and word[0]=='!':
        wordn = True
        word = word[1:]
    word_offset = None
    word = word.lower()
    text = text.lower()
    current_offset=-1
    while wordi>0:
        current_offset = text.find(word,current_offset+1)
        if current_offset<0: break
        if current_offset>= len(text): break
        if (current_offset==0 or current_offset>0 and not text[current_offset-1].isalpha()):
            wordi-=1
            word_offset = current_offset
    if word_offset==None:
        print "word %s from: %s not found in: %s" % (word,str(anno_word),text)
        return None
    ret = (token,wordn,anno_offset+word_offset,anno_offset+word_offset+len(word),extra)
    #print ret
    #print token,wordn,word_offset,word_offset+len(word),extra,text[word_offset:word_offset+len(word)]
    return ret

def parse_annotations(text,anno,anno_offset):
    ret = []
    for verb_anno in anno.split(';'):
        vword,vsubj,vobj,_ = (verb_anno+',,,').split(',',3)
        if not vword: continue
        vword = find_offsets(text,vword,anno_offset)
        vsubj = find_offsets(text,vsubj,anno_offset)
        vobj = find_offsets(text,vobj,anno_offset)
        ret.append((vword,vsubj,vobj))
    return ret

def get_data(do_segment=True,include_annotations=False):
    stories = []
    COL_STORY = 0
    COL_FUNCT = 3
    COL_VERB = 4
    COL_TEXT = 5
    story = []
    anno_offset = 0
    if include_annotations:
        character_dict = {}

    for line in open('stories/synthetic.tsv').readlines():
        if not line.strip():
            continue
        row = line.split('\t')
        if len(row)>0 and row[COL_STORY]:
            if story:
                if do_segment:
                    stories.append(segment_story(story))
                else:
                    stories.append(story)
            story = []
            anno_offset = 0
        if len(row)>COL_TEXT and row[COL_FUNCT] and row[COL_TEXT]:
            if include_annotations:
                anno = parse_annotations(row[COL_TEXT],row[COL_VERB],anno_offset)
            else:
                anno = row[COL_VERB]
            story.append((row[COL_FUNCT],row[COL_TEXT],anno))
            anno_offset += len(row[COL_TEXT]) + 1  # +1 is to account for a newline or space between sentences

    if do_segment:
        stories.append(segment_story(story))
    else:
        stories.append(story)

    return stories[1:]

def story_stats(story_i,story_data):
    import collections
    raw_text = ''
    character_mentions = collections.defaultdict(list)
    character_roles = collections.defaultdict(list)
    anno_c_verbs = 0
    anno_c_rels = 0
    for row in story_data:
        function, text, annotations = row
        raw_text += text + ' '
        for annotation in annotations:
            anno_c_verbs+=1
            for mention in [annotation[1],annotation[2]]:
                if not mention: continue
                anno_c_rels+=1
                key, neg, o_start, o_end, extra = mention
                character_mentions[key].append((raw_text[o_start:o_end],o_start,o_end))
                if extra:
                    character_roles[key].append(extra)
    for key in character_mentions:
        pass
        #print str(story_i)+"\t"+key+"\t"+str(len(character_roles.get(key,['other'])))+"\t"+character_roles.get(key,['other'])[0]+"\t"+str(character_roles.get(key,''))+"\t"+str(len(character_mentions[key]))+"\t"+str(character_mentions[key])
    #print anno_c_verbs,anno_c_rels
    return (raw_text,character_mentions,character_roles,story_data)


import networkcachemanager
import stanfordhelper
import settings

def create_document_from_story_data(story_data,properties={}):
    raw_text, character_mentions, character_roles, story_data = story_stats(properties['story_id'], story_data)
    import entitymanager
    stats_not_found = 0
    stats_ambiguous = 0
    stats_match_ok = 0
    doc = stanfordhelper.create_document_from_raw_text(raw_text,properties)

    # Annotate

    for character_id,character in enumerate(character_mentions):
        for mention_data in character_mentions[character]:
            token = doc.get_token_by_off(mention_data[1])
            mention = doc.get_mention_by_token_id(token.id)
            print mention_data,token,mention,character_id
            if mention:
                mention.annotations.coref = character_id
                mention.annotations.character = True

    return doc

import logging
import tool_entity_classification_loop_aaai
import classificationhelper

def aaai_loop():
    logging.root.setLevel(logging.ERROR)
    docs = load_documents()
    tool_entity_classification_loop_aaai.generate_tsv_file(docs, classificationhelper.TASK_COREF, 0)
    tool_entity_classification_loop_aaai.do_loop(docs)


def load_documents():
    stories = get_data(do_segment=False, include_annotations=True)
    documents = []
    for story_i,story_data in enumerate(stories):
        doc= create_document_from_story_data(story_data,{'story_id':story_i+20000})
        documents.append(doc)
    return documents



def main():
    aaai_loop()
    return
    import pprint

    stories = get_data(do_segment=False, include_annotations=True)
    for story_i,story_data in enumerate(stories):
        print create_document_from_story_data(story_data,{'story_id':story_i})
        break
    #print len(stories)
    #pprint.pprint(story_stats(stories[19]))
    #pprint.pprint(story_stats(stories[20]))


if __name__=='__main__':
    main()