import narrativehelper
import collections

synthetic_dataset_roles = {
    'hero': 'Hero',
    'villain': 'Villain',
    'sfp': 'Prize',
    'fh': 'FalseHero',
    'tester':'Tester'
}

def segment_story(story_data,do_segment='prep',do_mode='text'):
    phases = []
    if do_segment == 'prep':
        rows = []
        phase = 0
        for row in story_data:
            function, text, annotations, offset_start, offset_end = row
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
    if extra and extra not in synthetic_dataset_roles:
        print "role %s from: %s not found in %s" % (extra, str(anno_word), text)
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

def get_data(do_segment=True,include_annotations=False, glue_for_sentences = ' '):
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
            story.append((row[COL_FUNCT],row[COL_TEXT],anno,anno_offset,anno_offset+len(row[COL_TEXT])))
            anno_offset += len(row[COL_TEXT]) + 1  # +1 is to account for a newline or space between sentences

    if do_segment:
        stories.append(segment_story(story))
    else:
        stories.append(story)

    return stories[1:]

def story_stats(story_i,story_data,glue_for_sentences = ' '):
    raw_text = ''
    character_mentions = collections.defaultdict(list)
    character_roles = collections.defaultdict(list)
    anno_c_verbs = 0
    anno_c_rels = 0
    functions = []
    for row in story_data:
        function, text, annotations, offset_start, offset_end = row
        raw_text += text + glue_for_sentences
        if functions and functions[-1][0] and function and functions[-1][0][0]==function[0]: # TODO check the function group instead of the first char
            functions[-1] = (functions[-1][0],functions[-1][1],offset_end)
        else:
            functions.append((function,offset_start,offset_end))
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
    return (raw_text,character_mentions,character_roles,story_data,functions)


def story_stats(story_i,story_data,glue_for_sentences = ' '):
    raw_text = ''
    character_mentions = collections.defaultdict(list)
    character_roles = collections.defaultdict(list)
    anno_c_verbs = 0
    anno_c_rels = 0
    functions = []
    for row in story_data:
        print row
        function, text, annotations, offset_start, offset_end = row
        raw_text += text + glue_for_sentences
        if functions and functions[-1][0] and function and functions[-1][0][0]==function[0]: # TODO check the function group instead of the first char
            functions[-1] = (functions[-1][0],functions[-1][1],offset_end)
        else:
            functions.append((function,offset_start,offset_end))
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
    return (raw_text,character_mentions,character_roles,story_data,functions)


def create_document_from_story_data(story_data,properties={}, annotate=True):
    import stanfordhelper
    raw_text, character_mentions, character_roles, story_data, functions = story_stats(properties['story_id'], story_data)
    import entitymanager
    stats_not_found = 0
    stats_ambiguous = 0
    stats_match_ok = 0
    doc = stanfordhelper.create_document_from_raw_text(raw_text,properties)
    # TODO clean Stanford stuff
    for row in story_data:
        function, text, annotations, offset_start, offset_end = row
        raw_text += text + ' '
        for annotation in annotations:
            verb,msubj,mobj = annotation
            vargs = None
            try:
                token = doc.get_token_by_off(verb[2])
                vargs = doc.get_verb_by_token_id(token.id).arguments
            except:
                pass
            if vargs is not None:
                if msubj:
                    key, neg, o_start, o_end, extra = msubj
                    token = doc.get_token_by_off(o_start)
                    mention = doc.get_mention_by_token_id(token.id)
                    if mention:
                        vargs['nsubj'] = mention.tokens
                if mobj:
                    key, neg, o_start, o_end, extra = mobj
                    token = doc.get_token_by_off(o_start)
                    mention = doc.get_mention_by_token_id(token.id)
                    if mention:
                        vargs['dobj'] = mention.tokens

    # Annotate coref and roles
    for character_id,character in enumerate(character_mentions):
        for mention_data in character_mentions[character]:
            token = doc.get_token_by_off(mention_data[1])
            mention = doc.get_mention_by_token_id(token.id)
            #print mention_data,token,mention,character_id
            if mention:
                mention.is_independent = True
                mention.annotations.coref = character_id + 1000
                mention.annotations.character = True
                mention.annotations.type = 'animate'
                if character_roles.get(character,None):
                    mention.annotations.role = synthetic_dataset_roles.get(mention_data[0],'Other')
                    # TODO set the rest of mentions to NA
                if annotate:
                    mention.predictions.coref = character_id + 1000
                    mention.predictions.character = True
                    mention.annotations.type = 'animate'
                    if character_roles.get(character, None):
                        mention.predictions.role = synthetic_dataset_roles.get(mention_data[0], 'Other')
                        # TODO set the rest of mentions to NA
    # Annotate functions
    for function,offset_start,offset_end in functions:
        doc.narrative.add_function(doc.get_new_id("Function"),offset_start,offset_end-offset_start,function,[narrativehelper.NarrativeFunctionLocation('ACTUAL',[i.id for i in doc.get_tokens_by_off_len(offset_start,offset_end)])])
    return doc

def aaai_loop():
    import logging
    import tool_entity_classification_loop_aaai
    import classificationhelper
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

def generate_tokenized_files():
    stories = get_data(do_segment=False, include_annotations=True)
    stories_ = get_data(do_segment=True, include_annotations=True)[0:100]
    from nltk.tokenize import TreebankWordTokenizer
    for story_i, story_data_ in enumerate(zip(stories,stories_)):
        story_data,segmented_story_data = story_data_
        raw_text, character_mentions, character_roles, story_data, functions = story_stats(story_i, story_data)
        open("training stories/training_story_%d_full.txt" % (story_i+1),'w').write("\n".join(TreebankWordTokenizer().tokenize(raw_text.lower())).replace('.','\n.').replace('\n\n','\n'))
        #print TreebankWordTokenizer().tokenize(raw_text.lower())
        #print segmented_story_data[0][0][1]
        for segment_i in range(4):
            try:
                open("training stories/training_story_%d_%d.txt" % (story_i + 1,segment_i+1), 'w').write("\n".join(TreebankWordTokenizer().tokenize(' '.join(segmented_story_data[segment_i][0][1]).lower())).replace('.','\n.').replace('\n\n','\n'))
            except:
                print "error",story_i,segment_i,segmented_story_data

        '''part_2 = segmented_story_data[1:]
        text_ = []
        for segment in part_2:
            for group in segment:
                for sent in group[1]:
                    text_.append(sent)
        text_ = ' '.join(text_)
        print TreebankWordTokenizer().tokenize(text_.lower())'''

def generate_riu_files():
    stories = get_data(do_segment=False, include_annotations=True)
    print len(stories)

    stories_in_use = 40
    stories = stories[0:stories_in_use]
    import riuhelper
    export_path = '/Users/josepvalls/voz2/sam-clisp-irb/'
    script = ''
    for suffix_g in ['voz']:#['syn','voz']:  # sty voz
        for suffix_v in ['levinverb']:  # noverb, 'basicverb' levinverb
            for suffix_f in ['nofunc','functs']:
                for suffix_r in ['roleexp']:  # 'norole','roleent','roleexp'
                    for suffix_s in ['prep']:
                        suffix = suffix_v + '_' + suffix_f + '_' + suffix_r + '_' + suffix_s + '_' + suffix_g
                        # write the scripts
                        for k in ['complete']:#['eval', 'full']:
                            for i, s in enumerate(riuhelper.get_riu_runner(stories_in_use, suffix, k)):
                                fname = 'voz-' + k + '-' + str(i + 1) + '-' + suffix + '.lisp'
                                with open(export_path + fname, 'w') as f:
                                    f.write(s)
                                script += 'clisp ' + fname + ' > ' + fname + '.txt &\n'
                                # break

                        #continue

                        # write the stories
                        for story_i, story_data in enumerate(stories):
                            if suffix_g == 'syn':
                                doc = create_document_from_story_data(story_data, {'story_id': story_i}, True)
                            else:
                                doc = create_document_from_story_data(story_data, {'story_id': story_i}, True)
                                doc.compute_predictions()
                            out = riuhelper.doc_to_sam(doc, suffix_g, suffix_v, suffix_f, suffix_r, suffix_s, limit=None)
                            with open(export_path + ('voz/story%d' % doc.id) + '-full-%s.lisp' % suffix, 'w') as f:
                                f.write(out)
                            # continue
                            out = riuhelper.doc_to_sam(doc, suffix_g, suffix_v, suffix_f, suffix_r, suffix_s, limit=2)
                            with open(export_path + ('voz/story%d' % doc.id) + '-complete-%s.lisp' % suffix,
                                      'w') as f:
                                f.write(out)
                            out = riuhelper.doc_to_sam(doc, suffix_g, suffix_v, suffix_f, suffix_r, suffix_s, limit=1)
                            with open(export_path + ('voz/story%d' % doc.id) + '-partial-%s.lisp' % suffix,
                                      'w') as f:
                                f.write(out)

                                # break
    with open(export_path + 'main.sh', 'w') as f:
        f.write(script)


def main():
    generate_riu_files()
    return
    #generate_tokenized_files()
    stories = get_data(do_segment=False, include_annotations=True)
    print len(stories)

    for story_i,story_data in enumerate(stories):
        print create_document_from_story_data(story_data,{'story_id':story_i})
        break
    #print len(stories)
    #pprint.pprint(story_stats(stories[19]))
    #pprint.pprint(story_stats(stories[20]))


if __name__=='__main__':
    main()