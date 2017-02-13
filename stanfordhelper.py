import settings
import voz
import networkcachemanager
from xml.dom import minidom
import logging
import parse_tree_mention_helper
from nltk.tree import ParentedTree
logger = logging.getLogger(__name__)
import dependencyhelper
import util
import verbmanager
import os

VERB_POS = ['VB','VBD','VBG','VBN','VBP','VBZ']

def create_document_from_raw_text(str_input,properties={}):
    raw_xml_data = networkcachemanager.stanford_nlp.query(str_input)
    return annotate_document_from_corenlp(str_input,raw_xml_data, properties)

def annotate_document_from_xml_file(str_input,xml_file,properties={}):
    return annotate_document_from_corenlp(str_input,open(xml_file).read())

def annotate_document_from_corenlp(str_input,raw_xml_data,properties={}):
    properties = dict({'source':'annotate_document_from_corenlp'}, **properties)
    sentences = []
    document = voz.Document(str_input, sentences, properties)  # type: voz.Document
    xmldoc = minidom.parseString(raw_xml_data)
    #sentence_token_index_to_token_id = {}
    for sentence_xml in xmldoc.getElementsByTagName('sentences')[0].getElementsByTagName('sentence'):
        id_ = int(sentence_xml.getAttribute('id'))
        tokens = [token_from_xml(xml,str_input,document) for xml in sentence_xml.getElementsByTagName('tokens')[0].getElementsByTagName('token')]
        if tokens:
            offset = tokens[0].offset
            offset_end = tokens[-1].offset+tokens[-1].len
        sentence = voz.Sentence(id_,offset,offset_end-offset,tokens)
        parse_string = sentence_xml.getElementsByTagName('parse')[0].firstChild.nodeValue
        sentence.parse_string = parse_string
        sentence.parse_tree = ParentedTree.fromstring(parse_string)
        sentences.append(sentence)

    document.id = properties.get('story_id',-1)
    document._compute_caches(document)


    _annotate_document_from_corenlp_mentions(xmldoc,document)
    _annotate_document_from_corenlp_coref(xmldoc,document)
    _annotate_document_from_corenlp_deps(xmldoc,document)
    _annotate_document_from_corenlp_verbs_from_deps(xmldoc, document)
    _compute_roles()

    document._compute_caches(document) # verb slots

    return document

def _compute_roles():
    pass
    # TODO move here?

def _annotate_document_from_corenlp_deps(xmldoc, document):
    # TODO reimplement

    for sentence, sentence_xml in zip(document.sentences, xmldoc.getElementsByTagName('sentences')[0].getElementsByTagName('sentence')):
        sentence.dependencies = []
        # <collapsed-ccprocessed-dependencies
        elements = sentence_xml.getElementsByTagName('collapsed-ccprocessed-dependencies')
        if elements:
            sentence.dependencies += [dependencyhelper.Dependency.from_xml(i, sentence, False) for i in elements[0].getElementsByTagName('dep')]
        elements = sentence_xml.getElementsByTagName('basic-dependencies')
        if elements:
            sentence.dependencies += [dependencyhelper.Dependency.from_xml(i, sentence, True) for i in elements[0].getElementsByTagName('dep')]
        # <dependencies type="collapsed-ccprocessed-dependencies"
        elements = sentence_xml.getElementsByTagName('dependencies')
        for element in elements:
            if element.getAttribute('type')=="collapsed-ccprocessed-dependencies" or element.getAttribute('type')=="basic-dependencies":
                sentence.dependencies += [dependencyhelper.Dependency.from_xml(i, sentence) for i in element.getElementsByTagName('dep')]
        sentence.dependencies = filter(None,sentence.dependencies)



def _annotate_document_from_corenlp_verbs_from_deps(xmldoc, document):
    for sentence in document.sentences:
        #'AM-NEG'
        #verb.arguments = {type,tokens}
        for dep in [i for i in sentence.dependencies if
                    i.type in ['nsubj', 'expl', 'nsubjpass'] and i.governor.pos in VERB_POS]:
            verb_token = dep.governor
            # Particles (Phrasal Verbs)
            aprts = [obj.dependent for obj in sentence.dependencies if
                     obj.governor == verb_token and obj.type in ['prt']]
            # Modifiers (Negation, Adverbs...)
            amods = [obj.dependent for obj in sentence.dependencies if
                     obj.governor == verb_token and obj.type in ['advmod']]
            amneg = [obj.dependent for obj in sentence.dependencies if
                     obj.governor == verb_token and obj.type in ['neg']]
            slots = [(obj.type,obj.dependent) for obj in sentence.dependencies if
                     obj.governor == verb_token and obj.type in ['nsubj', 'expl', 'nsubjpass', 'dobj', 'iobj', 'pobj']]
            args = {'APRTS': aprts, 'AM': amods, 'AM-NEG': amneg}
            for label,token in slots:
                if label not in args: args[label] = []
                args[label].append(token)
            verb = verbmanager.Verb(document.get_new_id(verbmanager.Verb), verb_token.offset, verb_token.len, verb_token,
                                    verb_token.lemma, args)
            sentence.verbs.append(verb)


def _annotate_document_from_corenlp_mentions(xmldoc,document):
    for sentence_xml in xmldoc.getElementsByTagName('sentences')[0].getElementsByTagName('sentence'):
        id_ = int(sentence_xml.getAttribute('id'))
        sentence = document.get_sentence_by_id(id_)
        sentence.parse_string = sentence_xml.getElementsByTagName('parse')[0].firstChild.nodeValue
        parse = ParentedTree.fromstring(sentence.parse_string)
        assert len(parse.leaves())==len(sentence.tokens)
        for i in xrange(len(sentence.tokens)):
            parse[parse.leaf_treeposition(i)]=sentence.tokens[i]
        sentence.parse_tree = parse
        #parse = ImmutableParentedTree.convert(parse)
        parser = parse_tree_mention_helper.StanfordTreeParser()
        sentence.mentions = parser.get_mentions(parse,sentence,document)

def _annotate_document_from_corenlp_coref(xmldoc,document):
    if xmldoc.getElementsByTagName('document')[0].getElementsByTagName('coreference'):
        _init_coreference_from_xml(xmldoc.getElementsByTagName('document')[0].getElementsByTagName('coreference')[0],document)

def token_from_xml(xml,str_input,document):
    #id_ = int(xml.attributes.get('id').value)
    id_ = document.get_new_id(voz.Token)
    offset = int(xml.getElementsByTagName('CharacterOffsetBegin')[0].firstChild.nodeValue)
    offset_end = int(xml.getElementsByTagName('CharacterOffsetEnd')[0].firstChild.nodeValue)
    length = offset_end-offset
    pos = xml.getElementsByTagName('POS')[0].firstChild.nodeValue
    lemma = xml.getElementsByTagName('lemma')[0].firstChild.nodeValue
    text = str_input[offset:offset_end]
    token = voz.Token(id_,offset,length,pos,lemma,text) # type: voz.Token
    return token


def _init_coreference_from_xml(xml,document):
    for coreference in xml.getElementsByTagName('coreference'):
        id_ = document.get_new_id(voz.entitymanager.CoreferenceGroup)
        entity = None
        mentions = []
        for mention_xml in coreference.getElementsByTagName('mention'):
            representative,mention = _coref_mention_from_xml(document,mention_xml)
            if not mention.is_independent: continue
            mentions.append(mention)
            sentence = mention.tokens[0]._parent_sentence
            sentence.mentions.append(mention)
            if representative:
                entity = mention.to_new_entity(id_)
                document.coreference.entities.append(entity)
        for mention in mentions:
            # TODO clean this shit up, move elsewhere
            if mention.id==-1: continue
            sentence = mention.tokens[0]._parent_sentence
            mention_to_parse_node = dict([(v,k) for k,v in sentence.parse_highlight.get('mentions',{}).items()])
            if parse_tree_mention_helper.HIGHLIGHT_COREF not in sentence.parse_highlight:
                sentence.parse_highlight[parse_tree_mention_helper.HIGHLIGHT_COREF]={}
            # TODO: broke when set ids for coreference groups
            #sentence.parse_highlight[parse_tree_mention_helper.HIGHLIGHT_COREF][mention_to_parse_node[mention.id]]=entity.id

        for mention in mentions:
            if mention.is_independent:
                mention.predictions.coref = id_
        if not entity and mentions:
            entity = mentions[0].to_new_entity(id_)
        if entity:
            document.coreference.add_coreference_group(id_, mentions, entity)


def _coref_mention_from_xml(document,xml):
    sentence_id = int(xml.getElementsByTagName('sentence')[0].firstChild.nodeValue)
    sentence = document.get_sentence_by_id(sentence_id) #type: voz.Sentence
    start = int(xml.getElementsByTagName('start')[0].firstChild.nodeValue)
    end = int(xml.getElementsByTagName('end')[0].firstChild.nodeValue)
    representative= True if 'representative' in xml.attributes.keys() else False
    tokens = sentence.get_tokens_by_idx_range(start-1,end-1)
    mention,parent_mention = sentence.get_mention_by_tokens(tokens,True)
    if not mention:
        logger.warn("Coreference: Exact mention not found, creating new children")
        parser = parse_tree_mention_helper.StanfordTreeParser()
        mention = voz.entitymanager.Mention(document.get_new_id(voz.entitymanager.Mention)+1000,tokens)
        parse_node = sentence.get_parse_node_by_tokens(tokens)
        if parse_node:
            mention.is_compound = set([i.label() for i in parse_node.subtrees()]) & set(parser.get_tags_phrase())
        else:
            mention.is_compound = voz.Token.filter(mention.tokens, pos_list=parser.get_tags_phrase())
        mention.is_list = voz.Token.filter(mention.tokens,pos_list=parser.get_tags_list())
        mention.is_independent = not mention.is_compound and not mention.is_list and not (len(tokens)==1 and tokens[0].pos=="PRP$")
        mention.parent_mention = parent_mention
        mention._compute_caches(sentence)
        if parent_mention:
            parent_mention.child_mentions.append(mention)
    return representative,mention


def create_document_using_stanford_from_filtered_sty_file(sty_file):
    import styhelper,quotedspeechhelper,entitymanager
    stats_not_found = 0
    stats_ambiguous = 0
    stats_match_ok = 0
    logger.info("Processing %s" % sty_file)
    doc = styhelper.create_document_from_sty_file(sty_file)
    quotedspeechhelper.annotate_sentences(doc, settings.STORY_ALL_SENTENCES, format='tsv',single_sentences_file_story_id=doc.id)
    text = "\n".join([sentence.get_text() for sentence in doc.sentences if sentence.annotations.is_normal()])
    doc_new = create_document_from_raw_text(text,{'story_id':doc.id+1000})
    assert len([sentence for sentence in doc.sentences if sentence.annotations.is_normal()])==len(doc_new.sentences), "Sentence length mismatch between annotated and processed document"
    fixed_annotation_file = settings.STORY_ANNOTATION_FIXES + '%d.tsv' % doc_new.id
    if not os.path.isfile(fixed_annotation_file):
        # Dump data for fixing
        f_fixes = open(fixed_annotation_file,'w')
        for sentence in [sentence for sentence in doc.sentences if sentence.annotations.is_normal()]:
            mentions_check = [i for i in sentence.mentions if len([j for j in i.tokens if j.pos!='DT'])>1]
            mentions_check = sorted(mentions_check,key=lambda i:(len(i.child_mentions)*100-i.id),reverse=True)
            while mentions_check:
                mention = mentions_check.pop(0)
                assert isinstance(mention, entitymanager.Mention)
                f_data = mention.get_text() + "\t" + str(
                    mention.get_taxonomy(entitymanager.TaxonomyContainer.TAXONOMY_ENTITY_TYPES)) + ' ' + str(
                    mention.get_taxonomy(entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES)) + ' ' + str(mention.get_coref_group_id())
                f_data = "\t%d\t%d\t%s\n" % (doc_new.id,mention.id,f_data)
                f_fixes.write(f_data)
                for mention_ in mention.child_mentions:
                    f_data = mention_.get_text() + "\t" + str(
                        mention_.get_taxonomy(entitymanager.TaxonomyContainer.TAXONOMY_ENTITY_TYPES)) + ' ' + str(
                        mention_.get_taxonomy(entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES)) + str(mention.get_coref_group_id())
                    f_data = "\t%d\t%d\t - %s\n" % (doc_new.id, mention_.id, f_data)
                    f_fixes.write(f_data)
                    try:
                        mentions_check.remove(mention_)
                    except:
                        pass
        f_fixes.close()

    # Annotate
    fixed_annotation_file_extra = settings.STORY_ANNOTATION_FIXES + '%d-extra.tsv' % doc_new.id
    if not os.path.isfile(fixed_annotation_file_extra):
        f_fixes = open(fixed_annotation_file_extra, 'w')
    else:
        f_fixes = None

    for sentence_ref,sentence in zip([sentence for sentence in doc.sentences if sentence.annotations.is_normal()],doc_new.sentences):
        assert isinstance(sentence, voz.Sentence)
        for mention in sentence.mentions:
            if not mention.is_independent: continue
            assert isinstance(mention,entitymanager.Mention)
            tokens_ref = [sentence_ref.tokens[i.idx] for i in mention.tokens]
            mentions_ref = set(filter(None,[sentence_ref._parent_document.get_mention_by_token_id(i.id) for i in tokens_ref]))
            if not mentions_ref:
                logger.warning("UNABLE TO FIND ANNOTATION FOR MENTION %s" % mention.get_text())
                if f_fixes:
                    f_fixes.write("%d\tMISS\t%s\t%s\n" % (mention.id,mention.get_text(),str(mention)))
                stats_not_found += 1
                continue
            elif not len(mentions_ref)==1:
                logger.warning("AMBIGUOUS ANNOTATION FOR MENTION")
                stats_ambiguous += 1
                mentions_ref = sorted(mentions_ref,key=lambda i:len(i.tokens))
                for i in mentions_ref:
                    if mention_ref.get_taxonomy(entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES):
                        mention_ref = i
                        break
                if f_fixes:
                    f_fixes.write("%d\tAMBG\t%s\t%s\t%s\n" % (mention.id,mention.get_text(),[str(i) for i in mentions_ref],mention_ref))
            else:
                mention_ref = mentions_ref.pop()
                stats_match_ok +=1

            if len(mention_ref.get_taxonomy(entitymanager.TaxonomyContainer.TAXONOMY_ENTITY_TYPES))>1:
                logger.info(util.string_as_print("POTENTIALLY IGNORE",mention_ref,mention_ref.get_taxonomy(entitymanager.TaxonomyContainer.TAXONOMY_ENTITY_TYPES)))
                mention.annotations.split_ignore = True
            mention.annotations.coref = mention_ref.get_coref_group_id()
            mention.annotations.type = \
                (mention_ref.get_taxonomy(entitymanager.TaxonomyContainer.TAXONOMY_ENTITY_TYPES) or ['NA'])[0]
            mention.annotations.role = \
                (mention_ref.get_taxonomy(entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES) or ['NA'])[0]
        sentence.annotations.verbs = sentence_ref.verbs
    if f_fixes:
        f_fixes.close()

    #print stats_not_found, stats_ambiguous, stats_match_ok
    return doc_new


def main():
    logging.basicConfig(level=logging.DEBUG)
    settings.SERIALIZE_PRETTY_JSON_BY_DEFAULT = True
    file_path = "/Users/josepvalls/voz2/data/"
    story_file = "TestInput.txt"
    doc = create_document_from_raw_text(open(file_path+story_file).read())
    print doc
    print parse_tree_mention_helper.get_mention_nodes(doc.sentences[0].mentions[0],doc.sentences[0])

    import formatter
    file_name = 'temp_document.json'
    #doc.serialize_to_file(file_name,use_deep_copy=False)
    open('test_output.html','w').write(formatter.html(
        formatter.VozHTMLFormatter.format(doc,options={
            'include_parse':True,
            'parse_highlight':parse_tree_mention_helper.HIGHLIGHT_MENTIONS_INDEPENDNET,
            'include_raw':False,
            'include_text':True,
            'text_highlight':parse_tree_mention_helper.HIGHLIGHT_MENTIONS_INDEPENDNET,
            'include_mentions':False,
            'include_verbs':True,
        })
    ))
    print voz.Document.format_stats(doc.get_stats())
    print doc.sentences[0].tokens[2]
    print doc.sentences[1].tokens[4]
    print doc.get_mention_by_token_id(doc.sentences[0].tokens[2].id)
    print doc.get_mention_by_token_id(doc.sentences[1].tokens[4].id)
    mentions = util.object_list_to_dict(doc.get_all_mentions())
    print mentions[4]




if __name__ == '__main__':
    main()