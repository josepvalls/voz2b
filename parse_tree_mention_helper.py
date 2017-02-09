import settings
import voz
import util
from nltk.tree import ParentedTree
import logging
import stanfordhelper

logger = logging.getLogger(__name__)

MODE_STANFORD = 1

HIGHLIGHT_COREF = 'coref'
HIGHLIGHT_MENTIONS = 'mentions'
HIGHLIGHT_MENTIONS_INDEPENDNET ='mentions_independent'

class TreeParser(object):
    def get_mentions(self,parse,sentence,document):
        """
        :param parse: nltk.tree.ParentedTree
        :param sentence: voz.Sentence
        :param document: voz.Document
        :return: list[voz.entitymanager.Mention]
        """
        assert isinstance(parse,ParentedTree)
        assert isinstance(sentence,voz.Sentence)
        assert isinstance(document,voz.Document)
        assert len(parse.leaves()) is len(sentence.tokens)

        mentions = []
        color_map_mentions = {}
        color_map_mentions_independent = {}

        def traverse(node,parent,sentence):
            for child in node:
                if not isinstance(child,ParentedTree): continue
                if self.any_noun_in_tree(child):
                    mention = voz.entitymanager.Mention(document.get_new_id(voz.entitymanager.Mention),child.leaves())
                    mention.is_compound = self.any_phrase_in_tree(child)
                    mention.is_list = self.any_list_in_tree(child)
                    mention.is_independent = not mention.is_compound and not mention.is_list and not (len(mention.tokens) == 1 and mention.tokens[0].pos == "PRP$")
                    mention.parent_mention = parent
                    mention._compute_caches(sentence)
                    if parent:
                        parent.child_mentions.append(mention)
                    mentions.append(mention)
                    color_map_mentions[tuple(child.treeposition())]=mention.id
                    if mention.is_independent:
                        color_map_mentions_independent[tuple(child.treeposition())]=mention.id
                    if not mention.is_independent:
                        traverse(child,mention,sentence)
        traverse(parse,None,sentence)

        sentence.parse_highlight[HIGHLIGHT_MENTIONS]=color_map_mentions
        sentence.parse_highlight[HIGHLIGHT_MENTIONS_INDEPENDNET]=color_map_mentions_independent

        assert mentions[0].parent_mention is None
        assert len([i for i in mentions if i.parent_mention==None]) is 1

        return mentions
    def any_noun_in_tree(self,tree):
        return self.any_in_tree(tree,self.get_tags_nouns())
    def any_phrase_in_tree(self,tree):
        return self.any_in_tree(tree,self.get_tags_phrase())
    def any_list_in_tree(self,tree):
        return self.any_in_tree(tree,self.get_tags_list())
    def any_in_tree(self,tree,labels):
        iter = tree.subtrees(lambda node:node.label() in labels)
        return not next(iter,util.SentinelValue) == util.SentinelValue


def get_mention_nodes(mention, sentence):
    '''
    Get nodes for features
    :param sentence: voz.Sentence
    :return: (mention_nodes, mention_from_nodes )
    '''
    if not sentence.parse_highlight or not sentence.parse_highlight[HIGHLIGHT_MENTIONS]:
        logger.warning("NO PARSE HIGHLIGHT INFORMATION IN SENTENCE")
        return ([],[])
    id_to_parse = dict([(v,k) for k,v in sentence.parse_highlight[HIGHLIGHT_MENTIONS].items()])
    for mention in sentence.mentions:
        if mention.id not in id_to_parse:
            logger.warning("NO PARSE HIGHLIGHT INFORMATION FOR MENTION")
            return ([],[])
        node = sentence.parse_tree[id_to_parse[mention.id]]

        parents = [i.label() for i in get_node_parents(node)]
        #children = [i.pos for i in node.leaves()] + [i.label() for i in get_subtree_nodes(node, True)]
        # the POS for the leaves are also labels in the tree
        children = [i.label() for i in get_subtree_nodes(node, True)]
        return children, parents
def get_subtree_nodes(node, exclude_root=False):
    children = [] if exclude_root else [node]
    def traverse(node):
        for child in node:
            if not isinstance(child, ParentedTree): continue
            children.append(child)
            traverse(child)
    traverse(node)
    return children

def get_node_parents(node):
    parents = []
    while node and node.parent():
        node = node.parent()
        parents.append(node)
    return parents


class StanfordTreeParser(TreeParser):
    def get_tags_nouns(self):
        return ['NP','NN','NNP','NNS','NNPS'] # What to do with 'PP' 'ADJP' 'ADVP'
    def get_tags_list(self):
        return [',','CC']
    def get_tags_phrase(self):
        return ['SBAR','S','VP','SINV','PP', 'AJDP','SBARQ','SQ']
        # FIXME: adding PP breaks some good NP like [box of jewels] [master of the ship] [folks of the world] [the hollow of his hand]... but fixes [widower with his daughter]

def main():
    logging.basicConfig(level=logging.DEBUG)
    settings.SERIALIZE_PRETTY_JSON_BY_DEFAULT = True
    #doc = stanfordhelper.create_document_from_raw_text("The master of the ship found a box of jewels containing diamonds, silver and gold which were lost by a widower with his daughter and small son who lived in the hollow of his hand.")
    doc = stanfordhelper.create_document_from_raw_text("Alice, Bob and Charlie are friends and they have lots of fun and it is nice.")
    print [(i,i.get_text()) for i in doc.sentences[0].mentions if i.is_independent]
    print [(i,i.get_text()) for i in doc.sentences[0].mentions]
    print [i.format({'display_tokens':['lemma','pos','idx']}) for i in doc.sentences[0].tokens]
    #open('test_output.html','w').write(formatter.html(formatter.VozHTMLFormatter.format(doc,options={'parse_highlight':'mentions_independent'})))
    #import pprint
    #pprint.pprint(doc.sentences[0].parse_highlight)
    #open('test_output.html','w').write(formatter.html(formatter.VozHTMLFormatter.format(doc.sentences[0].parse_tree,options={'color_map':doc.sentences[0].parse_highlight['mentions_independent']})))



if __name__ == '__main__':
    main()