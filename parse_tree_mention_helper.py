import settings
import voz
import util
from nltk.tree import ParentedTree
import logging
import stanfordhelper
import formatter

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
                    mention.is_independent = not mention.is_compound and not mention.is_list
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
    open('test_output.html','w').write(formatter.html(formatter.VozHTMLFormatter.format(doc,options={'parse_highlight':'mentions_independent'})))
    #import pprint
    #pprint.pprint(doc.sentences[0].parse_highlight)
    #open('test_output.html','w').write(formatter.html(formatter.VozHTMLFormatter.format(doc.sentences[0].parse_tree,options={'color_map':doc.sentences[0].parse_highlight['mentions_independent']})))



if __name__ == '__main__':
    main()