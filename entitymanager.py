import vozbase
import re
import util
import logging
import collections
logger = logging.getLogger(__name__)
import numpy as np
import collections
import itertools



class TaggableContainer(object):
    """
    Enables tags to be added and queries
    """
    TAG_WORDNET_SENSE = 'WN'
    TAG_CHARACTER_SYMBOL = 'SYMBOL'
    def __init__(self):
        self.tag_labels = {}
    def set_tag(self,tag_class,tag_labels):
        if not tag_class in self.tag_labels:
            self.tag_labels[tag_class] = []
        self.tag_labels[tag_class] = tag_labels
    def get_tag(self,tag_class):
        if not tag_class in self.tag_labels:
            return []
        else:
            return self.tag_labels[tag_class]
    def add_tag(self,tag_class,tag_label):
        if not tag_class in self.tag_labels:
            self.tag_labels[tag_class] = []
        self.tag_labels[tag_class].append(tag_label)
    def format_all_tags(self):
        return ';'.join([','.join(v) for k,v in sorted(self.tag_labels.items())])
    def format_tags(self,tags):
        return ';'.join([','.join(v) for v in [self.tag_labels.get(k,[]) for k in tags]])


class TaxonomyContainer(object):
    """
    Enables taxonomies to be added and queries
    """
    TAXONOMY_ENTITY_TYPES = 'TYPE'
    TAXONOMY_NONCHARACTER = 'CNC'
    TAXONOMY_CHARACTER_6ROLES = 'ROLE6'
    TAXONOMY_CHARACTER_3ROLES = 'ROLE3'
    def __init__(self):
        self.taxonomy_labels = {}
    def set_taxonomy(self, taxonomy_class, taxonomy_labels):
        for taxonomy_label in taxonomy_labels:
            assert taxonomy_label in taxonomy_dict[taxonomy_class].labels
        if not taxonomy_class in self.taxonomy_labels:
            self.taxonomy_labels[taxonomy_class] = []
        self.taxonomy_labels[taxonomy_class] = taxonomy_labels
    def get_taxonomy(self, taxonomy_class):
        if not taxonomy_class in self.taxonomy_labels:
            return []
        else:
            return self.taxonomy_labels[taxonomy_class]
    def add_taxonomy(self,taxonomy_class,taxonomy_label):
        assert taxonomy_label in taxonomy_dict[taxonomy_class].labels, taxonomy_label
        if not taxonomy_class in self.taxonomy_labels:
            self.taxonomy_labels[taxonomy_class] = []
        self.taxonomy_labels[taxonomy_class].append(taxonomy_label)
    def format_all_taxonomies(self):
        return ';'.join([','.join(v) for k,v in sorted(self.taxonomy_labels.items())])


class TaxonomyItems(object):
    def __init__(self,id,labels,descriptions):
        self.id = id
        self.labels=labels
        self.desciptions=descriptions
    def __str__(self):
        return "Classes: %d, Values:\n %s" % (len(self.labels), '\n '.join(["%s: %s" % i for i in zip(self.labels,self.desciptions)]))


taxonomy_dict = {} # type: dict(string,TaxonomyItems)
taxonomy_dict_aux_type_to_parent = {} # dict(tuple(int,str),str)

def _init_taxonomy_dict():
    items = []
    descriptions = []
    parent = None
    for line in open('data/LabelTaxonomyTypes.txt').readlines():
        item = ''.join(re.findall('[A-Z]',line))
        if line.strip():
            if not line.startswith('    '):
                parent = item
            else:
                taxonomy_dict_aux_type_to_parent[(TaxonomyContainer.TAXONOMY_NONCHARACTER,item)]=parent
                items.append(item)
                descriptions.append(line.strip())
    taxonomy_dict[TaxonomyContainer.TAXONOMY_ENTITY_TYPES] = TaxonomyItems(TaxonomyContainer.TAXONOMY_ENTITY_TYPES, items, descriptions)
    taxonomy_dict[TaxonomyContainer.TAXONOMY_NONCHARACTER] = TaxonomyItems(TaxonomyContainer.TAXONOMY_NONCHARACTER, ['CH', 'NC'], ['Character', 'Non-Character'])
    items = []
    parents = []
    for line in open('data/LabelTaxonomyRoles.txt').readlines():
        a,b = line.strip().split('\t')
        items.append(a)
        if b not in parents: parents.append(b)
        taxonomy_dict_aux_type_to_parent[(TaxonomyContainer.TAXONOMY_CHARACTER_3ROLES,a)]=b
    taxonomy_dict[TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES] = TaxonomyItems(TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES, items, items)
    taxonomy_dict[TaxonomyContainer.TAXONOMY_CHARACTER_3ROLES] = TaxonomyItems(TaxonomyContainer.TAXONOMY_CHARACTER_3ROLES, parents, parents)

_init_taxonomy_dict()




class Entity(vozbase.VozContainer,TaxonomyContainer,TaggableContainer):
    """
    Models an entity (a referent).
    """
    def __init__(self,id,representation,symbol=None):
        """
        :param id: int
        :param representation: str
        :param symbol: str
        """
        #super(Entity,self).__init__()
        vozbase.VozContainer.__init__(self)
        TaxonomyContainer.__init__(self)
        TaggableContainer.__init__(self)
        self.id = id
        self.representation = representation
        self.symbol = symbol
        self._parent_document = None #type: voz.Document

    def _compute_caches(self,parent):
        self._parent_document = parent

    def _clear_caches(self,parent):
        del self._parent_document

    def distinct_coref_groups(self,tag_to_compare_against=TaggableContainer.TAG_CHARACTER_SYMBOL):
        groups_found = []
        for coref in self._parent_document.coreference.get_coreference_groups():
            for mention in coref.mentions:
                if self.symbol in mention.get_tag(tag_to_compare_against):
                    groups_found.append(coref)
                    break
        if len(groups_found)==0:
            logger.warning("Entity not found in any coreference group")
        elif len(groups_found)>1:
            logger.warning("Entity found in more than one coreference group")
        return groups_found
    def number_of_distinct_coref_groups(self,tag_to_compare_against=TaggableContainer.TAG_CHARACTER_SYMBOL):
        return len(self.distinct_coref_groups(tag_to_compare_against))

    def __str__(self):
        return "Entity %d - %s, %s (%d mentions)" % (self.id,self.representation,self.format_all_taxonomies(),len(self._parent_document.coreference.get_coreference_group_by_id(self.id).mentions))
    @classmethod
    def filter_by_taxonomy(cls,entities,taxonomy,value):
        return [i for i in entities if value in i.get_taxonomy(taxonomy)]
    @classmethod
    def filter_characters(cls,entities):
        return cls.filter_by_taxonomy(entities,TaxonomyContainer.TAXONOMY_NONCHARACTER,'CH')


class Mention(vozbase.VozContainer,TaxonomyContainer,TaggableContainer):
    """
    Models a single mention (a referring expression).
    """
    def __init__(self,id,tokens,tag_labels={}):
        """
        :param id: int
        :param tokens: voz.Token
        :param tag_labels: dict(str,list[str])
        """
        #super(Mention,self).__init__()
        vozbase.VozContainer.__init__(self)
        TaxonomyContainer.__init__(self)
        TaggableContainer.__init__(self)
        self.id = id
        self.tokens = tokens #type: voz.Token
        self.is_list = False
        self.is_compound = False
        self.is_independent = False
        self.parent_mention = None
        self.child_mentions = []

        self._coref_group = None

        self._text = ''
        self._compute_caches(self)

    def _compute_caches(self,parent):
        if self.tokens:
            _text_start = self.tokens[0].offset
            _text_end = self.tokens[-1].offset + self.tokens[-1].len
            self._text = self.tokens[0]._parent_sentence._parent_document.text[_text_start:_text_end]
            self._text = re.sub('\s+',' ',self._text)

    def _clear_caches(self,parent):
        del self._text

    def get_coref_group(self):
        return self.tokens[0]._parent_sentence._parent_document.coreference.get_coreference_group_by_id(self.get_coref_group_id())

    def get_coref_group_id(self):
        """
        :return: int
        """
        if self._coref_group:
            return self._coref_group
        else:
            return self.id
    def get_text(self):
        if not self._text:
            self._compute_caches()
        return self._text
    def to_new_entity(self,id_=None):
        if id_ is None: id_=self.id
        entity = Entity(id_,self.get_text()) #type: Entity
        entity._compute_caches(self.tokens[0]._parent_sentence._parent_document)
        return entity
    def __str__(self):
        return "Mention %d - %s, %s - %s (%s%s)" % (self.id, self.get_text(), self.format_all_taxonomies(),
                                                    #self.format_tags([TaggableContainer.TAG_CHARACTER_SYMBOL,TaggableContainer.TAG_CHARACTER_SYMBOL_OLD]),
                                                    self.format_tags([TaggableContainer.TAG_CHARACTER_SYMBOL,'OLD_SYMBOL','OLD_ROLE_GT',]),
                                                    ('I' if self.is_independent else ''),
                                                    ('L' if self.is_list else ''))
    def contains_mention(self,mention):
        # TODO fix some mentions marked as independent but contain other mentions!!!
        if False and self.is_independent:
            return False
        else:
            for token in mention.tokens:
                if token not in self.tokens:
                    return False
            return True
    def get_tokens(self):
        return self.tokens



class CoreferenceGroup(vozbase.VozContainer):
    """
    Models a set of mentions referring to an (optional) entity.
    """
    def __init__(self,id,mentions,entity=None,representation=None):
        super(CoreferenceGroup,self).__init__()
        assert isinstance(entity,Entity)
        if id:
            self.id = id
        elif not id and entity:
            self.id = entity.id
        elif not id and mentions:
            self.id = mentions[0].id
        else:
            self.id = -1
        if not representation and entity:
            representation = entity.representation
        self.mentions = mentions #type: list[Mention]
        self.entity = entity
        self._representation = representation
        self._update_mentions()
    def _update_mentions(self,mentions=[]):
        """
        :param mentions: list[Mention]
        :return: None
        """
        if mentions:
            self.mentions = mentions
        for i in self.mentions:
            i._coref_group = self.id
    def get_representation(self):
        if self.entity:
            return self.entity.representation
        else:
            return self._representation
    def get_tag(self,tag_class):
        return self.get_all('get_tag',tag_class)
    def get_taxonomy(self,taxonomy_class):
        return self.get_all('get_taxonomy',taxonomy_class)
    def get_all(self,property,target,collection='mentions'):
        lst = []
        for item in getattr(self,collection):
            lst += getattr(item,property)(target)
        return lst
    def get_taxonomy_voting(self,taxonomy=None):
        """
        If no taxonomy is specified, do majority voting for all possible taxonomies
        :param taxonomy: string
        :return: None
        """
        # TODO
    def get_tag_voting(self,tag):
        tags = util.flatten([i.get_tag(tag) for i in self.mentions])
        return collections.Counter(tags).most_common(1)[0][0]
    def contains_a_character(self):
        return True if self.filter_characters(self.mentions) else False
    def number_of_distinct_characters(self,tag_to_compare_against=TaggableContainer.TAG_CHARACTER_SYMBOL):
        characters = [mention.get_tag(tag_to_compare_against) for mention in self.mentions]
        characters = set(util.flatten(characters))
        return len(characters)
    @classmethod
    def filter_by_taxonomy(cls,entities,taxonomy,value):
        return [i for i in entities if value in i.get_taxonomy(taxonomy)]
    @classmethod
    def filter_characters(cls,entities):
        return cls.filter_by_taxonomy(entities,TaxonomyContainer.TAXONOMY_NONCHARACTER,'CH')


class Coreference(vozbase.VozContainer):
    """
    Models coreference
    """
    def __init__(self,document,label='Default',entities=None,coreference_groups=None):
        self.id = document.get_new_id(Coreference)
        self.document = document
        self.entities = entities or [] #type: list[entitymanager.Entity]
        self.coreference_groups = coreference_groups or [] #type: list[entitymanager.CoreferenceGroup]

        self._coreference_dict = {} #type: dict(int,entitymanager.Coreference)

    def create_coref_group_and_entity_from_mentions(self,id,symbol,mentions):
        entity = Entity(id,mentions[0].get_text())
        entity.symbol = symbol
        entity._compute_caches(self.document)
        #group = CoreferenceGroup(id,mentions,entity)
        group = CoreferenceGroup(id,mentions,None)
        role = group.get_tag_voting('OLD_ROLE_PRED1')
        #self.entities.append(entity)
        self.coreference_groups.append(group)
        logger.debug("Creating entity %s and coref group for %d,%s with %d mentions" % (mentions[0].get_text(),id,symbol,len(mentions)))
        entity.add_taxonomy(TaxonomyContainer.TAXONOMY_NONCHARACTER,'CH' if not role=='NA' else 'NC')

    def add_coreference_group(self,id_,mentions,entity=None,representation=None):
        self.coreference_groups.append(CoreferenceGroup(id_,mentions,entity,representation))

    def get_stats_based_on_entities(self,tag_to_compare_against=TaggableContainer.TAG_CHARACTER_SYMBOL):
        characters = Entity.filter_characters(self.entities)
        characters_uniq = set([i.symbol for i in characters if ',' not in i.symbol])
        coref_groups_with_characters = [i for i in self.coreference_groups if i.contains_a_character()]
        coref_groups_with_characters_c_gr = [i.number_of_distinct_characters(tag_to_compare_against) for i in coref_groups_with_characters]
        coref_groups_per_character_gr_c = [i.number_of_distinct_coref_groups(tag_to_compare_against) for i in characters]
        return [len(self.entities),
                len(characters),
                len(characters_uniq),
                sum([len(self.get_coreference_group_by_id(i.id).mentions) for i in self.entities]),
                sum([len(self.get_coreference_group_by_id(i.id).mentions) for i in characters]),
                sum([len(self.get_coreference_group_by_id(i.id).mentions) for i in characters])/len(characters) if characters else 0,
                len(self.coreference_groups),
                len(coref_groups_with_characters),
                util.average(coref_groups_with_characters_c_gr),
                util.average(coref_groups_per_character_gr_c)
        ]
    def number_of_distinct_coref_groups(self,symbol):
        groups_found = []
        for coref in self.get_coreference_groups():
            for mention in coref.mentions:
                if symbol in mention.get_tag(TaggableContainer.TAG_CHARACTER_SYMBOL):
                    groups_found.append(coref)
                    break
        if len(groups_found)==0:
            logger.warning("Entity not found in any coreference group")
        elif len(groups_found)>1:
            logger.warning("Entity found in more than one coreference group")
        return len(groups_found)


    def get_stats(self,tag_to_compare_against=TaggableContainer.TAG_CHARACTER_SYMBOL):
        characters = [i for i in self.document.get_all_mentions() if i.is_independent and 'CH' in i.get_taxonomy(TaxonomyContainer.TAXONOMY_NONCHARACTER)]
        characters_uniq = set(util.flatten([i.get_tag(TaggableContainer.TAG_CHARACTER_SYMBOL) for i in characters]))
        coref_groups_with_characters = [i for i in self.coreference_groups if i.contains_a_character()]
        coref_groups_with_characters_c_gr = [i.number_of_distinct_characters(tag_to_compare_against) for i in coref_groups_with_characters]
        coref_groups_per_character_gr_c = [self.number_of_distinct_coref_groups(i) for i in characters_uniq]
        #coref_groups_per_character_gr_c = [i.number_of_distinct_coref_groups(tag_to_compare_against) for i in characters]
        return [len(self.entities),
                len(characters),
                len(characters_uniq),
                sum([len(self.get_coreference_group_by_id(i.id).mentions) for i in self.entities]),
                #sum([len(self.get_coreference_group_by_id(i.id).mentions) for i in characters]),
                #sum([len(self.get_coreference_group_by_id(i.id).mentions) for i in characters])/len(characters) if characters else 0,
                len(self.coreference_groups),
                len(coref_groups_with_characters),
                util.average(coref_groups_with_characters_c_gr),
                util.average(coref_groups_per_character_gr_c)
        ]


    @classmethod
    def get_stats_labels(cls):
        return ["Num. Entities",
                "Num. Characters",
                "Num. Uniq. Characters",
                "Num. Mentions",
                #"Num. Mentions to Characters",
                #"Avg. Men. per Character",
                "Num. of Coref Groups",
                "Num. of Coref Groups with Characters",
                "Avg. C/Gr",
                "Avg. Gr/C",
                ]
    @classmethod
    def format_stats(cls,stats):
        return '\n'.join(["%s: %.2f" % i for i in zip(cls.get_stats_labels(),stats)])



    @classmethod
    def eval_prf(cls,coref_key,mentions):
        stats_characters = mentions
        stats_characters_uniq = set(util.flatten([i.get_tag(TaggableContainer.TAG_CHARACTER_SYMBOL) for i in mentions]))

        data = mentions
        table = np.zeros((len(data),len(data)))
        table_gt = np.zeros((len(data),len(data)))
        groups = collections.defaultdict(set)
        groups_mentions = collections.defaultdict(set)
        groups_gt = collections.defaultdict(set)
        mention_ids = []
        for mention in data:
            mention_id = mention.id
            coref_group = mention.get_tag(coref_key)
            if not coref_group:
                logger.warning("Mention %s does not have a prediction for coreference" % mention)
                continue
            coref_group = tuple(coref_group)
            coref_group_gt = tuple(mention.get_tag(TaggableContainer.TAG_CHARACTER_SYMBOL))
            mention_ids.append(mention_id)
            groups[coref_group].add(mention_id)
            groups_mentions[coref_group].add(mention)
            groups_gt[coref_group_gt].add(mention_id)
        for group_id,group in groups.items():
            for pair in itertools.combinations(group,2):
                if pair[0]==pair[1]: continue
                table[mention_ids.index(pair[0]),mention_ids.index(pair[1])]=1
                table[mention_ids.index(pair[1]),mention_ids.index(pair[0])]=1
        for group_id,group in groups_gt.items():
            for pair in itertools.combinations(group,2):
                if pair[0]==pair[1]: continue
                table_gt[mention_ids.index(pair[0]),mention_ids.index(pair[1])]=1
                table_gt[mention_ids.index(pair[1]),mention_ids.index(pair[0])]=1
        p1 = np.sum(table)
        r1 = np.sum(table_gt)
        count1 = r1
        intersect1 = np.sum(np.multiply(table,table_gt))
        p1 = 1.0 * intersect1/p1
        r1 = 1.0 * intersect1/r1
        f1 = 2.0 * p1 * r1 / (p1+r1)
        p0 = np.sum(np.logical_not(table))
        r0 = np.sum(np.logical_not(table_gt))
        count0 = r0
        intersect0 = np.sum(np.multiply(np.logical_not(table),np.logical_not(table_gt)))
        p0 = 1.0 * intersect0/p0
        r0 = 1.0 * intersect0/r0
        f0 = 2.0 * p0 * r0 / (p0+r0)
        logger.info(util.string_as_print("PRF1",p1,r1,f1,"PRF0",p0,r0,f0))

        #normalization = 1.0 * (len(data)**2)/(total_normalization**2)
        #normalization = 1.0 * len(data)/total_normalization
        normalization = len(data)**2 - len(data)



        def count_groups_for_symbol(groups,symbol):
            count = 0
            for group in groups:
                for mention in group:
                    if symbol in mention.get_tag(TaggableContainer.TAG_CHARACTER_SYMBOL):
                        count +=1
                        break
            if count == 0:
                logger.error("Character not found in any group %s" %symbol)
            # There is a max because some times a symbol is not found in any group?
            return max(1,count)
        def count_characters_per_group(group):
            count = len(set([i.get_tag(TaggableContainer.TAG_CHARACTER_SYMBOL)[0] for i in group]))
            if not count == 1:
                logger.error("Character not found in group is %d" %count)
            return count
        stats_coref_groups_with_characters_c_gr = [count_characters_per_group(i) for i in groups_mentions.values()]
        stats_coref_groups_per_character_gr_c = [count_groups_for_symbol(groups_mentions.values(),i) for i in stats_characters_uniq]

        return_data = [normalization*p1,normalization*r1,normalization*f1,normalization*p0,normalization*r0,normalization*f0,len(data),len(data)**2,count1,count0]+[
                len(stats_characters),
                len(stats_characters_uniq),
                len(groups),
                util.average(stats_coref_groups_with_characters_c_gr),
                util.average(stats_coref_groups_per_character_gr_c)]
        return_data.append(util.compute_f(return_data[-2],return_data[-1]))
        return table,return_data








    def get_coreference_groups(self):
        """

        :return: list[entitymanager.CoreferenceGroup]
        """
        return self.coreference_groups

    def remove_mention(self,mention):
        removed = 0
        coref_to_remove = []
        for coref in self.coreference_groups:
            while True:
                try:
                    coref.mentions.remove(mention)
                    removed += 1
                except ValueError:
                    break
            if len(coref.mentions)==0:
                coref_to_remove.append(coref)

        logger.info("Removing mention %s, removed %d instances in coref" % (mention,removed))
        if coref_to_remove:
            for i in coref_to_remove:
                self.remove_coref_and_entity(i.id)
            logger.info("Removing mention %s, removed %d coref groups" % (mention,len(coref_to_remove)))

        # invalidate cache
        self.document._token_to_mention_dict = None

    def remove_coref_and_entity(self,remove_id):
        logger.info("Removing coref and entity id %d" % remove_id)
        self.coreference_groups[:] = [i for i in self.coreference_groups if not i.id == remove_id]
        self.entities[:] = [i for i in self.entities if not i.id == remove_id]

        # invalidate cache
        self.document._coreference_dict = None
    def _clear_caches(self,parent):
        del self._coreference_dict
        for i in self.entities:
            i._clear_caches(parent)
    def _compute_caches(self,parent):
        for i in self.entities:
            i._compute_caches(parent)

    def get_coreference_group_by_id(self,id):
        if not self._coreference_dict:
            self._coreference_dict = util.object_list_to_dict(self.get_coreference_groups())
        return self._coreference_dict.get(id,None)


def main():
    for k,v in taxonomy_dict.items():
        print k,v
    print "Mapping from Taxonomies: ",taxonomy_dict_aux_type_to_parent

if __name__=='__main__':
    main()