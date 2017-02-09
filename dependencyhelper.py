class Dependency(object):
    def __init__(self,d_type,governor,dependent):
        self.type,self.governor,self.dependent=d_type,governor,dependent
    def __repr__(self):
        return 'Dependency %s(%s, %s)' % (self.type, self.governor, self.dependent)
    @staticmethod
    def from_xml(xml,sentence, include_verbs):
        d_type = xml.attributes.get('type').value
        if d_type in ['nsubj', 'expl', 'nsubjpass', 'dobj', 'iobj', 'pobj'] and not include_verbs: return None
        idx = int(xml.getElementsByTagName('governor')[0].attributes.get('idx').value)
        governor = sentence.tokens[idx-1] if idx else None
        idx = int(xml.getElementsByTagName('dependent')[0].attributes.get('idx').value)
        dependent = sentence.tokens[idx-1] if idx else None
        return Dependency(d_type,governor,dependent)
    @staticmethod
    def unfold_copulae(dependencies):
        # TODO unfold with coreference?
        for dep in [i for i in dependencies if i.type=='cop']:
            d_type = 'cop_%s' % dep.dependent.lemma
            for subj in [i for i in dependencies if i.governor == dep.governor and i.type=='nsubj']:
                subj.type = d_type
                dependencies.append(Dependency('nsubj',dep.dependent,subj.dependent))
    @staticmethod
    def weight_negatives(dependencies):
        # TODO add weight to tokens?
        for dep in [i for i in dependencies if i.type=='neg']:
            dep.governor.weight *= -0.5
        return dependencies
    @staticmethod
    def dependency_filter(dependencies,start=0,end=-1,filter_val=None,filter_vals=[],field=None,filter_range='dependent'):
        """Start is included, end is not included, filter_range specifies what field to check for range"""
        return [getattr(i, field) if field else i for i in dependencies if 
                (start == 0 or getattr(i, filter_range).idx >= start) and 
                (end == -1 or getattr(i, filter_range).idx < end) and 
                ((filter_val == None and not filter_vals) or i.type in filter_vals + [filter_val] or (filter_val[-1]=='*' and i.type.startswith(filter_val[0:-1])))
                ]

            
            
