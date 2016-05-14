from bs4 import BeautifulSoup, Comment, Tag

malec_mapping = {
'ConsentToCounteraction':   'C',
'DonorFunction':            'D',
'Transference':             'G',
'CommandExecution':         None, # encompasses gamma and delta
'Return':                   'return',
'HeroReaction':             'E',
'RescueOfHero':             'Rs', #?
'Transfiguration':          'T',
'Absentation':              'beta',
'Struggle':                 'H',
'Wedding':                  'W',
'PunishmentOfFalseHero':    'U',
'ClaimsOfFalseHero':        'L',
'PrelimMisfortune':         'lambda',
'Epilogue':                 None, # narrator comments
'Task':                     '',
'Departure':                'depart',
'Preparation':              None, # encompasses the alpha-lambda
'UnrecognizedArrival':      'o',
'Command':                  'gamma', #<Command subtype="Interdiction"> gamma
'Victory':                  'I',
'PursuitRescueOfHero':      'Rs', #?
'PursuitOfHero':            'Pr',
'LiquidationOfLack':        'K',
'DeceitByVillain':          'eta',
'Execution':                'delta',  #<Execution subtype="Violated"> delta1 <Execution subtype="Followed"> delta2
'DifficultTask':            'M',
'Lack':                     'a',
'Villainy':                 'A',
'ConnectiveIncident':       'B',
'StruggleVictory':          'H',
'RecognitionOfHero':        'Q',
'SolutionOfTask':           'N',
'InitialSituation':         'alpha',
'AcquisitionOfMagicalAgent': 'F',
}


f = "/Users/josepvalls/Desktop/propp/projects-voz/annotation-malec-00/AfanPFTMLCorpus.xml/Corpus.xml"
f = "/Users/josepvalls/Desktop/propp/Corpus-Translated-WorldLingo.xml"

d = BeautifulSoup(open(f).read(), 'xml')

DEBUG = False

def recurse(node,depth):
    #for child in node.findAll(): # this doesn't capture the comments
    for child in node.contents:
        if isinstance(child,Tag):
            if DEBUG:
                print ' '*depth,child.name,child.attrs.get('subtype','')
            else:
                m = malec_mapping.get(child.name,None)
                if m:
                    print m
            recurse(child,depth+1)
        elif isinstance(child,Comment):
            print ' '*depth,"?",child


for tale in d.select('Folktale'):
    print tale.attrs.get('Title'),tale.attrs.get('NewAfanasievEditionNumber'),tale.attrs.get('AT')
    #print len(tale.select('Move'))
    recurse(tale,1)



