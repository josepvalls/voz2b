import logging
#logging.basicConfig(format='%(levelname)s:\t%(message)s', level=logging.ERROR)
#logging.basicConfig(format='%(levelname)s:\t%(message)s', level=logging.WARN)
#logging.basicConfig(format='%(levelname)s:\t%(message)s', level=logging.CRITICAL)
#logging.basicConfig(format='%(levelname)s:\t%(message)s', level=logging.INFO)
logging.basicConfig(format='%(levelname)s:\t%(message)s', level=logging.DEBUG)


DATA_LOCAL_CACHE_DB_FILE = '/Users/josepvalls/voz2/data/networkcache.db'
#DATA_LOCAL_CACHE_DB_FILE = '/Users/josepvalls/voz-nlp/cache.db'
DATA_DISABLE_CACHING = False
DATA_USE_STANFORD_SYSTEM_LOCAL = True
DATA_USE_STANFORD_SYSTEM_LOCAL_ALT = False
DATA_USE_STANFORD_SYSTEM_LOCAL_URL = 'http://localhost:8888'
DATA_USE_STANFORD_SYSTEM_LOCAL_URL_ALT = 'http://centos:8888'
DATA_USE_STANFORD_SYSTEM_MIRROR_URL = 'http://parserservices.appspot.com'

SERIALIZE_PRETTY_JSON_BY_DEFAULT = False # slower and unpickable


STY_FILE_PATH = "/Users/josepvalls/voz2/stories/annotation-finlayson-01/"
#STY_FILE_PATH = "/Users/josepvalls/voz2/stories/annotation-finlayson-02/"
STY_FILES = ['01 - Nikita the Tanner.sty','02 - The Magic Swan Geese.sty','03 - Bukhtan Bukhtanovich.sty','04 - The Crystal Mountain.sty','05 - Shabarsha the Laborer.sty','06 - Ivanko the Bear\'s Son.sty','07 - The Runaway Soldier and the Devil.sty','08 - Frolka Stay-at-Home.sty','09 - The Witch.sty','10 - The Seven Simeons.sty','11 - Ivan Popyalov.sty','12 - The Serpent and the Gypsy.sty','13 - Prince Danila Govorila.sty','14 - The Merchant\'s Daughter and the Maidservant.sty','15 - Dawn, Evening and Midnight.sty']
STY_FILES += ['16 - 05b.Wee Little Havroshechka 2.sty','17 - 09a.Prince Ivan and Princess Martha 2.sty','18 - 20a.Rolling Pea 2.sty','19 - 08a.Nodey, the Priest\'s Grandson 2.sty','20 - 05a.The Soldier and the Princess 2.sty']

STORY_TXT_PATH = "/Users/josepvalls/voz2/stories/dialog_filtered/"

STY_ENTITY_TO_KEY = 'finlayson_sty_coref_group_role_dict.tsv' # outdated
STY_KEY_TO_ROLE = 'all_coreferenced-entities.csv' # outdated
STY_GT_ROLES = 'all_coreferenced-entities.tsv.csv'

STORY_ALL_SENTENCES = '/Users/josepvalls/voz2/stories/annotation-finlayson-01/all_sentences.tsv'

FEATURE_MANAGER_COMPUTED_FEATURES_PATH = '/Users/josepvalls/voz2/data/t_features/'
FEATURE_TSV_FILES = '/Users/josepvalls/voz2/data/t_tsv/'
COREFERENCE_JSON = '/Users/josepvalls/voz2/data/t_coref/'
STORY_ANNOTATION_FIXES = '/Users/josepvalls/voz2/data/t_annotation_fixes/'

FEATURE_MANAGER_USE_TOKEN_WEIGHTS_FOR_FEATURE_VALUE = False # weights not computed yet

RESOURCE_GENDER_COMMON_NAMES = 'data/res-gender-words.txt'


SENTIWORDNET_ENABLED = False
SENTIWORDNET_FILE = None
PHRASAL_VERB_FILE = None