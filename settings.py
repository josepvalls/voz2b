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
DATA_USE_STANFORD_SYSTEM_LOCAL_ALT = True
DATA_USE_STANFORD_SYSTEM_LOCAL_URL = 'http://localhost:8888'
DATA_USE_STANFORD_SYSTEM_LOCAL_URL_ALT = 'http://centos:8888'
DATA_USE_STANFORD_SYSTEM_MIRROR_URL = 'http://parserservices.appspot.com'

SERIALIZE_PRETTY_JSON_BY_DEFAULT = False # slower and unpickable


STY_FILE_PATH = "/Users/josepvalls/voz2/stories/annotation-finlayson-01/"
STY_FILES = ['01 - Nikita the Tanner.sty','02 - The Magic Swan Geese.sty','03 - Bukhtan Bukhtanovich.sty','04 - The Crystal Mountain.sty','05 - Shabarsha the Laborer.sty','06 - Ivanko the Bear\'s Son.sty','07 - The Runaway Soldier and the Devil.sty','08 - Frolka Stay-at-Home.sty','09 - The Witch.sty','10 - The Seven Simeons.sty','11 - Ivan Popyalov.sty','12 - The Serpent and the Gypsy.sty','13 - Prince Danila Govorila.sty','14 - The Merchant\'s Daughter and the Maidservant.sty','15 - Dawn, Evening and Midnight.sty']

STORY_TXT_PATH = "/Users/josepvalls/voz2/stories/dialog_filtered/"

STY_ENTITY_TO_KEY = 'finlayson_sty_coref_group_role_dict.tsv'
STY_KEY_TO_ROLE = 'all_coreferenced-entities.csv'
