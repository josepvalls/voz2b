import vozbase
import settings
import voz
import logging
import os
import util
from bs4 import BeautifulSoup

logger = logging.getLogger(__name__)

voz.main()
voz.entitymanager.main()
voz.verbmanager.main()

import styhelper
styhelper.main()

import stanfordhelper
stanfordhelper.main()

import quotedspeechhelper
quotedspeechhelper.main()