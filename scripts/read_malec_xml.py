from bs4 import BeautifulSoup

f = "/Users/josepvalls/Desktop/propp/projects-voz/annotation-malec-00/AfanPFTMLCorpus.xml/Corpus.xml"
f = "/Users/josepvalls/Desktop/propp/Corpus-Translated-WorldLingo.xml"

d = BeautifulSoup(open(f).read(), 'xml')

for tale in d.select('Folktale'):
    print tale.attrs.get('Title'),tale.attrs.get('NewAfanasievEditionNumber'),tale.attrs.get('AT')



