import subprocess, os
import settings

CLASSPATH = '/Users/josepvalls/Dropbox/projects/Voz-Java/build/classes'
VERBOSE_OUTPUT = False
DO_FORCE_LABEL_COMPUTE = False
def get_labels(file_in_use, docs):
    fname = file_in_use + '.labels'
    docs_ = [str(doc.id) for doc in docs]
    if DO_FORCE_LABEL_COMPUTE or not os.path.isfile(fname):
        # '/Users/josepvalls/Desktop/Voz-Java/build/classes'
        cmd = ['java', '-cp', CLASSPATH, '-Xmx2048m', 'characters.PerformanceEvaluation2', file_in_use, ','.join(docs_),'']
        if VERBOSE_OUTPUT:
            print ' '.join(cmd)
        data = subprocess.check_output(cmd)
        with open(fname,'w') as f:
            f.write(data)
    else:
        data = open(fname).read()

    data = [line.strip().split('\t') for line in data.split(os.linesep)][0:-1]
    if VERBOSE_OUTPUT:
        count = 0
        for i in data:
            if i[-2] == i[-1]: count += 1
        print 'JAVA ACCURACY ', 1.0 * count / len(data)
    data = [i[-3:] for i in data]
    return data
