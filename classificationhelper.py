import subprocess, os
import settings
import collections
import math
import coreferencehelper

TASK_COREF = 'COREF'
TASK_MYCOREF = 'MYCOREF'
TASK_CHARACTER = 'CH'
TASK_TYPE = 'TY'
TASK_ROLE = 'RO'

CLASSPATH = '/Users/josepvalls/Dropbox/projects/Voz-Java/build/classes'
VERBOSE_OUTPUT = False
DO_FORCE_LABEL_COMPUTE = False



def get_labels(file_in_use, docs):
    '''
    Used in the AAAI/ACL/IJCAI experiments
    Uses external Java and cross validation
    '''

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

def do_voting(docs, task, container='predictions', use_coref_annotations=False,filter_characters=True):
    if task == TASK_CHARACTER:
        pred_to_mention, anno_to_mention, _, _, _, _, _, _, _ = coreferencehelper.get_coref_stats(docs, None, stats=False)
        fields = ['is_character']
    else:
        pred_to_mention, anno_to_mention, _, _, _, _, _, _, _ = coreferencehelper.get_coref_stats(docs, stats=False, filter_characters=filter_characters)
        fields = ['type', 'role']

    if use_coref_annotations:
        groups = anno_to_mention
    else:
        groups = pred_to_mention
    for g in groups.values():
        for field in fields:
            labels = [getattr(getattr(i, container), field) for i in g]
            label = collections.Counter(labels).most_common()[0][0]
            for i in g:
                setattr(getattr(i, container), field, label)


def get_filename(what_file, iteration):
    return settings.FEATURE_TSV_FILES + 'all_coreferenced-entities-aaai-%s-%d.tsv' % (what_file, iteration)


def get_feature_weights():
    pass

_cache_data = {}
_cache_weights = {}

def get_label(feature_vector,task, n=5):
    if not task in _cache_data:
        data = []
        for line in open(get_filename(task, 0)).readlines():
            line = line.strip().split('\t')
            vector = [float(i) for i in line[1:-1]]
            label = line[-1]
            data.append((vector,label))
        _cache_data[task] = data
    if not task in _cache_weights:
        _cache_weights[task] = compute_weights(data)
    data = _cache_data[task] # type: [([float],str)]
    weights = _cache_weights[task]
    labels = sorted([(jaccard_distance_weights(feature_vector,i[0],weights),i[1]) for i in data])
    labels = [i[1] for i in labels]
    return collections.Counter(labels[0:n]).most_common()[0][0]

def get_distribution(lst):
    return collections.Counter([i[1] for i in lst])

def get_entropy(lst):
    total = sum(lst)
    accum = 0
    for l in lst:
        p = 1.0 * l / total
        accum += p * math.log(p, 2)
    return -accum

def get_entropy_from_distribution(d):
    return get_entropy(d.values())

def compute_weights(training):
    n = len(training[0][0])
    weights = [0]*n
    # learn the weights from the training set:
    for i in range(n):
        # Quinlan's gain:
        distribution = get_distribution(training)
        l0 = []
        l1 = []
        for c in training:
            if c[0][i]>=0.5:
                l1.append(c)
            else:
                l0.append(c)
        H = get_entropy_from_distribution(distribution)
        H0 = get_entropy_from_distribution(get_distribution(l0))
        H1 = get_entropy_from_distribution(get_distribution(l1))
        Q = H - (1.0*len(l0)*H0 + 1.0*len(l1)*H1)/(1.0*len(l0)+1.0*len(l1))
        weights[i] = Q
    return weights

def jaccard_distance_slow(a,b): #about 20% slower
    inter = sum([min(c,d) for c,d in zip(a[2:],b[2:])])
    union = sum([max(c,d) for c,d in zip(a[2:],b[2:])])
    return 1.0 - (inter/union) if union else 0.5

def jaccard_distance(a,b):
    inter,union=0.0,0.0
    for a_,b_ in zip(a,b):
        inter += min(a_,b_)
        union += max(a_,b_)
    return 1.0 - (inter/union) if union else 0.5

def jaccard_distance_weights(a,b,w):
    inter,union=0.0,0.0
    for a_,b_,w_ in zip(a,b,w):
        inter += min(a_,b_)*w_
        union += max(a_,b_)*w_
    return 1.0 - (inter/union) if union else 0.5


'''
ORIGINAL SANTI'S JAVA CODE FOR REFERENCE

public WeightedJaccard(List<Character> training,int verbose) {
        int n = training.get(0).attributes.length;
        weights = new double[n];

        // learn the weights from the training set:
        for(int i = 0;i<n;i++) {
            // Quinlan's gain:
            HashMap<String,Integer> distribution = getDistribution(training);
            List<Character> l0 = new LinkedList<Character>();
            List<Character> l1 = new LinkedList<Character>();
            for(Character c:training) {
                if (c.attributes[i]>=0.5) {
                    l1.add(c);
                } else {
                    l0.add(c);
                }
            }
            HashMap<String,Integer> distribution0 = getDistribution(l0);
            HashMap<String,Integer> distribution1 = getDistribution(l1);

            double H = entropy(distribution);
            double H0 = entropy(distribution0);
            double H1 = entropy(distribution1);

            double Q = H - (l0.size()*H0 + l1.size()*H1)/(l0.size()+l1.size());
            weights[i] = Q;
        }
    }

    public double entropy(HashMap<String,Integer> d) {
        double total = 0;
        double accum = 0;
        for(String l:d.keySet()) total+=d.get(l);
        for(String l:d.keySet()) {
            double p = d.get(l)/total;
            accum+=p*Math.log(p);
        }
        return -accum;
    }

    public HashMap<String,Integer> getDistribution(List<Character> l) {
        HashMap<String,Integer> distribution = new HashMap<String,Integer>();
        for(Character c:l) {
            String label = c.label;
            if (!distribution.containsKey(label)) distribution.put(label,0);
            distribution.put(label, distribution.get(label)+1);
        }

        return distribution;
    }
'''
