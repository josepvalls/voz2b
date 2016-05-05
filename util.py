import operator
import itertools
import math
import collections

def all_combinations_with_len(lst,min_len,max_len):
    for i in xrange(min_len,max_len+1):
         for j in list(itertools.product(*([lst]*i))):
            yield j

def powerset(iterable):
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    s = list(iterable)
    return itertools.chain.from_iterable(itertools.combinations(s, r) for r in range(len(s)+1))


def flatten(lst):
    if not lst:
        return lst
    if isinstance(lst[0], list):
        return reduce(operator.add, lst, [])
    elif isinstance(lst[0], set):
        return reduce(operator.or_, lst, set([]))
    else:
        raise


def remove_duplicates(lst):
    seen = set()
    result = []
    for i in lst:
        if i in seen: continue
        seen.add(i)
        result.append(i)
    return result

def union_list_without_duplicates(lst1,lst2):
    seen = set(lst1)
    result = list(lst1)
    for i in lst2:
        if i in seen: continue
        seen.add(i)
        result.append(i)
    return result


def remove_duplicates_and_intersect(lst,target_set):
    seen = set()
    result = []
    for item in lst:
        if item in seen: continue
        if item not in target_set: continue
        seen.add(item)
        result.append(item)
    return result



def clamp(minval, maxval, val):
    if val < minval: return minval
    if val > maxval: return maxval
    return val


def sum_lists(a, b):
    return [_a + _b for _a, _b in zip(a, b)]


def average(lst):
    """
    :param lst: list
    :return: float
    """
    if not lst: return 0.0
    return 1.0*sum(lst) / len(lst)


def count_iter(iter):
    """
    :param iter: iterator
    :return: int
    """
    return sum(1 for _ in iter)


def all_subclasses(cls, _seen=None):
    if not isinstance(cls, type):
        raise TypeError("all_subclasses must be called with "
                        "new-style classes, not %.100r" % cls)
    if _seen is None: _seen = set()
    try:
        subs = cls.__subclasses__()
    except TypeError:  # fails only when cls is type
        subs = cls.__subclasses__(cls)
    for sub in subs:
        if sub not in _seen:
            _seen.add(sub)
            yield sub
            for sub in all_subclasses(sub, _seen):
                yield sub


def chunks(l, n):
    for i in xrange(0, len(l), n):
        yield l[i:i + n]


def is_any_a_in_b(a,b):
    """
    Returns True if len(a&b)>0
    :param a: set
    :param b: set
    :return: bool
    """
    for i in a:
        if i in b:
            return True
    return False


def describe_distribution(data,verbose=False):
    mean = average(data)
    sqd = [(i-mean)**2 for i in data]
    variance = average(sqd)
    std = math.sqrt(variance)
    if not verbose:
        return mean,std
    median = sorted(data)[int(len(data)/2)]
    mode = collections.Counter(data).most_common(1)
    return mean,std,min(data),max(data),median,mode

from collections import deque

def sliding_window(seq, n=2):
    it = iter(seq)
    win = deque((next(it, None) for _ in xrange(n)), maxlen=n)
    yield win
    append = win.append
    for e in it:
        append(e)
        yield tuple(win)


def partition_data(dataset,test_set,id_column=0):
    """
    Partition the input dataset (as lists) and returns two datasets containing and excluding the items in test_set
    :param dataset: list[object]
    :param test_set: set
    :param id_column: int
    :return:
    """
    # this interface is left for legacy compatibility
    return partition_dataset(dataset,test_set,operator.itemgetter(id_column))


def partition_dataset(dataset,test_set,key):
    """
    Partition the input dataset (as objects) and returns two datasets containing and excluding the items in test_set
    :param dataset: list[object]
    :param test_set: set
    :param key: from operator import itemgetter,attrgetter
    :return:
    """
    # dataset is a list of objects
    # test_set is a list of ids to move to test set
    train = []
    test = []
    for i in dataset:
        if key(i) in test_set:
            test.append(i)
        else:
            train.append(i)
    return train,test

def most_common(lst):
    return max(set(lst), key=lst.count)

def return_one(*args, **kwargs):
    """
    Returns 1 for use with defaultdict.
    :return: int
    """
    return 1

def return_zero(*args, **kwargs):
    """
    Returns 0 for use with defaultdict.
    :return: int
    """
    return 0


def count_bits(n):
    """
    Counts the number of bits in an integer or flags in a bitmask.
    :param n: int
    :return: int
    """
    n = (n & 0x5555555555555555) + ((n & 0xAAAAAAAAAAAAAAAA) >> 1)
    n = (n & 0x3333333333333333) + ((n & 0xCCCCCCCCCCCCCCCC) >> 2)
    n = (n & 0x0F0F0F0F0F0F0F0F) + ((n & 0xF0F0F0F0F0F0F0F0) >> 4)
    n = (n & 0x00FF00FF00FF00FF) + ((n & 0xFF00FF00FF00FF00) >> 8)
    n = (n & 0x0000FFFF0000FFFF) + ((n & 0xFFFF0000FFFF0000) >> 16)
    n = (n & 0x00000000FFFFFFFF) + ((n & 0xFFFFFFFF00000000) >> 32) # This last & isn't strictly necessary.
    return n


def compute_set_intersection(a,b):
    a = set(a)
    b = set(b)
    return len(a & b),len(a-b),len(b-a)

def compute_abcprf(a,b,c):
    """
    Computes P, R, F-value given counts for true positives, false positives and true negatives.
    :param a: int
    :param b: int
    :param c: int
    :return: tuple(int,int,int,float,float,float)
    """
    if a:
        p,r=1.0*a/(a+b),1.0*a/(a+c)
        f = 2*p*r/(p+r)
        return a,b,c,p,r,f
    else:
        return 0,0,0,0,0,0
def compute_f(p,r):
    return 2*p*r/(p+r)

def string_as_print(*argv,**kargv):
    """
    Returns a string joining all the arguments except the special kargv `glue` used to join the string.
    :return: str
    """
    glue = kargv.get('glue',' ')
    return glue.join([str(i) for i in argv])

def is_numeric_int(s):
    try:
        int(s)
        return True
    except ValueError:
        return False

def is_numeric_float(s):
    try:
        float(s)
        return True
    except ValueError:
        return False


def object_list_to_dict(lst,key='id'):
    return dict([(getattr(i,key),i) for i in lst])


class AttrValueStorePrinter(object):
    """
    Inherit to easily print a representation of the object as it's dictionary
    """
    def __repr__(self):
        return 'id: %s, ' % str(id(self)) + ','.join(['%s: %s' % (key,str(getattr(self,key))) for key in sorted(self.__dict__) if not key.startswith('_')])


class AttrValueStore(AttrValueStorePrinter):
    """
    Key-Value store in an object
    """
    def __init__(self):
        self.__dict__data = {}

    def __setattr__(self, name, value):
        try:
            super(AttrValueStore, self).__setattr__(name, value)
        except:
            self.__dict__data[name] = value

    def __getattr__(self, name):
        return self.__dict__data[name]

    def __delattr__(self, name):
        del self.__dict__data[name]

class KnownException(Exception):
    pass

def ping(host):
    """
    Returns True if host responds to a ping request
    """
    import os, platform

    # Ping parameters as function of OS
    ping_str = "-n 1" if  platform.system().lower()=="windows" else "-c 1"

    # Ping
    return os.system("ping " + ping_str + " " + host) == 0

class SentinelValue(object):
    pass

def format_list(lst,glue=' ',options={}):
    return glue.join([i.format(options=options) for i in lst])
