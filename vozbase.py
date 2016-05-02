import jsonpickle
import copy
import settings


def serialize(obj, use_deep_copy=True, pretty_serializer=settings.SERIALIZE_PRETTY_JSON_BY_DEFAULT):
    if pretty_serializer:
        jsonpickle.set_encoder_options('json', sort_keys=True, indent=4)
        encoding_method = lambda i:jsonpickle.encode(i,make_refs=False)
    else:
        encoding_method = jsonpickle.encode
    return encoding_method(obj)
def serialize_to_file(obj,file_name,use_deep_copy=True,pretty_serializer=settings.SERIALIZE_PRETTY_JSON_BY_DEFAULT):
    open(file_name,'w').write(serialize(obj,use_deep_copy,pretty_serializer))

def unserialize(json):
    return jsonpickle.decode(json)

def unserialize_file(file_name):
    return unserialize(open(file_name).read())


class VozContainer(object):
    def __init__(self):
        self.id = id(self)
    def _compute_caches(self,parent):
        pass
    def _clear_caches(self,parent):
        pass
    def _compute_caches(self,parent):
        pass
    def serialize(self,use_deep_copy=True,pretty_serializer=settings.SERIALIZE_PRETTY_JSON_BY_DEFAULT):
        if pretty_serializer:
            jsonpickle.set_encoder_options('json', sort_keys=True, indent=4)
            encoding_method = lambda i:jsonpickle.encode(i,make_refs=False)
        else:
            encoding_method = jsonpickle.encode
        if use_deep_copy:
            self._prepare_copy()
            obj = copy.deepcopy(self) #type: VozContainer
            obj._clear_caches(obj)
            return encoding_method(obj)
        else:
            self._clear_caches(self)
            ret = encoding_method(self)
            self._compute_caches(self)
            return ret
    def serialize_to_file(self,file_name,use_deep_copy=True, pretty_serializer=settings.SERIALIZE_PRETTY_JSON_BY_DEFAULT):
        open(file_name,'w').write(self.serialize(use_deep_copy,pretty_serializer))
    def __repr__(self):
        return "%s %d" % (self.__class__.__name__, self.id)
    def _prepare_copy(self):
        pass


class VozTextContainer(object):
    def __init__(self,id,offset,length):
        """
        :param id: int
        :param offset: int
        :param length: int
        """
        self.id = id
        self.offset = offset
        self.len = length
    def __repr__(self):
        return "%s %d" % (self.__class__.__name__, self.id)
