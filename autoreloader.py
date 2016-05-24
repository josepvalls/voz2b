import sys,os,time,logging

RUN_RELOADER = True

logger = logging.getLogger(__name__)

whitelist = ['webapp2', 'paste', 'logging']

# this code is from autoreloader
_mtimes = {}
_win = (sys.platform == "win32")
_error_files = []
_cached_modules = set()
_cached_filenames = []


def gen_filenames(only_new=False):
    global _cached_modules, _cached_filenames
    module_values = set(sys.modules.values())
    _cached_filenames = clean_files(_cached_filenames)
    if _cached_modules == module_values:
        # No changes in module list, short-circuit the function
        if only_new:
            return []
        else:
            return _cached_filenames + clean_files(_error_files)

    new_modules = module_values - _cached_modules
    new_filenames = clean_files(
        [filename.__file__ for filename in new_modules
         if hasattr(filename, '__file__')])

    _cached_modules = _cached_modules.union(new_modules)
    _cached_filenames += new_filenames
    if only_new:
        return new_filenames + clean_files(_error_files)
    else:
        return _cached_filenames + clean_files(_error_files)

def clean_files(filelist):
    filenames = []
    for filename in filelist:
        if not filename:
            continue
        if filename.endswith(".pyc") or filename.endswith(".pyo"):
            filename = filename[:-1]
        if filename.endswith("$py.class"):
            filename = filename[:-9] + ".py"
        if os.path.exists(filename):
            filenames.append(filename)
    return filenames

# this code is modified from autoreloader
def check_code_changed():
    global _mtimes, _win
    for filename in gen_filenames():
        stat = os.stat(filename)
        mtime = stat.st_mtime
        if _win:
            mtime -= stat.st_ctime
        if filename not in _mtimes:
            _mtimes[filename] = mtime
            continue
        if mtime != _mtimes[filename]:
            _mtimes = {}
            try:
                del _error_files[_error_files.index(filename)]
            except ValueError:
                pass
            mname = filename.split('/')[-1].split('.')[0]
            logger.info('CHANGED %s, RELOADING %s' % (filename,mname))
            try:
                reload(sys.modules[mname])
            except:
                pass

    return False

def reloader_thread():
    while RUN_RELOADER:
        check_code_changed()
        time.sleep(1)
