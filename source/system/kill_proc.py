import os

ROOT = os.path.dirname(os.path.dirname(__file__))

def path_join(*args):
    def _rem(path):
        if path.startswith('/'):
            path = path[1:]
        return path
    args = list(map(_rem, args))

    return os.path.join(*args)

def main(script, *pid_files):
    for pid_file in pid_files:
        pid_file = path_join(ROOT, pid_file)
        os.system("cat %s | xargs kill"%pid_file)

if __name__ == '__main__':
    import sys
    main(*sys.argv)
