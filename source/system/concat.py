import sys
import os

ROOT = os.path.dirname(os.path.dirname(__file__))

def path_join(*args):
    def _rem(path):
        if path.startswith('/'):
            path = path[1:]
        return path
    args = list(map(_rem, args))
    
    return os.path.join(*args)
    

def main(script, target, *proc_files):
    procs = []
    for proc_file in proc_files:
        with open(path_join(ROOT, proc_file), 'r') as f:
            procs.append(f.read())

    with open(path_join(ROOT, target), 'w') as f:
        f.write('\n'.join(procs))
        
    print('OK')

if __name__ == '__main__':
    main(*sys.argv)
