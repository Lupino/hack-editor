import os

def main(script, *args):
    os.system(' '.join(args))

if __name__ == '__main__':
    import sys
    main(*sys.argv)
