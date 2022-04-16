# startup.py - loaded into interactive Python prompts.

print("(.startup.py)")

import collections, datetime, itertools, math, os, pprint, re, sys, time
print("(imported collections, datetime, itertools, math, os, pprint, re, sys, time)")

pp = pprint.pprint

# paste code into the repl.
def paste():
    import textwrap
    exec(textwrap.dedent(sys.stdin.read()), globals())

# readline and history support.
def hook_up_history():
    try:
        import readline
    except ImportError:
        print("No readline, use ^H.")
    else:
        import atexit
        import os
        import rlcompleter

        history_path = os.path.expanduser("~/.pyhistory{0}".format(sys.version_info[0]))

        def save_history():
            import readline
            readline.write_history_file(history_path)

        if os.path.exists(history_path):
            readline.read_history_file(history_path)

        atexit.register(save_history)

# don't do history stuff if we are IPython, it has its own thing.
is_ipython = "In" in globals()
if not is_ipython:
    hook_up_history()

# get rid of globals we don't want.
del is_ipython, hook_up_history
