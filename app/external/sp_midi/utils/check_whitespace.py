'''
Check if the files in a directory conform to whitespace rules:
- no trailing whitespace
- no tabs used as indentation
'''
import os
from os.path import join
import re

TYPES_TO_CHECK = ['.txt', '.c', '.cpp', '.h']


def has_trailing_whitespace(fname):
    '''
    true if trailing whitespace is found, false otherwise
    '''
    with open(fname) as fdesc:
        for line in fdesc:
            line = line.rstrip('\n')
            if len(line) > 0:
                if line[len(line) - 1] == ' ' or line[len(line) - 1] == '\t':
                    return True

    return False

def has_tabs_in_indentation(fname):
    '''
    true if a tab in indentation is found, false otherwise
    '''
    with open(fname) as fdesc:
        for line in fdesc:
            if re.match(r'^\s*\t', line):
                return True

    return False


bad_files = False
for root, dirs, files in os.walk('src'):
    for name in files:
        fullname = join(root, name)
        for end in TYPES_TO_CHECK:
            if fullname.endswith(end):
                if has_trailing_whitespace(fullname):
                    print(fullname + " contains trailing whitespace")
                    bad_files = True

                if has_tabs_in_indentation(fullname):
                    print(fullname + " contains tabs in indentation")
                    bad_files = True


exit(0 if not bad_files else 1)
