#!/usr/bin/env python

import os

with open("depsoftestinputs.txt") as f:
    s = f.readlines()

deps = set()
with open("myDb.txt", "w+") as _f:
    pass
for word in s:
    if word:
        index = word.strip().rindex("-")
        w = word[0:index].replace("-", "_")
        os.system(f"cat big-db/{w}-* >> myDb.txt")
