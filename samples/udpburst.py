#!/usr/bin/env python
# -*- coding: utf8 -*-

"""Generate UDP burst datas on localhost:9999 target port 

    200 sources sockets
    10K sended packets (each a random word from *words* dictionary)
"""

import random, time
from socket import *

NB_SOCKS=200
NB_MSGS=10000

socks = [socket(AF_INET, SOCK_DGRAM) for i in xrange(NB_SOCKS)]

with open('/usr/share/dict/words', 'r') as f:
    words = f.readlines()

for i in xrange(NB_MSGS):
    print i
    socks[random.randrange(0,len(socks))].sendto(
            words[random.randrange(0,len(words))],
            ("localhost", 9999)
    )

    time.sleep(random.random()/10)
