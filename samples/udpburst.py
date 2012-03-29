#!/usr/bin/env python
# -*- coding: utf8 -*-

"""Generate UDP burst datas on localhost:9999 target port 

    200 sources sockets
    10K sended packets (each a random word from *words* dictionary)
"""

import random, time, signal
from socket import *

NB_SOCKS=200
NB_MSGS=1000000

count = 0; total = 0; num = 1
def handler(signum, frame):
    global count, num, total

    print '%d/sec (%f)' % (count, float(total)/num)
    num += 1; total += count; count = 0
    signal.alarm(1)

signal.signal(signal.SIGALRM, handler)
socks = [socket(AF_INET, SOCK_DGRAM) for i in xrange(NB_SOCKS)]

with open('/usr/share/dict/words', 'r') as f:
    words = f.readlines()

signal.alarm(1)
for i in xrange(NB_MSGS):
    socks[random.randrange(0,len(socks))].sendto(
            words[random.randrange(0,len(words))],
            ("localhost", 9999)
    )
    count += 1

    #time.sleep(random.random()/10)
