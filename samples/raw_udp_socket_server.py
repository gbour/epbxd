#!/usr/bin/env python
# -*- coding: utf8 -*-

"""Sample program

    Manage to receive as much UDP packets as possible on port 9999
    Display UDP throughput (packets/seconds) and global average

    Compare with erlang matching program
"""
# os.kill(pid,sig)
import socket, os, signal

PORT = 9999

count = 0; total = 0; num = 1
def handler(signum, frame):
    global count, num, total

    print '%d/sec (%f)' % (count, float(total)/num)
    num += 1; total += count; count = 0
    signal.alarm(1)

signal.signal(signal.SIGALRM, handler)

s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(("localhost", PORT))

signal.alarm(1)
while True:
    try:
        data = s.recv(65565)
        count += 1
    except Exception:
        pass

s.close()
