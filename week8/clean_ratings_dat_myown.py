#!/usr/bin/python -tt

import re, sys

def clean_ratings():
  """
  Given standard input of the example data:
  userid::movieid::rating::timestamp
  1::1193::5::978300760
  reformat to tab separated.
  """ 
  for line in sys.stdin:
    line = line.strip()
    userid,movieid,rating,timestamp = line.split('::')
    print "%s\t%s\t%s\t%s" % (userid, movieid, rating, timestamp)

def main():
  clean_ratings()

if __name__ == '__main__':
  main()

