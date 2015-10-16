#!/usr/bin/python -tt

import re, sys

def genra2number(genre):
  """
  Given genra, return corresponding number
  """
  genreDict = {
  "Action":"1",
  "Adventure":"2",
  "Animation":"3",
  "Children's":"4",
  "Comedy":"5",
  "Crime":"6",
  "Documentary":"7",
  "Drama":"8",
  "Fantasy":"9",
  "Film-Noir":"10",
  "Horror":"11",
  "Musical":"12",
  "Mystery":"13",
  "Romance":"14",
  "Sci-Fi":"15",
  "Thriller":"16",
  "War":"17",
  "Western":"18",
  "None":"99"
  }
  return genreDict[genre]  

def year_from_title(title):
  """
  Given title, return year
  """
  match = re.search(r'\((\d{4})\)', title)
  year = match.group(1)
  return year

def clean_movies():
  """
  Given standard input of the example data:
  movieid::title::genres(pipe separated list of zero or more movie genres)
  1::Toy Story (1995)::Animation|Children's|Comedy
  reformat to tab separated and pull out year:
  movieid<tab>tile<tab>year<tab>list_of_generes_num
  """ 
 
  for line in sys.stdin:
    line = line.strip()
    movieid,title,genres = line.split('::')
    year = year_from_title(title)
    genresList = genres.split('|')
    genresNum = []
    for genre in genresList:
      genreNum = genra2number(genre)
      genresNum.append(genreNum)
    genresNumPrintList = ','.join(genresNum)
    print "%s\t%s\t%s\t%s" % (movieid, title, year,genresNumPrintList)

def main():
  clean_movies()

if __name__ == '__main__':
  main()

