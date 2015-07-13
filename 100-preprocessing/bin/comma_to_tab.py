import csv
import sys

with open(sys.argv[1], "r") as csvfile:
    reader = csv.reader(csvfile)

    for row in reader:
        
        print "\t".join(row[0:17]).replace("\r\n", " ");

