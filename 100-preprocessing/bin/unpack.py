import csv
import sys
import time

with open(sys.argv[1], "r") as csvfile:
    reader = csv.reader(csvfile)

    first_line = True

    for row in reader:
        if (first_line):
            first_line = False
        else:
            if (row[2] != ""):
                message_time = "{0} {1} {2}".format(row[2], row[3], row[4])
                print "{0}".format(message_time)
            #print "{0}\t{1}\t{2}".format(row[0], row[3], row[18])

