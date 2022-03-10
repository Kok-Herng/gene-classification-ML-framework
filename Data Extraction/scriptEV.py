#script to extract and merge multiple Expression Value files into 1

import string
import re
import csv
import os

for file in os.listdir(os.getcwd()): #for every file in current working directory
	if file.endswith(".csv"):
		
		fIn = open(file, "r+") #open each csv files in directory
		fOut = open("outEV.csv","a",newline='') #output each lines to one single output file using append
		#newline='' to disable extra newlines written to output file

		data = [] #empty tuple to store ALL lines
		cnt = 0

		lines = fIn.readlines() #read all lines
		last = lines[-1] #last line in the file
		
		for line in lines:
			line.strip() #Remove spaces at the beginning and end of string
			print(line)
			
			head = re.match(r'""(.*)', line, re.IGNORECASE) #return true for string starts with "" and followed
			#by multiple any characters (2nd line onwards in file)
			
			if head: #true
				tempRow = [] #empty tuple to store ONE line at a time, revert back to empty after each line
				headerLine = head.group(1) #return the SECOND subgroup for the re match (in this case, LOCxxxxx and so on)
				print (headerLine)

				terms = headerLine.split(',') #split string into list (tuple?) with comma as seperator
				print(terms)	

				geneId = terms[1]; geneId = geneId[1:-1] #extract the SECOND item in tuple and remove " at the beginning and end
				tissue = terms[-2]; tissue = tissue[1:-1]
				developmentStage = terms[-3]; developmentStage = developmentStage[1:-1]
				
				if line is last: #check if current line is last in file
					ev = terms[-1]; ev = ev[1:-1] #ensure no extra digit is removed since no /r for last line
				
				else:
					ev = terms[-1]; ev = ev[1:-1] #remove /r for rest of the lines

				tempRow = [geneId, developmentStage, tissue, ev] #put extracted items into a tuple
				print(tempRow)

				data.append(tempRow) #append the extracted items tuple into another tuple

				cnt+=1

			else:
				pass

		writer = csv.writer(fOut) #write to csv file
		#csvHeader = ['Gene Id', 'Development Stage', 'Tissue', 'EV'] #output file header
		#writer.writerow(csvHeader) #write a single header row
		writer.writerows(data) #write all data rows
		
		print(cnt)
