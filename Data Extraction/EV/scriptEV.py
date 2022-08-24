#script to extract and merge multiple Expression Value files into 1

import re
import csv
import os

for file in os.listdir(os.getcwd()): #for every file in current working directory
	if file.endswith(".csv"):
		
		fIn = open(file, "r+") #open csv files
		fOut = open("outEV2.csv","a",newline='') #open file for output
		#newline='' to disable extra newlines written to output file

		data = [] #empty list to store ALL lines
		cnt = 0

		lines = fIn.readlines() #read all lines into a list of strings
		last = lines[-1] #last line in the file
		
		for line in lines:
			line.strip() #remove spaces at the beginning and end of string
			print(line)
			
			head = re.match(r'""(.*)', line, re.IGNORECASE) #match string starts with "" and followed by zero or more any characters (2nd line onwards in file)
			
			if head: #true
				tempRow = [] #empty list to store ONE line at a time, revert to empty after each line
				headerLine = head.group(1) #return the SECOND subgroup for the re match (in this case, LOCxxxxx and so on)
				print (headerLine)

				terms = headerLine.split(',') #split string into list with comma as seperator
				print(terms)	

				#extract the respective gene ID, tissue, development stage and remove " at the beginning and end
				geneId = terms[1]; geneId = geneId[1:-1] 
				tissue = terms[-2]; tissue = tissue[1:-1]
				developmentStage = terms[-3]; developmentStage = developmentStage[1:-1]
				
				#weird but it works ¯\_(ツ)_/¯
				if line is last: #check if current line is last in file
					ev = terms[-1]; ev = ev[1:-1] #ensure no extra digit is removed since no newline for last line
				
				else:
					ev = terms[-1]; ev = ev[1:-1] #remove newline for rest of the lines

				tempRow = [geneId, developmentStage, tissue, ev] #put extracted items into a list
				print(tempRow)

				data.append(tempRow) #append the extracted items list into another list

				cnt+=1

			else:
				pass

		writer = csv.writer(fOut) #write to csv file
		#csvHeader = ['Gene Id', 'Development Stage', 'Tissue', 'EV'] #output file header
		#writer.writerow(csvHeader) #write a single header row
		writer.writerows(data) #write all data rows
		
		print(cnt)
