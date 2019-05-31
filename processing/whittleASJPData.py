o = open("../data/ASJP/ASJPIDs_FAIR.txt")
d = o.read()
o.close()

ids = [x for x in d.split("\n") if len(x)>1]

with open("../data/ASJP/levenshteinLanguageDistances.csv") as f:
	for line in f:
		bits = line.split(",")
		l1 = bits[0]
		l2 = bits[1]
		if l1 in ids and l2 in ids:
			print(line.rstrip())
