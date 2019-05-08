import os
files = os.listdir("./test/MatrixTest/")
for f in files:
	if ".out" in f or ".err" in f:
		tmp = []
		with open("./test/MatrixTest/" + f) as ff:
			for n, line in enumerate(ff):
				tmp.append(line)
		newf = open("./test/MatrixTest/" + f, 'w')
		for l in tmp[1:]:
			newf.write(l)
		newf.close()