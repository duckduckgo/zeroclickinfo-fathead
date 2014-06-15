#!/usr/bin/python3

with open("download/stable.txt") as f:
	stable = f.readlines()

with open("download/testing.txt") as f:
	testing = f.readlines()

stable_name = stable[0].rsplit(" ", 1)[1].strip("\"\n")
testing_name = testing[0].rsplit(" ", 1)[1].strip("\"\n")

stable = stable[6:]	#omit the 6 lines of header
testing = testing[6:]

pkgs = {}

for p in stable:
	q = p.split(" ", 2)
	pkgs[q[0]] = {
		"stable": {
			"ver": q[1].strip("()"),
			"desc": q[2].strip()
		},
		"testing": None
	}

for p in testing:
	q = p.split(" ", 2)
	try:
		pkgs[q[0]]["testing"] = {
			"ver": q[1].strip("()"),
			"desc": q[2].strip()
		}
	except KeyError:
		pkgs[q[0]] = {
			"stable": None,
			"testing": {
				"ver": q[1].strip("()"),
				"desc": q[2].strip()
			}
		}

for (p, q) in pkgs.items():
	if q["stable"] != None:
		print("stable\t%s\t%s\t%s\t%s\thttps://packages.debian.org/%s/%s" % (stable_name, p, q["stable"]["ver"], q["stable"]["desc"], stable_name, p))
	if q["testing"] != None:
		print("testing\t%s\t%s\t%s\t%s\thttps://packages.debian.org/%s/%s" % (testing_name, p, q["testing"]["ver"], q["testing"]["desc"], testing_name, p))
