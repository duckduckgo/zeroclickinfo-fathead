#!/usr/bin/python3

with open("download/stable.txt") as f:
    stable = f.readlines()

with open("download/testing.txt") as f:
    testing = f.readlines()

with open("download/unstable.txt") as f:
    unstable = f.readlines()

stable_name = stable[0].rsplit(" ", 1)[1].strip("\"\n")
testing_name = testing[0].rsplit(" ", 1)[1].strip("\"\n")
unstable_name = unstable[0].rsplit(" ", 1)[1].strip("\"\n")

stable = stable[6:]    #omit the 6 lines of header
testing = testing[6:]
unstable = unstable[6:]

pkgs = {}

for p in stable:
    q = p.split(" ", 2)
    pkgs[q[0]] = {
        "stable": {
            "ver": q[1].strip("()"),
            "desc": q[2].strip()
        },
        "testing": None,
        "unstable": None
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
            },
            "unstable": None
        }

for p in unstable:
    q = p.split(" ", 2)
    try:
        pkgs[q[0]]["unstable"] = {
            "ver": q[1].strip("()"),
            "desc": q[2].strip()
        }
    except KeyError:
        pkgs[q[0]] = {
            "stable": None,
            "testing": None,
            "unstable": {
                "ver": q[1].strip("()"),
                "desc": q[2].strip()
            }
        }

for (p, q) in pkgs.items():
    desc = None
    ver = None
    abstract = []
    if q["stable"] != None:        #11
        abstract.append("stable (%s) %s https://packages.debian.org/%s/%s" % (stable_name, q["stable"]["ver"], stable_name, p))
        if not desc:
            desc = q["stable"]["desc"]
        if not ver:
            ver = q["stable"]["ver"]
    if q["testing"] != None:
        abstract.append("testing (%s) %s https://packages.debian.org/%s/%s" % (testing_name, q["testing"]["ver"], testing_name, p))
        if not desc:
            desc = q["testing"]["desc"]
        if not ver:
            ver = q["testing"]["ver"]
    if q["unstable"] != None:
        abstract.append("unstable (%s) %s https://packages.debian.org/%s/%s" % (unstable_name, q["unstable"]["ver"], unstable_name, p))
        if not desc:
            desc = q["unstable"]["desc"]
        if not ver:
            ver = q["unstable"]["ver"]

    out = p + "\t"        #0
    out += "A\t"        #1
    out += "\t"            #2
    out += "\t"            #3
    out += "\t"            #4
    out += "\t"            #5
    out += "\t"            #6
    out += "\t"            #7
    out += "\t"            #8
    out += "\t"            #9
    out += "[[Image:https://screenshots.debian.net/thumbnail-with-version/%s/%s]]\t" % (p, ver)        #10
    out += desc + "<br>" + "<br>".join(abstract)    #11
    out += "\t"            #12
    print(out)
