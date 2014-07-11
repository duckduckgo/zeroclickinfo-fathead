#!/usr/bin/python3

repos = ["stable", "testing", "unstable"]

lines = {}
names = {}
pkgs = {}

for repo in repos:
    with open("download/%s.txt" % repo) as f:
        lines[repo] = f.readlines()

    names[repo] = lines[repo][0].rsplit(" ", 1)[1].strip("\"\n")

    lines[repo] = lines[repo][6:]   #omit the 6 lines of header

    for p in lines[repo]:
        (name, ver, desc) = p.split(" ", 2)

        if name not in pkgs.keys():
            pkgs[name] = {} #this dict will hold the (ver, desc) from each of the three repos

        if repo not in pkgs[name].keys():
            pkgs[name][repo] = {}

        pkgs[name][repo]["ver"] = ver.strip("()")
        pkgs[name][repo]["desc"] = desc.strip()

for (p, q) in pkgs.items():
    desc = None
    ver = None
    abstract = []
    for repo in repos:
        if repo in q.keys():        #11
            abstract.append("%s (%s) %s https://packages.debian.org/%s/%s" % (repo, names[repo], q[repo]["ver"], names[repo], p))
            if not desc:
                desc = q[repo]["desc"]
            if not ver:
                ver = q[repo]["ver"]

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
