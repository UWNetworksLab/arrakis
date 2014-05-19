
colors=[
"#ff0000",
"#00ff00",
"#0000ff",
"#ffff00",
"#ff00ff",
"#00ffff",
"#ff9900",
"#ff0099",
"#00ff99",
"#99ff00",
"#9900ff",
"#0099ff",
"#ff9933",
"#ff3399",
"#99ff33",
"#9933ff",
"#33ff99",
"#3399ff"
]

entries = dict()

# open file
f = open("dep.txt")

# read first list until "---"
i = 0
for line in f: 
    line = line.strip()
    if line.startswith("---"):
        break
    entries[line] = colors[i % len(colors)]
    i += 1

# create 2d dict from list
deps = dict()
for entry in entries.keys():
    deps[entry] = dict()
    e_dep = deps[entry]
    for entry2 in entries:
        e_dep[entry2] = 0
        
# read rest of file to get all dependencies
for line in f:
    line = line.strip()
    parts = line.split(None) # split on whitespace
    if line.startswith("#"):
        continue
    if len(parts) >= 3:
        deps[parts[0]][parts[1]] = parts[2]

f.close()

# write dot graph header
print "# process with: dot -Tpdf mydep.dot -O"
print "digraph dep {"
print "\tcolorscheme=x11;"

# write actual dot content
for dep1 in deps.keys():
    for dep2 in deps[dep1].keys():
        if deps[dep1][dep2] == "0":
            # no dependency
            continue
        if deps[dep1][dep2] == "1":
            print dep1, " -> ", dep2, " [color=\"", entries[dep1], "\"];"
        elif deps[dep1][dep2] == "2":
            print dep1, " -> ", dep2, " [color=\"", entries[dep1], "\"][style=\"dotted\"];"
        else:
            # this dependency hasn't been processed yet
            continue
#            print dep1, " -> ", dep2, " [color=\"yellow\"];"

# write dot graph footer
print "}"

