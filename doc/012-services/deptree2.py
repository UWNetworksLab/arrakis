import sys

# manually generated list
# colors=[
# "#ff0000",
# "#00ff00",
# "#0000ff",
# "#ffff00",
# "#ff00ff",
# "#00ffff",
# "#ff9900",
# "#ff0099",
# "#00ff99",
# "#99ff00",
# "#9900ff",
# "#0099ff",
# "#ff9933",
# "#ff3399",
# "#99ff33",
# "#9933ff",
# "#33ff99",
# "#3399ff"
# ]

# from: http://en.wikipedia.org/wiki/HSL_and_HSV
colors = [
#"#FFFFFF",
"#808080",	
"#000000",	
"#FF0000",
"#BFBF00",
"#008000",
"#80FFFF",
"#8080FF",
"#BF40BF",
"#A0A424",
"#411BEA",
"#1EAC41",
"#F0C80E",
"#B430E5",
"#ED7651",
#"#FEF888",
"#19CB97",
"#362698",
"#7E7EB8",
]

# taken from http://www.mcfedries.com/books/cightml/x11color.htm
# colors=[
# "#DC143C", # Crimson	
# "#228B22", # ForestGreen	
# "#FF00FF", # Magenta	
# "#808000", # Olive	
# "#8B008B", # DarkMagenta
# "#7B68EE", # MediumSlateBlue	
# "#DAA520", # Goldenrod
# "#191970", # MidnightBlue
# "#FFA500", # Orange	
# "#00CED1", # DarkTurquoise
# "#8B4513", # SaddleBrown	
# "#8B0000", # DarkRed	
# "#808080", # Gray	
# ]

# colors = [
# "aquamarine4",
# "azure4", 
# "bisque3",
# "blue1", 
# "brown", 
# "burlywood",
# "cadetblue", 
# "chartreuse",
# "chocolate", 
# "coral",
# "cornflowerblue",
# "cornsilk4", 
# "cyan3",
# "darkgoldenrod3",
# "darkolivegreen1", 
# "darkorange1", 
# "darkorchid1",
# "darkseagreen", 
# "darkslateblue",
# "darkslategray4",
# "deeppink1",
# "deepskyblue1",
# "dimgrey",
# "dodgerblue4",
# "firebrick4",
# "gold"
# ]

#colors = ["blueviolet",	"brown", "burlywood", "cadetblue", "chartreuse", "chocolate", "coral", "cornflowerblue", "cornsilk", "crimson", "cyan", "darkblue", "darkcyan", "darkgoldenrod", "darkgray", "darkgreen", "darkgrey", "darkkhaki", "darkmagenta", "darkolivegreen", "darkorange", "darkorchid", "darkred", "darksalmon", "darkseagreen"]

dep_type = sys.argv[1]

# open file
f = open("dep.txt")

entries_1 = dict()
entries_2 = dict()

# read first list until "---"
i = 0;
for line in f: 
    line = line.strip()
    if line.startswith("---"):
        break
    entries_1[line] = [line+"_1", colors[i % len(colors)]]
    entries_2[line] = [line+"_2", colors[i % len(colors)]]
    i += 1

# create 2d dict from list
deps = dict()
for entry in entries_1.keys():
    deps[entry] = dict()
    e_dep = deps[entry]
    for entry2 in entries_1.keys():
        e_dep[entry2] = dict()
        e_dep[entry2]["base"] = 0
        e_dep[entry2]["type"] = "x"
        
# read rest of file
for line in f:
    line = line.strip()
    parts = line.split(None) # split on whitespace
    if line.startswith("#"):
        continue
    if len(parts) >= 3:
        deps[parts[0]][parts[1]]["base"] = parts[2]
    if parts[2] != 0 and len(parts) >= 4:
        deps[parts[0]][parts[1]]["type"] = parts[3]

f.close()

# write dot graph header
print "# process with: dot -Tpdf mydep2.dot -O -Gratio=0.5"
print "digraph dep {"
print "\tcolorscheme=x11;"

# write actual dot content
for dep1 in deps.keys():
    for dep2 in deps[dep1].keys():
        if deps[dep1][dep2]["base"] == "0":
            # no dependency
            continue
        if dep_type != "a" and deps[dep1][dep2]["type"][0] != dep_type:
            # HACK: [0] to deal with fb type
            # we only want desired dependency
            continue
        if deps[dep1][dep2]["base"] == "1":
            print entries_1[dep1][0], " -> ", entries_2[dep2][0], " [color=\"", entries_1[dep1][1], "\"][style=\"bold\"];"
        elif deps[dep1][dep2]["base"] == "2":
            print entries_1[dep1][0], " -> ", entries_2[dep2][0], " [color=\"", entries_1[dep1][1], "\"][style=\"dashed\"];"
        else:
            # this dependency hasn't been processed yet
            continue
#            print entries_1[dep1], " -> ", entries_2[dep2], " [color=\"yellow\"];"

# write dot graph footer
print "}"

