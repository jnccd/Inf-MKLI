def elementAt(it, n):
    return next((x for i,x in enumerate(it) if i==n), None)

def createArray(base, mapper):
    re = []
    for e in base: re.append(mapper(e))
    return re