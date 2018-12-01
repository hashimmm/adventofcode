from itertools import cycle


with open('input-day1') as f:
    text = f.read()


# part 1
print(str(eval('0' + ''.join(text.split()))))


# part 2
freq = 0
found = {0}
for term in cycle(text.split()):
    nextval = eval(str(freq)+term)
    if nextval in found:
        break
    found.add(nextval)
    freq = nextval

print(nextval)
