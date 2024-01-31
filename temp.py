import zipfile
import itertools
import time

print(next(itertools.count(int(time.time() * 1000))))

with zipfile.ZipFile("test.apkg", "w") as myzip:
    myzip.write("./temp.db", "collection.anki2")
