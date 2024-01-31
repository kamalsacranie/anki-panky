import zipfile

with zipfile.ZipFile("test.apkg", "w") as myzip:
    myzip.write("./collection.anki2", "collection.anki2")
