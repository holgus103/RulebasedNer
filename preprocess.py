import json
from pathlib import Path


def repeat(val, times):
    return [val for i in range(0, times)];


def extractEntities(x):
    if "subentities" in x:
        return x["subentities"]
    else:
        return None

contents = Path("train_wit_batch_1.json").read_text()
data = json.loads(contents);
samples = [];
tokens = [];
for v in data:
    txt = v["text"]
    beginnings = [0] + [i+1 for i,x in enumerate(v["text"]) if  x == ' '];
    words = txt.split(' ');
    # initialize tags
    tags = repeat("none", len(words))
    entities = v["entities"]
    for e in entities:
        if "start" in e:
            shift = e["start"]
        else:
            print("error at: ")
            print(json.dumps(e))
            print(txt)
            continue
        if "subentities" in e:
            for s in e["subentities"]:
                end = s["end"] + shift;
                start = s["start"] + shift;
                entity = txt[start:end];
                tag = s["entity"];
                count = len(entity.split(' '));
                if start in beginnings:
                    word_number = beginnings.index(start)
                else:
                    print("index: " + str(start) + " text: " + txt)
                    word_number = beginnings.index(start)
                tags[word_number:(word_number+count)] = repeat(tag, count);
    samples.append(words);
    tokens.append(tags);



z = list(zip(samples, tokens));
output = json.dumps(z);
file = open("output3.json", "w")
file.write(output)
file.close()

