# norm

A Clojure library designed to perform lexical normalisation of tweets.

## Usage

**WARNING:** While this works and everything, it probably won't help you. The quality of normalisation is really quite poor. Also you'll need access to some seriously hardcore HPC to train it, and even then it'll probably take a day or three. I really think you'd be better off not using it. For real.

First you must train the system. To do this, you need the following:

- `dict` -- A dictionary (linefeed-separated words, all lowercased)
- `nyt` -- A  folder containing a selection of English Gigaword NYT gzipped XML files
- `twt` -- An enormous corpus of linefeed-separated english tweets (~100 million should do the trick)
- Access to some seriously hardcore HPC, to reiterate.

Get your data in the right place (see `example.norm-config.edn`. Remove `example.` and uncomment relevant lines for the options to take effect) and then run

```
lein run bootstrap
```

or equivalent if you make a uberjar.

After many hours it will stop and you will be able to be all like:

```
lein run batch some_tweets.in some_tweets.out
```

The tweets in `some_tweets.in` will get normalised and put in `some_tweets.out`.

If you didn't specify an output path, those tweets would be output to `some_tweets.in.norm`.

It takes and outputs three formats:

- `raw` -- Linefeed-separated tweets.
- `tkn` -- Linefeed-separated tokens, double-linefeed separated tweets.
- `json` -- A list of tweet objects, where `"text"` give the tweet text and/or `"tokens"` gives the tokens.

The default input format is `raw`. It's not clever enough to figure out when you want to use a different format, so specify it manually with `-i`. The default output format is `tkn`, and you can specify it manually with `-o`.

In addition to formats, there are three types of normalisation it can do:

- `simple` -- takes no time at all, gives okish results sometimes
- `complex` -- takes a long time, gives entirely unpredictable results
- `duplex` -- the best of both worlds

Simple is the default. You can change it with -t

e.g. 

```
lein run batch some_tweets.in -i tkn -o json -t complex
```

There are a bazillion other options you can set, either in norm-config.edn or at the command line (use `-h` to get a big list).

### As a lib (Clojure)

To use it as an library, alls you need to do from Clojure is

```Clojure
(def normalise (norm.alise/get-simple-normaliser-fn))
```

or replace `simple` with one of the other normaliser types.

Then you can be all like:

```Clojure
(normalise ["sum" "day" "im" "gunna" "eat" "da" "salad" "b4" "da" "meat" "," "loool"])
```

And it'll hopefully give you something like:
```Clojure
["some" "day" "i'm" "gonna" "eat" "the" "salad" "before" "the" "meat" "," "lol"]
```

But will probably give you something more like:
```Clojure
["same" "dad" "ma" "guns" "eat" "add" "salad" "bet" "add" "meat" "," "loll"]
```

### As a lib (Java)
`norm.Norm.getSimpleNormaliser`, replacing `Simple` with camel-cased version of your favourite normaliser type.

This will give you a `norm.jvm.Normaliser` object, exposing a method with signature:

`public List<String> normalise(List<String>);`

## Only Simple

If you only want to use the simple normaliser, all you need is `dict` and `twt`.

Then just run:

`lein run train dm-dict && lein run train nmd`

and you should be good to go.

## Config

As already mentioned, configuration is done with the file `norm-config.edn`. Put this in the current working directory. Alternatively, you can put a file, `.norm-config.edn`, in your home directory for system-wide config goodness.

## Good Luck!

And happy normalising.

## License

Distributed under the Eclipse Public License, the same as Clojure.
