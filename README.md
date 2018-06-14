# CaseStudy

Associated code for a small case study for the paper

  Type Safe Interpreters for Free

[presented at TFP 2018](http://www.cse.chalmers.se/~myreen/tfp2018/program.html).

[Presentation slides online](http://www.cse.chalmers.se/~patrikj/talks/TFP_2018_Jansson_TySaInt.pdf) and [tweeted](https://twitter.com/patrikja/status/1006473768221532160).

```shell
stack build
stack exec server
```

Then open [index.html](index.html) in a browser and splice in some example code.

```
scale 100 fish
```
or

```
scale 500 (beside (above (rot fish) fish) (above (rot (rot fish)) (rot (rot (rot fish)))))
```
