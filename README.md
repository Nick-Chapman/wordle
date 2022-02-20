# wordle

Explore wordle strategies.


### Some results:

#### bot1

```
description: guess1='raise'; choose first word from remaining
#games=2315
total=9252
max=9
distribution[(1,1),(2,131),(3,663),(4,857),(5,466),(6,152),(7,33),(8,9),(9,3)]
average=3.9965442764578833
```

#### bot2

```
description: guess1='raise'; choose from remaining, maximizing entropy over remaining
#games=2315
total=8334
max=8
distribution[(1,1),(2,131),(3,999),(4,918),(5,208),(6,46),(7,10),(8,2)]
average=3.6
```

#### bot3

```
description: guess1='raise'; choose from answers, maximizing entropy over remaining
#games=2315
total=8231
max=6
distribution[(1,1),(2,46),(3,1034),(4,1137),(5,94),(6,3)]
average=3.5555075593952483
```

#### bot4

```
description: guess1='raise'; choose from answers, maximizing entropy over remaining; prefer possible words
#games=2315
total=8095
max=6
distribution[(1,1),(2,81),(3,1110),(4,1016),(5,104),(6,3)]
average=3.4967602591792657
```

#### bot5

```
description: guess1='raise'; choose from legal, maximizing entropy over remaining; prefer possible words
#games=2315
total=8044
max=6
distribution[(1,1),(2,71),(3,1168),(4,981),(5,91),(6,3)]
average=3.474730021598272
```

#### bot5 (salet)

```
description: guess1='salet'; choose from legal, maximizing entropy over remaining; prefer possible words
#games=2315
total=7953
max=6
distribution[(2,99),(3,1182),(4,963),(5,69),(6,2)]
average=3.4354211663066954
```
