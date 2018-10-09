# flock-pieces-fglang

A very small term-rewriting language, related to the study of push-throughs.


## Demo executable

```bash
> ./.stack-work/install/x86_64-osx/lts-10.2/8.2.2/bin/fglang-demo 4
["GFG","GFF","GF"]
["GGFF","GGFG","FGFG","FGFF"]
["FFGFG","GFGFF","FFGFF","GFGFG","GGGFG","GGGFF","FGGFF","FGGFG"]
["FGGGFG","FGGGFF","GFGGFG","FFGFF","GFGGFF","GFGFG","FFGGFF","FFGGFG","FFGF","GGGGFF","GGGGFG","GGGF","GGGFG","FGGF","FGFGFF","GFGF","FFFGFG","FGFGFG","FFFGFF","GGFGFG","FGGFF","GFFGFF","GGFGFF","GFFGFG"]
```


## Module Dependency Graph

```bash
render_graph.sh
```

![dependency_graph.jpg](https://github.com/michaeljklein/flock-pieces-fglang/raw/master/dependency_graph.jpg "graphmod dependency graph; see dependency_graph.dot, render_graph.sh")



## Tree

```bash
> git rev-parse HEAD
73d42c0cdc7f9747c32cdc3c5051e7f1398e0426

> tree .
.
├── LICENSE
├── README.md
├── Setup.hs
├── app
│   └── Main.hs
├── bench
│   └── Bench.hs
├── flock-pieces-fglang.cabal
├── render_graph.sh
├── src
│   └── Data
│       ├── FGLang.hs
│       ├── Flip.hs
│       ├── Functor
│       │   ├── Join.hs
│       │   ├── Object.hs
│       │   └── Turn.hs
│       ├── HashSet
│       │   └── Utils.hs
│       ├── Hashable
│       │   └── Orphans.hs
│       └── Tuple
│           └── Utils.hs
├── stack.yaml
└── test
    └── Test.hs

9 directories, 17 files
```

# Docs

Haddock generated documentation may be found [here](https://michaeljklein.github.io/flock-pieces-fglang/doc/index.html)

