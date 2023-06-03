# csg

Building requires [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

```sh
stack build
```

Running:

```sh
stack run -- path/to/map/file.unr path/to/build/output.bvh
```

The exact format of the `.bvh` file is defined at [src/Export/Bvh.hs](src/Export/Bvh.hs).
