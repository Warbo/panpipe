# Used for building and testing on build servers like Hydra
with (import ./.).panpipe.components;
exes // tests // { inherit library; }
