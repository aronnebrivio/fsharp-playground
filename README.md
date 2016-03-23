# F# Playground
This repo contains, divided between *numerated* folders, a personal college F# playground.    

## Struct
There is a folder for **each** lesson.   
Each folder contains
- 2do work (`*.txt`)
- code (`*.fsi`)
- [slides (`*.pdf`)]
    
## Dependencies
You will have to install `fsharpi` package in order to have an interpreter for this code (`fsharpc` too if you want to compile them).    
Plus you'll have to download (and compile/import) `FsCheck` from [here](https://fscheck.github.io/FsCheck/).

## Note
I'm currently using `git-flow` ([http://danielkummer.github.io/git-flow-cheatsheet/](http://danielkummer.github.io/git-flow-cheatsheet/)) to track my work.   
Basically I'll develop every lesson as a separated branch (`l<#lesson>`) from `develop` (I'll try to remember to track them). Once completed a lesson I'll merge it into `develop`, releasing a.. ehm...`release` (will try to track them too) with a `tag` and I'll push it to the `master` passing through `release/<#release>` branch.    
This is how it worked till now (well, it's my first release, so funny) but things *might* change. After all it is a **playground**, isn't it?    