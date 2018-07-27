# git-update-all


## Synopsis

Perform a git pull in every subdir (Haskell)


## Description

A script to perform a `git remote update` in every repo in a directory on the
local system. It's intended to synchronize one server's repos with another.

The local repo copies will need to be what's known as "mirror clones" of the
originals to ensure it gets all the changes. To make this type of clone:

    $ git clone --mirror https://path/to/project.git


## Getting source

- Get the source with git: `$ git clone https://github.com/dino-/git-update-all.git`
- If you're just looking, [browse the source](https://github.com/dino-/git-update-all)


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>
