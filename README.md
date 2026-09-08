# git-update-all

## !!!!! ATTENTION !!!!!

This project has been permanently moved to Codeberg
([git-update-all](https://codeberg.org/dinofp/git-update-all)) and is no longer actively
maintained on Github. Do not use the Issues system on Github to report to us.
Don't bother forking or getting source from here as it will not be updated.

Microsoft is not a friend of open-source and we do ourselves a disservice
giving them this impressive power over our work.

Never forget 2020 when Github (a Microsoft product) removed the popular
open-source `youtube-dl` project, sparking enormous controversy. The issue is
not that pushback eventually prompted reinstatement - Github can and will act
like this against us at any time.

## !!!!! ATTENTION !!!!!

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
