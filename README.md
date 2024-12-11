# Cran-Work


I'm doing a little experimentation into the R ecosystem, looking to build 
a system for R package management that is more robust and cost effective than 
what currently exists. 


# Features

## Generating A Rattler-Build Recipe From DESCRIPTION.

This project can generate rattler-build recipe manifests from source.

After cloning this repo, the command:

```sh
cargo run --release -p description-to-rattler generate-cran Matrix --export
```

should generate an r-matrix directory in the current folder which contains the rattler-build yaml file. See https://github.com/wolfv/r-forge for examples for how to compile these R files. Output is designed to match those files as much as possible, and indeed, a great deal of the code that generates the YAMLs comes directly from the `rattler-build` crate.


# Project Layout

- `crates/description-to-rattler`: convert DESCRIPTION files off of the CRAN into
  rattler-build yamls. Supports alternative CRAN's to r-project.org, and can print `DESCRIPTION` files. Can also output yaml files for historical versions of packages.
- `crates/cran-description-file-parser`: a (mostly) zero-copy, chumsky-based parser for R DESCRIPTION files. Supports `miette` and `ariadne` error messages, comments in file, strongly typed parsing of dependencies, and error recovery on failed field parses.
- `crates/cran-fetch`: currently a rudimentary R package version solver.
- `crates/crancherry`: an abstraction over the CRAN which abstracts
  having to traverse the directories yourself, and simplifies interfacing with it.
- `crates/mini-r`: a miniature R language which hopefully will someday be
  good enough to create a sandboxed execution environment to execute
  the R code present in DESCRIPTION files.
- `crates/description-file-crater`: attempts to parse every DESCRIPTION file of the 
  most recent version of every package on the CRAN. Caches downloads and uses github
  when available to diminish load on the CRAN server.


# Problems to Solve.

## The CRAN has a reproducibility issue.

CRAN has some Windows/Mac builders which compile packages for easier distribution. 
This pre-compilation reduces likelihood of a compilation error, accelerates package
and installs, and on Windows/Mac, reduces the need to install compilers (usually FORTRAN, C++, and Rust). 

While it's great that the CRAN provides these resources, it deletes old versions of 
package binaries, when new versions are released such that only the most recent version
of the package is hosted, built against the latest 3 breaking changes of R. 

This means it's easy to set up a project now, but 3 months from now when a package has a
breaking change, full re-compilation is necessary, as _well_ as knowing which dependency
the project was originally built with, which is not always very easy to determine, as
R projects do not track which dependencies they use by default.

More problems with binary substitution arise when working in a cross OS environment, 
when building on Mac and then deploying in Docker, the latest binary build may 
be behind the most recent, sometimes breaking, publication. This means that an
install.packages that was done when starting the project on Mac might work seamlessly, 
but an install.packages in the docker container could be using a newer version, 
breaking reproducibility, and might require system dependencies previously compiled
and linked into the R package installed on the host OS. 

## Direction to take this.

I'm not sure exactly how to solve these problems in a way that exports the same
old interface that R users love, with the _speed_ that the current method has. 

I have a full version solver based on the `pubgrub` crate in this repository, and 
it works, and I can scrape the CRAN DESCRIPTION files to do a correct and full, 
backtracking version solve. 

The most reasonable solution to me sounds like 
 1. Choose as many binary packages as you possibly can that satisfy the requirements, right 
    now, warning users of breakage across operating systems due to mismatched binary versions
    on the CRAN (this will be kind of slow, but *much* faster than hand-checking this).
 2. Spinning up a global cache build environment which will compile the packages, then 
    translocate these pre-compiled artifacts into projects when they are requested. 
    This also sets up some precedent for privately hosted build servers which could 
    do this binary substitution for whole teams (like Nix does).

## Why not use Nix?

Nix isn't available on Windows just yet; hopefully, that will change, but my 
experience with Nix is that it's a bit of an *experts* system, many things are written
with the assumption that you understand the intricacies and the amount of thought
that goes into reproducible packaging; that's to say the abstraction is very flat, but
also very shallow, you're put into the deep end very quickly. 

I also find that while Nix is reproducible, it tends to break on my MacOS box between 
major MacOS upgrades. This is kind of painful compared to more robust package 
managers such as the Homebrew, Conda, etc. 

I also am just of the preference that leans away from the mono-repo structure, having 
to understand the git workflow to submit packages is a bit of a high bar, imo. Having 
one unified system which integrates nicely and allows simple, version based publishing 
is the goal. I'm not opposed to snapshots  (a la Stackage from the Haskell ecosystem), 
but mono-repos just make it so challenging to fall back on old versions unless you're
a git legend. Perhaps this could be improved, but between private package repos, and
and the fact that it's not improved right now, makes it a hard choice to adopt for 
newcomers to software development.


## Very speculative direction to go.

It's possible that one thing I could do is see if rattler-build in combination 
with Pixi, could set up a global package environment which could build R packages
in isolation, and solve environments. 

### Concerns: 

1. The `conda-forge` does not have all the cran-packages. I'm not sure if I can 
   even detect the the ones it does have, I just find ones it *doesn't* have quite
   often.
2. `conda-forge` might not have all the versions that I want to depend on.

I think, to this end, setting up a local forge on a machine, writing a custom solver, 
determining which ones are available on the conda-forge, then determining which ones 
we need to build ourselves *and* building them in the local forge looks like the most
likely way to go. My only concern with this is pointing at that forge, where I don't 
believe rattler-build allows relative paths or substitution (so no like `~/.r-forge/` allowed
I don't believe). 

The advantage of a local forge approach is that hopefully you can use it to 
write an impure docker build which mounts the forge, pre-compiles, and writes
into it, and then can use them across builds, in order to only have one build per
project per library at most, rather than one build for every library per library added, 
reducing build time complexity from $n^2$ to $n$.

### Devotion.

I'm not sure how much time I'll be able to dedicate to this project, but, at the very 
least I hope my research, knowledge, and libraries produced come in handy for the 
broader R and Python ecosystem.