# Writing A Version Resolver.

So what we essentially, are given, is a list of packages,
and these packages depend on other packages, and these packages depend on other packages, and so on.

However, in order to be able to tell which package needs which dependencies, we
need to *lock* the package versions, because packages can change their dependencies
between versions, therefore the tree isn't all fully known right up front.
