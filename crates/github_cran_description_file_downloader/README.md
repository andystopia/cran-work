# GitHub CRAN Description File Reader

The basic premise of this is that the CRAN hosts an index of the CRAN.
This allows us to more efficiently scrape *that* rather than download
the entire CRAN, which is, needless to say, pretty sizeable.

So let's get into it. I think the idea is that we'll basically do,

```
cran-description-files/
  pkg_name/
    version-a/
      DESCRIPTION
    version-b/
      DESCRIPTION
```

This ought to give us a workable structure for querying the whole index.
Once this is done, we'll actually have enough information to really work with.

I think I'll probably use a little tokio and reqwest to make this one happen, we
will need the GitHub API
