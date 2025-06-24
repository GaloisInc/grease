# Discussion of GREASE formatting choices

GREASE enforces an opinionated formatter in continuous integration (CI) via a [fourmolu](https://github.com/fourmolu/fourmolu) formatting lint.
Enforcing a formatting choice in CI enables GREASE developers to use automated formatters in either pre-commit or format-on-save without formatting unrelated
text by maintaining a consistent format across the repository. This consistency limits time spent in review discussing formatting choices (e.g. case branch indentation or import grouping)
and sets clear expectations with respect to formatting requirements for accepting a pull request. These clear expectations are particularly useful for new developers and open source contributors
who may not be familiar with the style preferences of GREASE developers. Below we highlight some pain points introduced by this approach and suggested mitigations.

## Additional setup overhead introduced by a formatting Tool

In the [dev guide](./dev.md) we provide installation and setup instructions. Additionally, in VSCode, selecting fourmolu as the autoformatter for a workspace asks to install
the formatting tool automatically.

## Code readability

While, in general, autoformatters lead to consistent expectations with respect to style, they can alter developer intended whitespace. Sometimes these formatting choices may negatively impact readability.

Developers can locally disable formatting with the following magic comments:
```haskell
{- FOURMOLU_DISABLE -}

{- FOURMOLU_ENABLE -}
```

Developers should balance tradeoffs between consistent formatting and readability constraints when deciding whether to disable the formatter for some span.

## Developer confusion encountering CI formatting failures

Developers may encounter unexpected formatting failures if they do not use fourmolu locally. The [dev guide](./dev.md) offers advice on how to
setup fourmolu locally for autoformatting. Additionally, the CI job is named `Check formatting with fourmolu` to provide hints as to where a
developer should look if encountering this failure.

## Commit hygiene

Commit hygiene helps isolate functional groups of changes. Commit hygiene enables reviewing a change in isolation without having to consider unrelated changes, and also allows reverting or cherry-picking commits that address specific pieces of functionality if required. In general, enforcing a consistent style helps maintain commit hygiene by avoiding commits with inconsistent formatting being merged into the main branch. Unformatted code then needs to be fixed post-hoc with additional formatting changes meaning there are multiple commits that have to be grouped together to revert a specific feature (for example).

Enforcing a formatting style, however, does mean developers cannot delay formatting choices to a later date. Ideally, developers use a local formatting setup described in the [dev guide](./dev.md) to prevent
the need for post-hoc formatting commits, but these types of formatting fix commits are somewhat inevitable. There are at least two ways to handle this situation:
1. If the PR is amenable, squashing the commits can be a good option. Then the feature commits are merged with the formatting fix, resulting in a single commit that implements the intended feature with the correct formatting, resulting in a minimal,
local change.
2. If squashing would merge too many unrelated changes within the PR, then sequencing the formatting-fixup commit into a single formatting commit that applies to the entire feature branch is preferred. This strategy makes the formatting commit explicit, and separate from functional changes.

When possible, squashing formatting commits into the related feature is preferred because then the feature can be reverted on its own without also tracking a formatting commit.