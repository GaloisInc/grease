# Development practices

This document describes the current development practices of GREASE maintainers.
It is descriptive, not normative. The idea is to make it easy for new
contributors to get started.

We generally try to balance the goals of producing high-quality (featureful,
correct, maintainable) software with experiencing joy during its development.
Happily, these goals often coincide rather than conflict.

Our practices are generally rooted in substantial trust in one another's
judgment, grounded in a sense of shared goals and direction. In particular, we
trust one another to make exceptions and ignore or modify conventions whenever
it makes sense to do so.

## In brief

- We use GitHub issues to track upcoming work.
- We make changes via pull requests and require review and approval before merge.
- We review in a timely manner (generally within one business day).
- We have no recommended merge strategy (squash vs. rebase vs. merge commit).
- We use automated testing and CI pervasively.

## Pull requests

The following state diagram shows our normal pull request (PR) workflow. The
transitions are labeled by  actions by the  **A**uthor and **R**eviewer. Some
points to note:

- We generally expect that each comment thread in a PR review will be
  "resolved", most often by (1) answering the question therein, (2) making a
  change to the PR, or (3) explicitly deferring the change to a new issue and
  adding a numbered TODO (see [Style](style.md)). Authors or reviewers may
  resolve comment threads as appropriate.
- Reviewers often approve before all threads are resolved, trusting the author
  to appropriately address them before merging.
- The author requests a new review when they have made substantial changes after
  approval. Whether a change is substantial enough to warrant another review is
  left to the author's judgment.
- The author (rather than the reviewer) merges the PR. We have no
  recommended/default merge strategy (squash vs. rebase vs. merge commit).
  Instead, we make a decision in each case that supports a comprehensible git
  history. We assume developers are familiar with the trade-offs between these
  options.

```
                   ┌───────────────┐
                   │     Issue     │
                   └───────────────┘
                          │
                          │ A: Assign to self
                          ▼
                   ┌───────────────┐
                   │  In progress  │ ─────────────┐
                   └───────────────┘              │
                          │                       │
                          │ A: Open draft PR      │
                          ▼                       │
              ┌─── ┌───────────────┐              │ A: Open PR
 A: Revisions │    │   Draft PR    │              │
              └──⏵ └───────────────┘              │
                          │                       │
                          │ A: Mark PR as ready   │
                          ▼                       │
 A: Revisions ┌─── ┌───────────────┐ ⏴────────────┘
 R: Comments  │    │      PR       │
              └──⏵ └───────────────┘ ⏴───┐
                          │              │
                          │ R: Approve   │ A: Re-request review, or
                          ▼              │ R: Un-approve
              ┌─── ┌───────────────┐ ────┘
 A: Revisions │    │   Approved    │
              └──⏵ └───────────────┘ ⏴────────────┐
                          │                       │
                          │ CI passes, and        │ CI fails, or
                          │ Comments are resolved │ R: Comments
                          ▼                       │
A: If needed, ┌─── ┌───────────────┐ ─────────────┘
rewrite git   │    │     Ready     │
history       └──⏵ └───────────────┘
                          │
                          │ A: Merge
                          ▼
                   ┌───────────────┐
                   │    Merged     │
                   └───────────────┘
```
