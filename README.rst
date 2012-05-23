==========================================
 Environmental Layers repository overview
==========================================

At the top-level, the repository is split into four main topical domains
(``climate``, ``derived-climate``, ``land-cover``, and ``terrain``),
plus an additional ``shared`` directory for general project-level
materials. Each of the topical directories themselves contain up to 6
predetermined subdirectories as indicated below. See descriptions for
specific criteria for determining what goes in each directory.

**All committed work should fall within this directory structure.**

``research``
  Scripts, notes, generated summaries/reports, and possibly additional
  relevant background information intended to inform and/or evaluate
  methodological decisions. The defining criterion is that these
  materials are *not* designed to be used as operational components of
  the eventual data production workflow itself, but instead serve to
  guide its development. Examples include case studies, statistical
  model comparisons, dataset assessments, validation procedures, etc. 

``procedures``
  Scripts that either have been, could be, or will be executed with the
  express purpose of *producing data*, including acquisition of data
  from elsewhere, creation of intermediate datasets, and production of
  final layers. Prose (or mixed prose/text) text documents may be
  substituted for executable scripts if absolutely necessary, e.g. to
  express procedures that had to be performed manually.

``tests``
  Scripts intended for testing whether particular data holdings meet
  expectations (e.g., completeness, consistency, etc), or for testing
  whether particular scripts produce output as expected given specified
  test inputs. Although not necessarily the case while in-development,
  ultimately all tests should be able to run without intervention,
  reporting either success or failure along with appropriate supporting
  details.

``doc``
  Documentation and associated files (figures, thumbnail images, etc).
  Strong preference should be given to text-based documentation
  (including standard markup formats), and especially reproducible
  documentation schemes such as LaTeX/Sweave.

``lib``
  Source code (possibly with supporting materials such as Makefiles)
  that contains generally useful and reusable bits of functionality that
  are or could be leveraged elsewhere. The idea is that these would be
  imported, included, sourced, etc. by scripts in one or more other
  project directories.

``extra``
  Miscellaneous notes, code snippets, blue-sky scripts, and other
  materials that seem worth capturing into this repository, but don't
  fit under any other directories.


*Directory tree:*
::

 .
 ├── climate/
 │   ├── research/
 │   ├── procedures/
 │   ├── tests/
 │   ├── lib/
 │   ├── doc/
 │   └── extra/
 ├── derived-climate/
 │   ├──
 │   ├──
 │   ├── <as above>
 │   ├──
 │   ├──
 │   └──
 ├── land-cover/
 │   ├──
 │   ├──
 │   ├── <as above>
 │   ├──
 │   ├──
 │   └──
 ├── terrain/
 │   ├──
 │   ├──
 │   ├── <as above>
 │   ├──
 │   ├──
 │   └──
 └── shared/ 

