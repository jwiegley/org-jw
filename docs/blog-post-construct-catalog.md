# Blog-post syntactic-construct catalog (johnwiegley + newartisans)

Catalog of every non-paragraph Org construct appearing across the two blog
corpora, with counts, validity, and per-output impact. Purpose: define the
extra strictness rules that a future `org-jw lint --blog` / `--strict` mode
should enforce, beyond the standard Org checks already in
`org-lint/src/Org/Lint.hs`.

- **johnwiegley/posts** — 1018 `.org` files. Personal essays & **poetry**.
- **newartisans/posts** — 107 `.org` files. Technical posts & **code**.

Date of survey: 2026-05-27. Numbers come from `rg` over the raw files
(authoritative) cross-checked against the Postgres index.

---

## 0. Method & three load-bearing caveats

1. **The DB undercounts links/bodies.** 975 / 1018 johnwiegley posts are
   *headline-less* (all content lives in the file **preamble**), and the index's
   `entry_links` only captures links *under headings*. So link/body tallies
   here come from `rg` on raw text, not the DB.
2. **Raw-text tallies pick up code-block content.** e.g. all 119 `^# ` "comment"
   lines in newartisans are shell/Python comments *inside* `#+begin_src`, and
   bash `[[ -f $x ]]` test syntax matches the bracket-link regex. Prose-construct
   counts below are stated net of obvious code-block noise where it matters.
3. **org-jw's own parser is deliberately shallow** (`org-types`, `org-parse`):
   bodies are stored as raw `[String]` lines, *all* `#+begin_X` blocks collapse
   to one opaque `BeginDrawer "label"`, and inline markup / links / lists /
   tables / footnotes are **not modeled at all**. Consequence: every blog-strict
   check must be a **regex over the raw body text** (the `bodyString`/`paragraphs`
   helpers already used in `Lint.hs`), not an AST query.

---

## 1. The two rendering targets and what each breaks

| | **Website** (`org-site/src/Org/Site.hs`) | **PDF book** (`make books`) |
|---|---|---|
| Pipeline | Pandoc Org-reader → HTML5, `writerHighlightMethod = NoHighlighting`, TOC depth 2; then a custom inline pass fixes `id:` links + `./slug/` image paths; `wordpressifyUrls` | Pandoc Org→markdown + `book/filters/org-post-to-markdown.lua` → concat all posts → Pandoc markdown→LaTeX (`xelatex`), one post = one `\chapter` |
| `file:` links | **broken** — emitted as literal `href` (no fixup) | **broken** — dead `\href{…}` |
| `id:` links | **resolve** — `findEntryByUuid` matches the post's file-level `#+ID:` and rewrites to its route (Site.hs:685) | **dead** — book pipeline has *no* id resolver |
| `#+begin_export html` | injected **raw & unsanitized** | non-comment HTML **silently dropped** by the LaTeX writer |
| `#+begin_export latex`/other | dropped | raw LaTeX *blocks* pass through unfiltered → can **break xelatex** |
| missing/remote/`attachment:` images | broken `<img>`/`href` | Lua filter degrades to a text link (safe) |
| `_` in prose (`a_b`) | → `<sub>` subscript | → `\textsubscript` |
| `\x` in prose (`foo\bar`) | text after `\` **dropped** | neutralised to code |
| wide table | fine (HTML) | `\longtable`, can overflow margin |
| table inside footnote | fine | `\footnote{\longtable}` → **fragile/illegal LaTeX** |

The verse/src/example preformatted family is the axis of the poetry-vs-code
rule; see §4.

---

## 2. Construct catalog — counts by site

`F` = files containing, `N` = total occurrences. "✓ ok" = wanted/clean;
"⚠" = needs a rule; see §3 for the problem instances.

### 2.1 Document / affiliated keywords (`#+keyword:`)

| Keyword | johnwiegley | newartisans | Notes |
|---|---|---|---|
| `#+title:` | 1018 / 1018 | 107 / 107 | ✓ universal, expected |
| `#+filetags:` | 1018 / 1018 | 107 / 107 | ✓ universal |
| `#+category:` | – | 17 | newartisans only |
| `#+caption:` | 1 | 6 | ⚠ johnwiegley one is **mid-body** (parser ignores it) |
| `#+date:` | – | 4 | |
| `#+tags:` | – | 2 | redundant with filetags? |
| `#+startup:` | – | 2 | editor hint, harmless |
| `#+seq_todo:` | – | 1 | ⚠ TODO-workflow keyword in a *blog* post |

Plus a `:PROPERTIES:` drawer per post carrying `:ID:`/`:CREATED:` (johnwiegley
1018 file-level; newartisans 392 across entries). No `:LOGBOOK:` or custom
drawers anywhere. ✓

### 2.2 Blocks (`#+begin_X … #+end_X`)

| Block | johnwiegley (F / N) | newartisans (F / N) | |
|---|---|---|---|
| `verse` | **457 / 458** | 0 / 0 | poetry (johnwiegley only) ✓ |
| `quote` | 158 / 462 | 7 / 11 | ✓ (no nesting — verified) |
| `example` | 36 / 52 | 40 / 227 | monospace literal |
| `src` | **0 / 0** | **47 / 223** | code (newartisans only) ✓ |
| `export` | 10 / 84 | 61 / 61 | see §3.3 |
| `center`,`comment` | 0 | 0 | absent |

Block begin/end are **balanced, terminated, and unnested** in both corpora
(an apparent newartisans delta of 1 was a case-sensitivity counting artifact;
the structural pass finds nothing). ✓

### 2.3 Headlines

| | johnwiegley | newartisans |
|---|---|---|
| files with headlines | 43 | 43 |
| total headlines | 159 | 312 |
| TODO keywords | 0 | 0 |
| priority cookies `[#A]` | 0 (the 6 hits are example content in one meta-article) | 0 |

johnwiegley headlines are benign editorial section breaks (e.g.
`* RSS now available`). ✓ — but a blog post arguably shouldn't carry TODO
workflow at all (see `#+seq_todo:` above).

### 2.4 Links

| Link form | johnwiegley (F / N) | newartisans (F / N) | Verdict |
|---|---|---|---|
| `[[t][desc]]` described | 44 / 73 | 68 / 263 | ✓ |
| `[[t]]` plain | 21 / 37 | 12 / 31 | ✓ |
| `[[http(s):…]]` | 19 / 26 | 62 / 254 | ✓ |
| bare `http(s)://` | 23 / 31 | 66 / 264 | ✓ (autolinked) |
| **`[[file:…]]`** | **52 / 74** | 0 | **⚠ ALL broken** (§3.1) |
| `[[id:…]]` | 9 / 29 | 3 / 4 | ⚠ resolve on web, **dead in PDF** (§3.2) |
| `[[./slug/img]]` rel image | 0 | 11 / 15 | ✓ all exist on disk (1 TIFF — §3.5) |
| `[[/abs/path]]` internal | 0 | ~8 | ⚠ stale permalinks (§3.4) |
| `[[ftp://…]]` | 0 | 4 | ⚠ dead download host (§3.4) |
| `[[irc://…]]` | 0 | 2 | ⚠ unusual scheme |
| `[[mailto:…]]` | 0 | 2 | ✓ |
| `[[#custom-id]]` | 0 | 1 | ✓ if anchor exists |
| `[[attachment:…]]` | 0 | 0 | absent (would break both outputs) |

### 2.5 Inline markup (prose, net of code-block noise)

| Markup | johnwiegley | newartisans | |
|---|---|---|---|
| `/italic/` | 217 / 581 | 40 / 81 | ✓ heavy in essays (emphasis, foreign words, titles) |
| `=verbatim=` | 2 / 3 | 84 / 1371 | ✓ code spans dominate newartisans |
| `*bold*` | 4 / 40 | 21 / 43 | ✓ |
| `~code~`,`_underline_`,`+strike+` | 0 | 0 | absent |

org-jw does not model inline markup; it round-trips as raw text, so these are
a rendering concern only (see the `_`/`\` prose hazards in §1).

### 2.6 Lists, tables, footnotes, math, misc

| Construct | johnwiegley (F / N) | newartisans (F / N) | |
|---|---|---|---|
| unordered list `- /+` | 11 / 77 | 13 / 113 | ✓ |
| ordered list `1./1)` | 12 / 79 | 29 / 143 | ✓ |
| description `term ::` | 0 | 1 / 4 | ✓ |
| checkbox `[ ]` | 0 | 0 | absent |
| org table `\| … \|` | 1 / 3 | 12 / 83 | ✓ (max width 68 ch — no overflow); 1 johnwiegley table lacks a header rule |
| footnotes (ref / def) | 71f · 536 / 268 | 2f · 8 / 4 | ✓ citation style; **no** footnote-embedded tables (latent PDF break absent) |
| inline `$…$` math | 0 | 14 / 50 | ✓ newartisans; relies on site MathJax |
| `\( \)` / `\[ \]` | 0 | 4 / 7 · 1 / 2 | ✓ |
| horizontal rule `-----` | 2 / 5 | 0 | ✓ |
| line break `\\` | 0 | 1 / 3 | ✓ |
| `# ` comment | 0 | 0 real (119 = code inside blocks) | ✓ |
| macros `{{{…}}}`, targets `<<…>>`, sub/superscript `^{}`/`_{}`, inline tasks, `#+TBLFM:`, `#+RESULTS:`, stat cookies `[n/m]` | 0 | 0 | **absent** in both corpora |

---

## 3. Invalid / problematic constructs found (the defects)

### 3.1 Broken `file:` links — johnwiegley, 52 files / 74 occ — **HIGH**
Every `file:` target is a non-`.org` legacy WordPress slug; none resolve, on
the website or in the PDF. Breakdown:
- **59** bare-slug / `slug#anchor` cross-references to *other posts*
  (`[[file:detachment]]`, `[[file:soul.of.rebellion]]`, `[[file:all.poems#sec6]]`)
  — these should be `id:` links to the target post.
- **10** dead gallery links (`[[file:gallery/California/Aptos/index.html]]`).
- **1** image whose file is **missing on disk**
  (`[[file:images/patbunny.jpg]]` in `20040501-pat-the-bunny.org`).

### 3.2 `id:` links dead in the PDF — johnwiegley 9f/29 + newartisans 3f/4 — **MED**
All 32 distinct targets resolve to an existing post's file-level `#+ID:`, so
they work on the website. The **book pipeline has no id resolver**, so each is
a dead `\href{id:UUID}` in the PDF.

### 3.3 Raw-HTML `<table>` export blocks lost in PDF — johnwiegley, 1 file — **MED**
`20111116-turn-thereunto.org` wraps Persian parallel-text in ~74
`#+begin_export html` blocks containing `<table class="persian">…`. These
render on the website but markdown→LaTeX **silently drops all raw HTML**, so
the content **vanishes from the book**. (All other export blocks in both
corpora are exactly the `<!--more-->` teaser marker — 10 johnwiegley files,
61 newartisans files — which is benign: it renders on the web and correctly
disappears from the PDF.)

### 3.4 Stale internal links — newartisans — **MED/LOW**
- ~8 absolute-path permalinks (`[[/2009/03/hello-haskell-goodbye-lisp.html]]`,
  `[[/contact.php]]`, `[[/downloads_files/modpython_gateway.py]]`) — old
  WordPress paths, broken in both outputs.
- 4 `ftp://ftp.newartisans.com/…` download links (host long dead).
- 2 `irc://` links (unusual scheme, won't render as a useful link).

### 3.5 Risky image format — newartisans, 1 file — **LOW**
`[[./…/network-stack.tiff]]` — TIFF isn't web-displayable and
xelatex/`xdvipdfmx` embedding is unreliable. (A sibling PNG,
`ping-www.marius-fabre.fr.png`, is already hardcoded in the Lua filter as a
"convert to link" workaround because it overflows xdvipdfmx — evidence real
images do break the build.)

### 3.6 Mid-body affiliated keyword — johnwiegley, 1 file — **LOW**
`#+caption:` appears in the body of `20040501-pat-the-bunny.org`. org-jw's
parser only recognises `#+keyword:` lines at the file-header position, so this
caption is treated as plain paragraph text by the tool (Pandoc still honors it).
Also note `#+seq_todo:` (newartisans) is a TODO-workflow keyword that has no
business in a published post.

### Verified NOT problems (clean now, or latent-but-absent)
- Block balance / termination / **nesting**: clean — no nested quotes or blocks.
- **Footnote-embedded tables**: none (the `\footnote{\longtable}` PDF break is latent only).
- **Wide tables**: none (widest row 68 chars).
- **Poetry/code violations**: **none today** — johnwiegley has 0 `src`,
  newartisans has 0 `verse`. The rule already holds; lint would *keep* it true.

---

## 4. The poetry-vs-code rule (confirmed against both renderers)

Preformatted output is **mutually exclusive by site**: `verse ⊕ src`.

| Construct | Website | PDF | Meaning |
|---|---|---|---|
| `#+begin_verse` | `<div class="line-block">`+`<br/>` | LineBlock → `\\` lines | **poetry** (proportional font, line-accurate) |
| `#+begin_src L` | `<pre class="L">` (no coloring) | fenced → syntax-highlighted, line-wrapped | **code** |
| `#+begin_example` | `<pre class="example">` | fenced monospace | literal monospace (either site) |

- **johnwiegley** → `verse` for poetry; **`src` must not appear** (would render
  poetry as code). Reserve `<pre>` for verse.
- **newartisans** → `src`/`example` for code; **`verse` must not appear** (would
  strip code's monospacing/indentation). Reserve `<pre>` for code.

Neither renderer *enforces* this — they happily render the wrong block; it just
looks wrong. So it is a lint rule, and it needs to know **which site** a file
belongs to (see §6 open questions).

---

## 5. Candidate strict-lint rules (for the design phase)

Grouped; severities are a starting suggestion.

**A. Link integrity**
- A1 `BlogLegacyFileLink` (Error) — any `[[file:…]]`. Suggest `id:`/`http`.
- A2 `BlogUnresolvedIdLink` (Error) — `[[id:UUID]]` whose UUID is no known post `#+ID:`.
- A3 `BlogAbsoluteInternalLink` (Warn) — `[[/path…]]` site-absolute permalink.
- A4 `BlogNonWebScheme` (Warn) — `ftp:`/`irc:`/`attachment:`/other non-`http(s)`/`id`.
- A5 `BlogMissingRelativeImage` (Error) — `[[./slug/img]]` whose file is absent.

**B. Web ∩ PDF portability**
- B1 `BlogRawHtmlBlock` (Warn) — `#+begin_export html` whose body is anything
  other than the `<!--more-->` marker (block-level HTML is lost in the PDF).
- B2 `BlogNonHtmlExport` (Warn) — `#+begin_export <backend≠html>` (dropped on web; raw LaTeX risks xelatex).
- B3 `BlogRiskyImageFormat` (Warn) — image links ending `.tiff`/`.bmp`/etc.
- B4 (latent) `BlogTableInFootnote` (Error) / `BlogWideTable` (Warn) — guard the PDF.

**C. Site block discipline (needs site identity)**
- C1 `BlogSrcOnPoetrySite` (Error) — `#+begin_src` in johnwiegley.
- C2 `BlogVerseOnCodeSite` (Error) — `#+begin_verse` in newartisans.

**D. Keyword/affiliated hygiene**
- D1 `BlogMidBodyAffiliatedKeyword` (Info) — `#+caption:`/`#+name:`/`#+attr_*` not at header position.
- D2 `BlogWorkflowKeywordInPost` (Warn) — `#+seq_todo:`, TODO keywords, priority cookies in a post.

**E. Prose hazards (Pandoc Org-reader semantics; Info)**
- E1 unescaped `_` producing an unintended subscript span.
- E2 stray `\` that silently drops following text.

---

## 6. How org-jw would be extended (mechanism only — design deferred)

From `org-lint`/CLI analysis:

- There is **no per-rule config today**; the only gate is the global severity
  threshold (`-l`). Rules are hardcoded statement lists in `lintOrgFile'`
  (`Lint.hs:182`) and `lintOrgEntry` (`Lint.hs:403`).
- To add a mode: add `_strict`/`_blog` (or a `LintMode`) to `LintOptions`
  (`org-jw/bin/Lint/Options.hs`), thread it through `lintOrgFiles` →
  `lintOrgFile'` → `lintOrgEntry` alongside `level`, and guard the new rules
  with `when blog …`. Each new check needs a `LintMessageCode` constructor
  (`Lint.hs:75`) + a `showLintOrg` arm (`Lint.hs:991`).
- New checks read prose via the existing `bodyString`/`paragraphs` helpers —
  **regex on raw lines**, since the AST doesn't model these constructs.
- Precedent for per-file opt-out already exists as magic file properties
  (`#+IGNORE_LINKS:`, `#+WHITESPACE: ignore`, `#+NOSLUG:`).

## 7. Open questions for the design phase
1. **Site identity:** how does the linter know johnwiegley (poetry) vs
   newartisans (code)? Options: directory path, a `#+SITE:` file keyword, or
   config. Needed for rules C1/C2.
2. **`file:` → `id:` migration:** most bare-slug `file:` links map to an
   existing post — auto-fixable, or just flag?
3. **turn-thereunto:** convert its HTML parallel-text tables to real Org tables
   (so the PDF keeps them), or exempt the file?
4. **Where it runs:** fold blog checks into `make lint`, or a separate
   `make lint-blog`? Hard error vs warning per rule.
