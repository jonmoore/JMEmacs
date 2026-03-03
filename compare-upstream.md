---
description: Compare local JMEmacs with upstream and create a detailed report
args: [format]
---

### Task

Run the compare.sh script to check differences between the local JMEmacs installation and the latest master branch from GitHub, then create a comprehensive report of the findings.

### Input

The user may specify an output format: `org` (default), `markdown`, `pandoc`, or `text`.

If no format is specified, use org mode format.

### Steps

1. **Run the comparison**: Execute `./compare.sh` to get the diff output
2. **Parse the results**: Categorize the differences into:
   - Files only in upstream (missing locally)
   - Files only in local (not in upstream)
   - Files that differ between versions
3. **Analyze each difference**: For files that differ or are missing, read the relevant files to understand what changed.
4. **Provide recommendations**: For each difference, suggest an action (update, merge, keep local, ignore, etc.)

### Report Sections

- **Summary**: High-level overview of differences found (counts of missing, extra, and differing files)

- **Files Missing Locally**: list files that exist in upstream but not locally
  - For each file: show its purpose and recommend whether to add it.

- **Files Only in Local**: List files that exist locally but not in upstream
  - For each file: identify if it's a local customization or should be added to upstream

- **Files That Differ**: For each file with differences:
  - Show key changes (use diff output)
  - Explain what changed and why it might matter
  - Recommend action: update to match upstream, keep local version, or merge changes

- **Recommended Actions**: Provide a step-by-step checklist of what to do:
  - Commands to copy missing files
  - Files to review manually
  - Conflicts to resolve

- **Appendices**: Full diff output for reference

### Formatting

**Org Mode** (default)
- Use org-mode heading levels (*, **, ***)
- Use `#+BEGIN_SRC` blocks for code/diffs
- Use `- [ ]` for checklists
- Use `#+BEGIN_EXAMPLE` for command output

**Markdown**
- Use markdown heading levels (#, ##, ###)
- Use triple back ticks for code blocks
- Use `- [ ]` for checklists

**Pandoc**
- Use markdown with pandoc extensions
- Include YAML frontmatter with title and date
- Use fenced code blocks with syntax highlighting

**Plain Text**
- Use simple formatting with dashes and indentation
- No special markup
- Clear section separators

### Output

Save the report to a file named:
- `comparison-report.org` (org mode)
- `comparison-report.md` (markdown)
- `comparison-report.pandoc.md` (pandoc)
- `comparison-report.txt` (text)

Return the filename and a brief summary of findings to the user.
