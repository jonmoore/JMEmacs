---
description: Compare local JMEmacs with upstream and create a detailed report
---

### Task

Run the compare.sh script to check differences between the local JMEmacs installation and the latest master branch from GitHub, then create a comprehensive report of the findings.

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

Use org-mode format throughout
- Use org-mode heading levels (*, **, ***)
- Use `#+BEGIN_SRC` blocks for code/diffs
- Use `- [ ]` for checklists
- Use `#+BEGIN_EXAMPLE` for command output

### Output

Save the report to `comparison-report.org` and return the filename and a brief summary of findings to the user.
