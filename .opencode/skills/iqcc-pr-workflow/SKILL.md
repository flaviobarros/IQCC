# IQCC Issue-to-PR Workflow Skill

End-to-end workflow for implementing a GitHub issue and opening a well-documented
Pull Request. Follow this sequence for every issue.

## Phase 1: Understand and plan

1. Read the issue body (title, description, acceptance criteria, dependencies).
2. Read referenced functions, test files, and related issues.
3. Create a `todowrite` with all tasks for the implementation.
4. If the task matches a specialized agent, delegate:
   - `free-implement` for scoped coding
   - `free-docs` for vignettes/large documentation
   - `free-explore` for codebase research
   - `free-review` for pre-PR review
   - Ask user before calling `gpt-orchestrator` or `sol-reviewer`

## Phase 2: Branch

```bash
# From up-to-date master
git fetch origin
git checkout master
git pull --no-rebase origin master

# Create focused branch
git checkout -b feat/descriptive-name

# Branch naming convention:
#   feat/    — new feature or function
#   docs/    — documentation, vignettes, roxygen
#   fix/     — bug fix
#   audit/   — scientific validation or audit
#   release/ — release preparation
```

## Phase 3: Implement

1. Follow existing API patterns in the package (see `iqcc-roxygen-contract` skill
   for documentation, `iqcc-scientific-validation` for tests).
2. Write tests **before or alongside** the implementation (test-driven where
   feasible).
3. Run the narrowest check that proves the intermediate change:

```bash
Rscript -e 'devtools::test(filter = "pattern")'
```

4. Only stage intended files — never stage `sources/`, `docs/`, or temp files.

## Phase 4: Full validation ladder

Before committing, run the full validation:

```bash
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
Rscript -e 'devtools::check()'
git diff --check
git status --short
```

- If `check()` fails, fix errors first, then warnings, then notes.
- If the tracked `docs/` blocks pkgdown, build to a temp dir instead.
- Confirm `sources/` is excluded from both Git and R package builds.

## Phase 5: Commit

```bash
git add <specific files>  # never "git add ." or "git add -A"
git diff --cached --name-only  # verify only intended files
git commit -m "type: concise description (#issue)"
```

Commit message conventions:
- `feat:` — new function or feature
- `refactor:` — code change that reuses new pure functions
- `docs:` — documentation only
- `test:` — test additions
- `fix:` — bug fix
- `chore:` — housekeeping

Reference the issue number in the commit body.

## Phase 6: Push and PR

```bash
git push origin <branch-name>

# Write PR body to a temp file to avoid shell escaping issues
cat > /tmp/pr-body.md << 'PRBODY'
## Descrição

(Summarize what was implemented and why, in Portuguese or English as the issue
language.)

### Mudanças

- (list of key changes)

### Validação

- (test results: "0 errors, 0 warnings, 1 NOTE pre-existente")
- (key numerical results if scientific validation)

### Checklist

- [ ] Acceptance criteria items (check off what's done)

Closes #<issue>
PRBODY

gh pr create \
  --base master \
  --head <branch-name> \
  --title "[P<N>] Issue title (#<issue>)" \
  --body-file /tmp/pr-body.md
```

## Phase 7: Post-PR

1. Report the PR URL to the user.
2. If the user requested review from a paid agent (`sol-reviewer` or
   `gpt-orchestrator`), ask for explicit confirmation before invoking.
3. Do not merge the PR unless explicitly asked.
