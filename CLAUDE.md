# CLAUDE.md

Guidance for AI coding agents (Claude Code, Copilot, Cursor) working in **sonic-pi**.

This is bamr87's fork of [Sonic Pi](https://github.com/sonic-pi-net/sonic-pi), the live-coding music synthesizer. The fork tracks the upstream `dev` branch (upstream has no `main`); changes here are occasional fork-local tweaks, not a divergent product.

## Build & run

Do not duplicate build instructions here — they live upstream in the platform-specific build docs at the repo root: `BUILD-LINUX.md`, `BUILD-MAC.md`, `BUILD-RASPBERRY-PI.md`, and `BUILD-WINDOWS.md`. See also `TESTING.md` and `CONTRIBUTING.md` for the upstream test and contribution workflow.

## Conventions

- Conventional Commits: `type(scope): description` (`feat`/`fix`/`docs`/`refactor`/`test`/`chore`/`ci`).
- Default branch is `dev` — branch from it and open a PR; never push to it directly.
- README-First, README-Last: read the nearest `README.md` before changing a
  directory, and update it after.
- Don't suppress type errors (`as any`, `@ts-ignore`, `# type: ignore`) or
  leave empty exception handlers.

## Fleet context

This repo is one of ~40 managed by the [bamr87/bamr87 dash](https://github.com/bamr87/bamr87) (registry: `_data/projects.yml`; tiered baseline: `docs/STANDARDS.md`). It is vendored there as a git submodule: commit and push changes **here** first — the hub only bumps its pointer afterwards. Shared CI, release, schema, and agent kits are seeded from the hub's `templates/`; prefer adopting those over hand-rolling equivalents.
