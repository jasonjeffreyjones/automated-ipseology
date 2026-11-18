# Ipseological Analyses maintainer instructions

This repository runs an unattended scientific publication workflow. Preserve the separation of responsibilities: Python owns deterministic acquisition, validation, publication, Git, and deployment; Codex owns only the daily scientific analysis and article.

## Safety boundaries

- Never read or copy credentials from `/home/ubuntu/ipseity-daily/`, and never modify that project.
- Never put credentials, respondent-level data, private provenance, logs, or working directories in Git.
- Do not expose `hashed_respondent_id` values in articles, results, figures, logs intended for publication, or HTML.
- Do not run generated R outside the standard Codex CLI sandbox used by `generate-post.py`.
- Do not weaken dataset, output, HTML, or navigation validation merely to make a failed run publish.
- Do not publish a partially assembled entry. A retry must reuse an assembled entry rather than create a second entry for the date.
- Keep all operational dates and times in UTC.

## Engineering conventions

- Prefer Python's standard library and straightforward, inspectable code.
- Keep the public site static HTML/CSS with no JavaScript, CMS, database, or application framework.
- Use tidyverse conventions in R. Generated analysis must explicitly load its libraries.
- Treat `entries/YYYY-MM-DD/` as the tracked source for published pages and `website/` as the deployable output.
- Treat `data/snapshots/`, `.work/`, `provenance/`, and `logs/` as private operational material.
- Run `python3 -m unittest discover -s tests -v` after changes. Also run `python3 generate-post.py --validate-only` when the site contains at least one generated entry.

The production URL is `https://jasonjones.ninja/automated-ipseology/`. Deployment is exclusively the orchestrator's rsync step; analyst sessions must never run Git, SSH, rsync, package installation, or infrastructure commands.
