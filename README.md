# Ipseological Analyses

Ipseological Analyses is an unattended scientific publishing workflow. Once per UTC day it takes the latest canonical [Ipseity Daily](https://jasonjones.ninja/social-science-dashboard-inator/ipseity-daily/) microdata, asks Codex to conduct one reproducible R analysis, verifies the work independently, builds a static article, commits and pushes it, and deploys the site with rsync over SSH.

Production URL: <https://jasonjones.ninja/automated-ipseology/>

## Responsibility split

`generate-post.py` owns every deterministic or privileged operation: locking, idempotency, dataset acquisition and validation, snapshots and hashes, prompt selection, Codex isolation and timeout, output validation, a clean R rerun, safe Markdown rendering, navigation, site validation, Git, rsync, and outcome records.

The analyst working directory is populated only with the canonical snapshot plus `ANALYST.md`, `DATA.md`, and the selected research question. Codex creates `analysis.R`, `visualization.png`, `article.md`, `metadata.json`, and `results.txt`. It is not deliberately given publishing credentials or permission to deploy. The analyst runs with the standard Codex CLI `workspace-write` sandbox, restricted tool network, and a `never` approval policy. The independent clean R rerun is also launched through the standard Codex CLI `:workspace` permission profile. Neither path has an unrestricted fallback.

The site itself is plain HTML and CSS. It uses no JavaScript, database, CMS, or web framework.

Each site render also rebuilds `website/feed.xml` from the newest 30 dated entries and
`website/sitemap.xml` from the complete entry archive. The same renderer adds canonical,
Open Graph, RSS-autodiscovery, and Schema.org `BlogPosting` metadata to the generated
HTML. Entry metadata remains the single source of publication and modification dates, so
retries reproduce stable URLs, GUIDs, timestamps, and structured data without a separate
scheduled process.

## Repository layout

| Path | Purpose | Git policy |
| --- | --- | --- |
| `entries/YYYY-MM-DD/` | Public-safe source artifacts for each entry | Tracked |
| `website/` | Complete rsync-ready static site, RSS feed, and XML sitemap | Tracked |
| `data/manifests/` | Dataset hash, coverage, counts, and source reference | Tracked |
| `data/snapshots/` | The one latest validated canonical CSV snapshot | Private/ignored |
| `state/prompt-history.jsonl` | Successful publication history used by prompt selection | Tracked on the following publication |
| `state/run-state/` | Resumable operational stage and outcome | Private/ignored |
| `provenance/` | Original analyst outputs and invocation record | Private/ignored |
| `logs/` | Workflow, Codex JSONL, R, and rsync logs | Private/ignored |
| `.work/` | Temporary analysis/verification directories; failed context may remain, but redundant data copies are removed | Private/ignored |

`website/YYYY-MM-DD/analysis.R` is downloadable from each article. Raw microdata, analyst logs, and `results.txt` are never copied into the website.

## Initial setup

The server already has Python 3.12, R 4.3, tidyverse, Codex CLI, Git, rsync, and `flock`. The Python runtime uses only the standard library; `requirements.txt` is intentionally dependency-free.

1. Populate `ipseity-prompts.txt`. Each nonblank line is one complete research prompt. Do not add comments: a nonblank comment would itself be eligible for selection.
2. Copy `.env.example` to `.env`, set the exact Codex model identifier, Git destination, and production rsync target, then restrict it with `chmod 600 .env`.
3. Authenticate Codex CLI for the service account and verify `codex --version`. Do not put a Codex token in `.env` when the existing CLI login can be used.
4. Configure this repository's `origin` remote and make the initial source commit before enabling cron.
5. Verify SSH host keys and noninteractive SSH authentication for the narrowly scoped deployment account. The target should be the dedicated production directory, never a broad web root.
6. Run the tests and a local publication rehearsal from the service account's ordinary terminal session.

```sh
python3 -m unittest discover -s tests -v
python3 generate-post.py --no-publish
python3 generate-post.py --validate-only
```

The rehearsal invokes Codex and creates today's local entry but deliberately does not push or deploy. Once reviewed, running `python3 generate-post.py` for the same date resumes that already validated entry at the Git/deployment stages; it does not rerun or duplicate the analysis.

### Sandbox verification

The standard Codex `workspace-write` invocation and standalone `codex sandbox` command have been verified directly from an ordinary NEOD terminal. The automated tests mock the model call; before enabling cron, run the complete rehearsal above from the service account's terminal and confirm in the Codex JSONL/R logs that the full analysis and clean rerun succeeded.

The standard `workspace-write` profile is designed primarily around write containment; its exact read scope can vary with Codex CLI version and host policy. This repository scrubs the analyst environment and does not deliberately pass Git, SSH, rsync, or deployment credentials, but filesystem-resident account material may still be readable if the installed profile grants broad reads. Run production under a dedicated least-privilege service account, use a deployment key restricted to the single destination, keep `.env` mode `0600`, verify the effective permission profile after Codex upgrades, and do not enable cron until the ordinary-terminal sandbox check passes.

## Configuration

Settings are read from process environment variables, with `.env` used as a non-overriding local fallback.

| Variable | Default / meaning |
| --- | --- |
| `IPSEITY_LOCAL_DATA` | `/home/ubuntu/ipseity-daily/data/ipseity.csv.gz`; preferred analysis-ready local input |
| `IPSEITY_DATA_URL` | Optional HTTP(S) fallback or forced source |
| `DATA_SOURCE` | `auto`, `local`, or `url`; `auto` tries local and then configured URL |
| `MAX_DATA_AGE_DAYS` | `3`; maximum lag between publication date and newest observation |
| `CODEX_MODEL` | Required exact model identifier, passed to Codex and published authoritatively |
| `CODEX_TIMEOUT_SECONDS` | `1800` (30 minutes) |
| `R_TIMEOUT_SECONDS` | `600` for the independent clean rerun |
| `BASE_URL` | Production URL shown above |
| `GIT_PUSH` | `true`; when false a commit is made but not pushed |
| `GIT_REMOTE` / `GIT_BRANCH` | `origin` / `main` |
| `DEPLOY_TARGET` | Required explicit `user@host:/dedicated/path/` rsync-over-SSH destination |
| `RSYNC_ARGS` | Atomic-friendly archive synchronization flags; see `.env.example` |

The orchestrator starts from the known-working `codex -a never exec --sandbox workspace-write` form. It adds the configured model, analyst working directory, JSON logging, and an explicit `sandbox_workspace_write.network_access=false` override. It preserves the ordinary terminal runtime environment while removing deployment, Git-hosting, SSH-agent, rsync, archive, and cloud-provider credential variables. No unrestricted fallback is implemented.

## Dataset handling

The preferred input is the canonical file produced locally by Ipseity Daily, not `/home/ubuntu/ipseity-daily/Ipseity Daily.csv` (that similarly named file is a wide raw Qualtrics export). The canonical validator requires the exact documented 16-column schema, binary endorsements, allowed demographic statuses, valid dates and pseudonymous IDs, no future observations, and a unique respondent/date/signifier primary key.

Every run copies or downloads the source before reading it, validates the copy, calculates SHA-256, and promotes it to a read-only path named `data/snapshots/YYYY-MM-DD-SHA256.csv.gz`. Only after that promotion succeeds does Python remove superseded local snapshots, so this directory retains exactly one current dataset copy. A public-safe dated manifest permanently records the source reference, hash, coverage, aggregate counts, and latest-only retention policy. URL query parameters and user information are removed from the recorded source. Historical data recovery relies on the separately maintained Ipseity Daily archives plus the recorded SHA-256, rather than on this project's local snapshot directory.

## Prompt selection

`choose-script.py` selects uniformly at random among prompts without a successful publication in the previous 30 days. If all prompts were recently used, it selects uniformly among those with the oldest last-success date. Prompt identity is a content hash plus duplicate occurrence number, so identical nonblank lines still represent distinct prompt slots.

Only an rsync-successful publication appends a `published` JSON Lines record to `state/prompt-history.jsonl`. Failed and local-only runs do not affect 30-day eligibility. The successful record is included in the next day's Git commit because deployment and outcome recording intentionally happen after the current day's push.

For a transparent deterministic selection test only:

```sh
python3 choose-script.py --date 2026-11-17 --seed 42 --json
```

Production never supplies a seed and uses operating-system randomness.

## Failure and retry behavior

The workflow acquires a nonblocking exclusive lock. It will not start a second copy while one is running. An entry source directory is promoted only after Codex outputs and the clean R rerun pass. The website is rendered and every HTML reference is checked before Git or rsync.

`state/run-state/YYYY-MM-DD.json` records the last stage. Important retry cases are automatic:

- Before assembly: nothing is published; repair the reported cause and rerun.
- After local assembly: rerun revalidates and resumes publication without another Codex call.
- After Git push but before successful rsync: rerun can repeat the idempotent commit check and resumes deployment from the existing entry.
- After deployment: prompt history and a `complete` outcome are written; further runs for that date validate and exit without duplicate publication.

Successful runs remove their current analyst and verification directories. Failed work directories remain for diagnosis, but their copied `ipseity.csv.gz` is removed unless `--keep-workdir` was explicitly requested. The authoritative latest local copy remains in `data/snapshots/`.

Rsync uses delayed updates and delayed deletion. Since remote deployment cannot be a cross-system transaction with Git, the tracked run stage is the recovery mechanism. Logs use UTC and retain command stderr without exposing it on the public site.

## Cron

Ipseity Daily currently updates its canonical dataset at 06:15 UTC. Schedule this project later so it sees the completed upstream file. For example, after a successful manual production run:

```cron
15 7 * * * cd /home/ubuntu/automated-ipseology && /usr/bin/python3 generate-post.py >> logs/cron.log 2>&1
```

The same line is provided in `cron.example`. Use the service account's existing crontab. The command itself obtains the exclusive lock, so an overlapping manual run fails safely. Cron, SSH keys, Git credentials, and server configuration are intentionally not installed or changed by this repository.

## Development checks

```sh
python3 -m py_compile generate-post.py choose-script.py
python3 -m unittest discover -s tests -v
python3 generate-post.py --validate-only
```

`--validate-only` is meaningful after at least one entry exists. Tests use temporary fixtures and never invoke Codex, Git push, SSH, rsync, or the production dataset.
