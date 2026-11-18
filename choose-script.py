#!/usr/bin/env python3
"""Choose an Ipseological Analyses prompt without requiring a database."""

from __future__ import annotations

import argparse
import hashlib
import json
import random
import secrets
import sys
from dataclasses import dataclass
from datetime import date, datetime, timedelta, timezone
from pathlib import Path
from typing import Iterable, Sequence


@dataclass(frozen=True)
class Prompt:
    prompt_id: str
    text: str
    line_number: int


def load_prompts(path: Path) -> list[Prompt]:
    """Load nonblank lines, giving duplicate text stable occurrence identifiers."""
    prompts: list[Prompt] = []
    occurrences: dict[str, int] = {}
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except FileNotFoundError as exc:
        raise ValueError(f"Prompt file does not exist: {path}") from exc

    for line_number, raw in enumerate(lines, start=1):
        text = raw.strip()
        if not text:
            continue
        occurrence = occurrences.get(text, 0) + 1
        occurrences[text] = occurrence
        digest = hashlib.sha256(f"{text}\0{occurrence}".encode()).hexdigest()[:16]
        prompts.append(Prompt(digest, text, line_number))

    if not prompts:
        raise ValueError(f"Prompt file has no nonblank prompts: {path}")
    return prompts


def load_successes(path: Path) -> list[dict[str, object]]:
    """Read published history records; tolerate a missing or empty history file."""
    if not path.exists():
        return []
    records: list[dict[str, object]] = []
    for line_number, raw in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        if not raw.strip():
            continue
        try:
            record = json.loads(raw)
        except json.JSONDecodeError as exc:
            raise ValueError(
                f"Invalid JSON in {path} on line {line_number}: {exc.msg}"
            ) from exc
        if record.get("status") != "published":
            continue
        try:
            date.fromisoformat(str(record["date"]))
            str(record["prompt_id"])
        except (KeyError, ValueError) as exc:
            raise ValueError(
                f"Invalid published history record in {path} on line {line_number}"
            ) from exc
        records.append(record)
    return records


def choose_prompt(
    prompts: Sequence[Prompt],
    successes: Iterable[dict[str, object]],
    publication_date: date,
    rng: random.Random | secrets.SystemRandom | None = None,
) -> tuple[Prompt, str]:
    """Choose uniformly from prompts unused for 30 days, or least recently used."""
    chooser = rng or secrets.SystemRandom()
    last_published: dict[str, date] = {}
    for record in successes:
        prompt_id = str(record["prompt_id"])
        published = date.fromisoformat(str(record["date"]))
        if published >= publication_date:
            # Future records and same-day records do not describe the previous 30 days.
            continue
        previous = last_published.get(prompt_id)
        if previous is None or published > previous:
            last_published[prompt_id] = published

    cutoff = publication_date - timedelta(days=30)
    eligible = [
        prompt
        for prompt in prompts
        if prompt.prompt_id not in last_published
        or last_published[prompt.prompt_id] < cutoff
    ]
    if eligible:
        return chooser.choice(eligible), "not published during previous 30 days"

    oldest = min(last_published[prompt.prompt_id] for prompt in prompts)
    least_recent = [
        prompt for prompt in prompts if last_published[prompt.prompt_id] == oldest
    ]
    return chooser.choice(least_recent), "least recently published"


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--prompts", type=Path, default=Path("ipseity-prompts.txt"))
    parser.add_argument(
        "--history", type=Path, default=Path("state/prompt-history.jsonl")
    )
    parser.add_argument(
        "--date",
        type=date.fromisoformat,
        default=datetime.now(timezone.utc).date(),
    )
    parser.add_argument("--seed", type=int, help="Deterministic seed for tests only")
    parser.add_argument("--json", action="store_true", help="Emit a JSON object")
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(argv)
    try:
        prompts = load_prompts(args.prompts)
        successes = load_successes(args.history)
        rng = random.Random(args.seed) if args.seed is not None else None
        selected, reason = choose_prompt(prompts, successes, args.date, rng)
    except ValueError as exc:
        print(f"choose-script.py: {exc}", file=sys.stderr)
        return 2

    result = {
        "prompt_id": selected.prompt_id,
        "prompt": selected.text,
        "line_number": selected.line_number,
        "selection_reason": reason,
        "publication_date": args.date.isoformat(),
    }
    if args.json:
        print(json.dumps(result, ensure_ascii=False, sort_keys=True))
    else:
        print(selected.text)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
