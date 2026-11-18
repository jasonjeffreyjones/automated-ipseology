from __future__ import annotations

from datetime import date, timedelta
import json
from pathlib import Path
import random
import tempfile
import unittest

from support import load_script


choose = load_script("choose-script.py", "choose_script_for_tests")


class ChooseScriptTests(unittest.TestCase):
    def test_loads_nonblank_lines_and_distinguishes_duplicates(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "prompts.txt"
            path.write_text("Question A\n\n Question B \nQuestion A\n", encoding="utf-8")
            prompts = choose.load_prompts(path)
        self.assertEqual([p.text for p in prompts], ["Question A", "Question B", "Question A"])
        self.assertNotEqual(prompts[0].prompt_id, prompts[2].prompt_id)

    def test_selects_only_prompts_unused_in_previous_30_days(self) -> None:
        prompts = [
            choose.Prompt("a", "A", 1),
            choose.Prompt("b", "B", 2),
            choose.Prompt("c", "C", 3),
        ]
        today = date(2026, 11, 17)
        successes = [
            {"prompt_id": "a", "date": (today - timedelta(days=1)).isoformat(), "status": "published"},
            {"prompt_id": "b", "date": (today - timedelta(days=30)).isoformat(), "status": "published"},
            {"prompt_id": "c", "date": (today - timedelta(days=31)).isoformat(), "status": "published"},
        ]
        selected, reason = choose.choose_prompt(
            prompts, successes, today, random.Random(10)
        )
        self.assertEqual(selected.prompt_id, "c")
        self.assertIn("previous 30 days", reason)

    def test_falls_back_to_least_recently_published_ties(self) -> None:
        prompts = [
            choose.Prompt("a", "A", 1),
            choose.Prompt("b", "B", 2),
            choose.Prompt("c", "C", 3),
        ]
        today = date(2026, 11, 17)
        successes = [
            {"prompt_id": "a", "date": "2026-11-10", "status": "published"},
            {"prompt_id": "b", "date": "2026-11-10", "status": "published"},
            {"prompt_id": "c", "date": "2026-11-12", "status": "published"},
        ]
        selected, reason = choose.choose_prompt(
            prompts, successes, today, random.Random(3)
        )
        self.assertIn(selected.prompt_id, {"a", "b"})
        self.assertEqual(reason, "least recently published")

    def test_history_accepts_only_successful_publications(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "history.jsonl"
            records = [
                {"date": "2026-11-15", "prompt_id": "a", "status": "failed"},
                {"date": "2026-11-16", "prompt_id": "b", "status": "published"},
            ]
            path.write_text(
                "\n".join(json.dumps(record) for record in records) + "\n",
                encoding="utf-8",
            )
            loaded = choose.load_successes(path)
        self.assertEqual([record["prompt_id"] for record in loaded], ["b"])


if __name__ == "__main__":
    unittest.main()
