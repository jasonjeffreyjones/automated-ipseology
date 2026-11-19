from __future__ import annotations

import csv
from datetime import date, timedelta
from email.utils import parsedate_to_datetime
import gzip
import json
import os
from pathlib import Path
import shutil
import struct
import tempfile
import unittest
from unittest import mock
import xml.etree.ElementTree as ET

from support import PROJECT_ROOT, load_script


generate = load_script("generate-post.py", "generate_post_for_tests")


def tiny_png(width: int = 1200, height: int = 750) -> bytes:
    return (
        b"\x89PNG\r\n\x1a\n"
        + struct.pack(">I", 13)
        + b"IHDR"
        + struct.pack(">II", width, height)
    )


def canonical_row(**changes: str) -> dict[str, str]:
    row = {column: "" for column in generate.EXPECTED_COLUMNS}
    row.update(
        {
            "endorsed": "1",
            "signifier": "curious",
            "observation_date": "2026-11-16",
            "hashed_respondent_id": "012345abcdef",
            "demographics_status": "available",
            "age": "34",
            "sex": "Female",
            "residence_country": "United States",
            "approvals": "1200",
        }
    )
    row.update(changes)
    return row


def write_dataset(path: Path, rows: list[dict[str, str]]) -> None:
    with gzip.open(path, "wt", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=generate.EXPECTED_COLUMNS)
        writer.writeheader()
        writer.writerows(rows)


class DatasetValidationTests(unittest.TestCase):
    def test_validates_schema_keys_and_aggregate_counts(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "data.csv.gz"
            write_dataset(
                path,
                [
                    canonical_row(),
                    canonical_row(
                        endorsed="0",
                        signifier="patient",
                        hashed_respondent_id="fedcba543210",
                    ),
                ],
            )
            result = generate.validate_dataset(path, date(2026, 11, 17), 3)
        self.assertEqual(result["rows"], 2)
        self.assertEqual(result["respondents"], 2)
        self.assertEqual(result["signifiers"], 2)
        self.assertEqual(result["latest_observation_date"], "2026-11-16")

    def test_rejects_duplicate_primary_key(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "data.csv.gz"
            write_dataset(path, [canonical_row(), canonical_row()])
            with self.assertRaisesRegex(generate.WorkflowError, "Duplicate primary key"):
                generate.validate_dataset(path, date(2026, 11, 17), 3)

    def test_rejects_stale_dataset(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "data.csv.gz"
            write_dataset(path, [canonical_row(observation_date="2026-10-01")])
            with self.assertRaisesRegex(generate.WorkflowError, "stale"):
                generate.validate_dataset(path, date(2026, 11, 17), 3)

    def test_acquisition_retains_only_latest_snapshot(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            temporary_path = Path(temporary)
            root = temporary_path / "project"
            (root / "data/snapshots").mkdir(parents=True)
            (root / "data/manifests").mkdir(parents=True)
            old_snapshot = root / "data/snapshots" / (
                "2026-11-16-" + ("a" * 64) + ".csv.gz"
            )
            old_snapshot.write_bytes(b"old snapshot")
            source = temporary_path / "source.csv.gz"
            write_dataset(source, [canonical_row()])
            config = generate.Config(
                local_data=source,
                data_url="",
                data_source="local",
                max_data_age_days=3,
                codex_model="test-model",
                codex_timeout_seconds=10,
                r_timeout_seconds=10,
                base_url="https://example.com/",
                git_push=False,
                git_remote="origin",
                git_branch="main",
                deploy_target="user@example:/safe/site/",
                rsync_args=("-az",),
            )
            old_root = generate.ROOT
            generate.ROOT = root
            try:
                latest, manifest = generate.acquire_dataset(
                    config, date(2026, 11, 17)
                )
                snapshots = list((root / "data/snapshots").glob("*.csv.gz"))
                self.assertEqual(snapshots, [latest])
                self.assertFalse(old_snapshot.exists())
                self.assertEqual(manifest["local_snapshot_retention"], "latest-only")

                retried, _ = generate.acquire_dataset(config, date(2026, 11, 17))
                self.assertEqual(retried, latest)
                self.assertEqual(
                    list((root / "data/snapshots").glob("*.csv.gz")), [latest]
                )
            finally:
                generate.ROOT = old_root

    def test_failed_acquisition_does_not_remove_existing_snapshot(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "project"
            (root / "data/snapshots").mkdir(parents=True)
            (root / "data/manifests").mkdir(parents=True)
            existing = root / "data/snapshots" / (
                "2026-11-16-" + ("a" * 64) + ".csv.gz"
            )
            existing.write_bytes(b"existing snapshot")
            invalid_source = Path(temporary) / "invalid.csv"
            invalid_source.write_text("not,the,canonical,schema\n", encoding="utf-8")
            config = generate.Config(
                local_data=invalid_source,
                data_url="",
                data_source="local",
                max_data_age_days=3,
                codex_model="test-model",
                codex_timeout_seconds=10,
                r_timeout_seconds=10,
                base_url="https://example.com/",
                git_push=False,
                git_remote="origin",
                git_branch="main",
                deploy_target="user@example:/safe/site/",
                rsync_args=("-az",),
            )
            old_root = generate.ROOT
            generate.ROOT = root
            try:
                with self.assertRaisesRegex(generate.WorkflowError, "No valid dataset source"):
                    generate.acquire_dataset(config, date(2026, 11, 17))
                self.assertTrue(existing.is_file())
            finally:
                generate.ROOT = old_root

    def test_failed_workdir_cleanup_removes_only_dataset_copy(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            data_dir = root / ".work/failed-run/data"
            data_dir.mkdir(parents=True)
            dataset = data_dir / "ipseity.csv.gz"
            dataset.write_bytes(b"large copied dataset")
            task = root / ".work/failed-run/TASK.md"
            task.write_text("diagnostic context\n", encoding="utf-8")
            old_root = generate.ROOT
            generate.ROOT = root
            try:
                self.assertEqual(generate.purge_working_dataset_copies(), 1)
                self.assertFalse(dataset.exists())
                self.assertTrue(task.is_file())
            finally:
                generate.ROOT = old_root


class SafeRenderingTests(unittest.TestCase):
    def test_codex_invocation_uses_verified_minimal_settings(self) -> None:
        config = generate.Config(
            local_data=Path("unused"),
            data_url="",
            data_source="local",
            max_data_age_days=3,
            codex_model="test-model",
            codex_timeout_seconds=10,
            r_timeout_seconds=10,
            base_url="https://example.com/",
            git_push=False,
            git_remote="origin",
            git_branch="main",
            deploy_target="user@example:/safe/site/",
            rsync_args=("-az",),
        )
        command = generate.codex_exec_command(
            config, Path("/tmp/ipseology-test-work"), "Do the analysis."
        )
        self.assertEqual(command[:4], ["codex", "-a", "never", "exec"])
        self.assertIn("workspace-write", command)
        self.assertIn("sandbox_workspace_write.network_access=false", command)
        self.assertIn("test-model", command)
        self.assertEqual(command[-1], "Do the analysis.")
        for removed in (
            "--ignore-user-config",
            "--ignore-rules",
            "--ephemeral",
            'shell_environment_policy.inherit="none"',
        ):
            self.assertNotIn(removed, command)

    def test_codex_environment_keeps_runtime_and_removes_publishing_credentials(self) -> None:
        with mock.patch.dict(
            os.environ,
            {
                "XDG_RUNTIME_DIR": "/run/user/1000",
                "SSH_AUTH_SOCK": "/run/user/1000/agent",
                "GITHUB_TOKEN": "publish-secret",
                "ZENODO_API_TOKEN": "archive-secret",
                "DEPLOY_TARGET": "example:/site",
            },
            clear=False,
        ):
            environment = generate.filtered_codex_environment()
        self.assertEqual(environment["XDG_RUNTIME_DIR"], "/run/user/1000")
        self.assertNotIn("SSH_AUTH_SOCK", environment)
        self.assertNotIn("GITHUB_TOKEN", environment)
        self.assertNotIn("ZENODO_API_TOKEN", environment)
        self.assertNotIn("DEPLOY_TARGET", environment)

    def test_sandbox_failure_is_reported_from_successful_codex_jsonl(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            log = Path(temporary) / "codex.jsonl"
            log.write_text(
                json.dumps(
                    {
                        "type": "item.completed",
                        "item": {
                            "type": "agent_message",
                            "text": (
                                "The shell sandbox failed to start with bwrap: loopback: "
                                "Failed RTM_NEWADDR: Operation not permitted."
                            ),
                        },
                    }
                )
                + "\n",
                encoding="utf-8",
            )
            with self.assertRaisesRegex(
                generate.WorkflowError, "Codex sandbox initialization failed.*RTM_NEWADDR"
            ):
                generate.raise_codex_sandbox_failure(log)

    def test_markdown_escapes_html_and_drops_unsafe_link(self) -> None:
        rendered = generate.render_markdown(
            "## Result\n\n<script>alert(1)</script> [bad](javascript:alert(1)) "
            "and [good](https://example.com/*literal*)."
        )
        self.assertNotIn("<script>", rendered)
        self.assertIn("&lt;script&gt;", rendered)
        self.assertNotIn("javascript:", rendered)
        self.assertIn('href="https://example.com/*literal*"', rendered)
        self.assertNotIn('href="https://example.com/<em>', rendered)

    def test_png_dimensions_requires_png_header(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "figure.png"
            path.write_bytes(tiny_png(1000, 600))
            self.assertEqual(generate.png_dimensions(path), (1000, 600))

    def test_analysis_code_blocks_process_calls(self) -> None:
        unsafe = (
            'library(tidyverse)\npath <- Sys.getenv("IPSEITY_DATA_PATH")\n'
            'system("whoami")\nggsave("visualization.png")\nwriteLines("x", "results.txt")\n'
        )
        with self.assertRaisesRegex(generate.WorkflowError, "prohibited"):
            generate.validate_analysis_code(unsafe)

    def test_clean_r_rerun_uses_standard_codex_sandbox(self) -> None:
        workdir = Path("/tmp/ipseology-test-work")
        command = generate.r_sandbox_command(workdir)
        self.assertEqual(command[:4], ["codex", "sandbox", "--permission-profile", ":workspace"])
        self.assertIn("/usr/bin/Rscript", command)
        self.assertNotIn("--dangerously-bypass-approvals-and-sandbox", command)


class SiteRenderingTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        (self.root / "templates").mkdir()
        (self.root / "website").mkdir()
        (self.root / "entries").mkdir()
        shutil.copy2(PROJECT_ROOT / "templates/entry.html", self.root / "templates/entry.html")
        shutil.copy2(PROJECT_ROOT / "templates/about.html", self.root / "templates/about.html")
        shutil.copy2(PROJECT_ROOT / "website/style.css", self.root / "website/style.css")
        self.old_root = generate.ROOT
        generate.ROOT = self.root
        self.config = generate.Config(
            local_data=Path("unused"),
            data_url="",
            data_source="local",
            max_data_age_days=3,
            codex_model="test-model",
            codex_timeout_seconds=10,
            r_timeout_seconds=10,
            base_url="https://example.com/automated-ipseology/",
            git_push=False,
            git_remote="origin",
            git_branch="main",
            deploy_target="user@example:/safe/site/",
            rsync_args=("-az",),
        )

    def tearDown(self) -> None:
        generate.ROOT = self.old_root
        self.temporary.cleanup()

    def add_entry(self, day: str, title: str, article: str | None = None) -> None:
        directory = self.root / "entries" / day
        directory.mkdir()
        (directory / "analysis.R").write_text("library(tidyverse)\n", encoding="utf-8")
        (directory / "article.md").write_text(
            article or "This is a safe article paragraph with **one finding**.\n",
            encoding="utf-8",
        )
        (directory / "visualization.png").write_bytes(tiny_png())
        (directory / "results.txt").write_text("N = 100\n", encoding="utf-8")
        (directory / "prompt.txt").write_text("What changed?\n", encoding="utf-8")
        metadata = {
            "title": title,
            "figure_alt": "A chart displaying a test result across two clearly labeled groups.",
            "date": day,
            "prompt": "What changed?",
            "prompt_id": day,
            "model": "test-model",
            "dataset_sha256": "a" * 64,
            "generated_at_utc": f"{day}T12:34:56+00:00",
        }
        (directory / "metadata.json").write_text(
            json.dumps(metadata), encoding="utf-8"
        )

    def test_latest_entry_becomes_index_and_navigation_is_complete(self) -> None:
        self.add_entry("2026-11-16", "Earlier result")
        self.add_entry("2026-11-17", "Latest result")
        generate.render_site(self.config)
        generate.validate_site()

        index = (self.root / "website/index.html").read_text(encoding="utf-8")
        earlier = (self.root / "website/2026-11-16/index.html").read_text(encoding="utf-8")
        latest = (self.root / "website/2026-11-17/index.html").read_text(encoding="utf-8")
        self.assertIn("Latest result", index)
        self.assertIn('href="2026-11-16/">First</a>', index)
        self.assertIn('href="../2026-11-17/">Next</a>', earlier)
        self.assertIn('aria-disabled="true">Next</span>', latest)
        self.assertTrue((self.root / "website/2026-11-17/analysis.R").is_file())
        self.assertTrue((self.root / "website/feed.xml").is_file())
        self.assertTrue((self.root / "website/sitemap.xml").is_file())

    def test_social_metadata_rss_sitemap_and_json_ld_are_stable(self) -> None:
        self.add_entry("2026-11-16", "Earlier result")
        title = 'A "quoted" result & a <careful> comparison'
        article = (
            "The first result compares A & B while calling the pattern **careful**. "
            "It is a real article-specific summary with enough context for metadata.\n\n"
            "## Method\n\nThe rest of the article explains the method.\n"
        )
        self.add_entry("2026-11-17", title, article)
        generate.render_site(self.config)
        generate.validate_site()

        tracked = {
            relative: (self.root / "website" / relative).read_bytes()
            for relative in (
                "index.html",
                "2026-11-16/index.html",
                "2026-11-17/index.html",
                "feed.xml",
                "sitemap.xml",
            )
        }
        mtimes = {
            relative: (self.root / "website" / relative).stat().st_mtime_ns
            for relative in tracked
        }
        generate.render_site(self.config)
        self.assertEqual(
            tracked,
            {
                relative: (self.root / "website" / relative).read_bytes()
                for relative in tracked
            },
        )
        self.assertEqual(
            mtimes,
            {
                relative: (self.root / "website" / relative).stat().st_mtime_ns
                for relative in tracked
            },
        )

        feed = ET.parse(self.root / "website/feed.xml").getroot()
        channel = feed.find("channel")
        self.assertIsNotNone(channel)
        assert channel is not None
        self.assertEqual(channel.findtext("title"), "Ipseological Analyses")
        self.assertEqual(channel.findtext("language"), "en-US")
        items = channel.findall("item")
        self.assertEqual([item.findtext("title") for item in items], [title, "Earlier result"])
        self.assertEqual(
            items[0].findtext("link"),
            "https://example.com/automated-ipseology/2026-11-17/",
        )
        self.assertEqual(items[0].findtext("guid"), items[0].findtext("link"))
        self.assertEqual(
            parsedate_to_datetime(items[0].findtext("pubDate") or "").isoformat(),
            "2026-11-17T12:34:56+00:00",
        )
        self.assertIn("A & B", items[0].findtext("description") or "")

        namespace = {"sm": generate.SITEMAP_NAMESPACE}
        sitemap = ET.parse(self.root / "website/sitemap.xml").getroot()
        locations = [
            item.findtext("sm:loc", namespaces=namespace)
            for item in sitemap.findall("sm:url", namespace)
        ]
        self.assertEqual(
            locations,
            [
                "https://example.com/automated-ipseology/",
                "https://example.com/automated-ipseology/2026-11-16/",
                "https://example.com/automated-ipseology/2026-11-17/",
            ],
        )

        homepage_parser = generate.SiteHTMLParser()
        homepage_parser.feed(
            (self.root / "website/index.html").read_text(encoding="utf-8")
        )
        self.assertEqual(homepage_parser.meta_properties["og:type"], ["website"])
        self.assertEqual(homepage_parser.meta_properties["og:title"], ["Ipseological Analyses"])
        self.assertEqual(homepage_parser.scripts, [])

        article_source = (self.root / "website/2026-11-17/index.html").read_text(
            encoding="utf-8"
        )
        self.assertIn(r"\u003ccareful\u003e", article_source)
        self.assertNotIn("<careful>", article_source)
        article_parser = generate.SiteHTMLParser()
        article_parser.feed(article_source)
        self.assertEqual(article_parser.meta_properties["og:title"], [title])
        self.assertEqual(article_parser.meta_properties["og:type"], ["article"])
        self.assertEqual(
            article_parser.meta_properties["og:image"],
            ["https://example.com/automated-ipseology/2026-11-17/visualization.png"],
        )
        structured = json.loads(article_parser.scripts[0]["content"])
        self.assertEqual(structured["@type"], "BlogPosting")
        self.assertEqual(structured["headline"], title)
        self.assertEqual(structured["datePublished"], "2026-11-17T12:34:56Z")
        self.assertEqual(structured["dateModified"], "2026-11-17T12:34:56Z")
        self.assertEqual(
            structured["author"]["name"],
            "Ipseological Analyses autonomous AI analyst",
        )
        self.assertIn("test-model", structured["author"]["description"])
        self.assertEqual(structured["publisher"]["name"], "Ipseological Analyses")

    def test_feed_is_limited_to_30_entries_while_sitemap_keeps_all(self) -> None:
        first = date(2026, 1, 1)
        for offset in range(31):
            day = (first + timedelta(days=offset)).isoformat()
            self.add_entry(day, f"Result {offset:02d}")
        generate.render_site(self.config)
        generate.validate_site()

        feed = ET.parse(self.root / "website/feed.xml").getroot()
        items = feed.findall("channel/item")
        self.assertEqual(len(items), 30)
        self.assertEqual(items[0].findtext("title"), "Result 30")
        self.assertEqual(items[-1].findtext("title"), "Result 01")
        self.assertNotIn("Result 00", [item.findtext("title") for item in items])

        namespace = {"sm": generate.SITEMAP_NAMESPACE}
        sitemap = ET.parse(self.root / "website/sitemap.xml").getroot()
        self.assertEqual(len(sitemap.findall("sm:url", namespace)), 32)

    def test_site_validation_rejects_broken_feed_metadata(self) -> None:
        self.add_entry("2026-11-17", "A valid result")
        generate.render_site(self.config)
        feed = self.root / "website/feed.xml"
        feed.write_text(
            feed.read_text(encoding="utf-8").replace("<language>en-US</language>", ""),
            encoding="utf-8",
        )
        with self.assertRaisesRegex(generate.WorkflowError, "channel language"):
            generate.validate_site()


class MockedDailyWorkflowTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        for relative in (
            "templates",
            "website",
            "entries",
            "data/manifests",
            "data/snapshots",
            "state/run-state",
            "logs",
            "provenance",
            ".work",
        ):
            (self.root / relative).mkdir(parents=True, exist_ok=True)
        shutil.copy2(PROJECT_ROOT / "templates/entry.html", self.root / "templates/entry.html")
        shutil.copy2(PROJECT_ROOT / "templates/about.html", self.root / "templates/about.html")
        shutil.copy2(PROJECT_ROOT / "website/style.css", self.root / "website/style.css")
        (self.root / "ANALYST.md").write_text("Analyst instructions\n", encoding="utf-8")
        (self.root / "DATA.md").write_text("Data instructions\n", encoding="utf-8")
        (self.root / "ipseity-prompts.txt").write_text("A test question?\n", encoding="utf-8")
        (self.root / "state/prompt-history.jsonl").write_text("", encoding="utf-8")
        self.old_root = generate.ROOT
        generate.ROOT = self.root
        self.run_date = generate.utc_today()
        self.config = generate.Config(
            local_data=Path("unused"),
            data_url="",
            data_source="local",
            max_data_age_days=3,
            codex_model="mock-codex-model",
            codex_timeout_seconds=10,
            r_timeout_seconds=10,
            base_url="https://example.com/automated-ipseology/",
            git_push=True,
            git_remote="origin",
            git_branch="main",
            deploy_target="user@example:/safe/site/",
            rsync_args=("-az",),
        )

    def tearDown(self) -> None:
        import logging

        logging.shutdown()
        logging.getLogger().handlers.clear()
        generate.ROOT = self.old_root
        self.temporary.cleanup()

    def fake_invoke(self, *_args, **_kwargs):
        workdir = self.root / ".work" / "mock-analyst"
        workdir.mkdir()
        (workdir / "data").mkdir()
        (workdir / "data/ipseity.csv.gz").write_bytes(b"mock snapshot")
        analysis = (
            "library(tidyverse)\n"
            'data_path <- Sys.getenv("IPSEITY_DATA_PATH")\n'
            'data <- read_csv(data_path, show_col_types = FALSE)\n'
            'writeLines("N = 100 response observations; prevalence = 50 percent.", "results.txt")\n'
            'ggsave("visualization.png", ggplot(data, aes(endorsed)) + geom_bar())\n'
            + ("# Reproducible explanatory comment.\n" * 12)
        )
        (workdir / "analysis.R").write_text(analysis, encoding="utf-8")
        article = "## Finding\n\n" + " ".join(["evidence"] * 600) + "\n"
        (workdir / "article.md").write_text(article, encoding="utf-8")
        (workdir / "visualization.png").write_bytes(tiny_png())
        (workdir / "results.txt").write_text(
            "N = 100 response observations; prevalence = 50 percent.\n",
            encoding="utf-8",
        )
        (workdir / "metadata.json").write_text(
            json.dumps(
                {
                    "title": "A mocked but complete daily analysis",
                    "figure_alt": "A bar chart showing the mocked response count for each endorsement value.",
                }
            ),
            encoding="utf-8",
        )
        json_log = self.root / "logs" / "mock-codex.jsonl"
        json_log.write_text(
            '{"type":"command.completed","command":"Rscript analysis.R"}\n',
            encoding="utf-8",
        )
        return workdir, json_log

    def fake_rerun(self, _config, _date, workdir, _snapshot):
        verify = self.root / ".work" / "mock-verify"
        verify.mkdir()
        (verify / "visualization.png").write_bytes(tiny_png())
        shutil.copy2(workdir / "results.txt", verify / "results.txt")
        return verify

    def test_local_daily_run_assembles_valid_entry_without_external_actions(self) -> None:
        snapshot = self.root / "data/snapshots/mock.csv.gz"
        snapshot.write_bytes(b"mock snapshot")
        manifest = {
            "sha256": "b" * 64,
            "local_snapshot_at_acquisition": "data/snapshots/mock.csv.gz",
            "local_snapshot_retention": "latest-only",
            "rows": 100,
            "latest_observation_date": self.run_date.isoformat(),
        }
        selection = {
            "prompt_id": "mock-prompt",
            "prompt": "A test question?",
            "line_number": 1,
            "selection_reason": "test",
        }
        with (
            mock.patch.object(generate.Config, "from_environment", return_value=self.config),
            mock.patch.object(generate, "preflight"),
            mock.patch.object(generate, "acquire_dataset", return_value=(snapshot, manifest)),
            mock.patch.object(generate, "choose_research_prompt", return_value=selection),
            mock.patch.object(generate, "invoke_codex", side_effect=self.fake_invoke),
            mock.patch.object(generate, "rerun_analysis", side_effect=self.fake_rerun),
            mock.patch.object(generate, "commit_and_push") as git_mock,
            mock.patch.object(generate, "deploy_site") as deploy_mock,
        ):
            result = generate.main(["--date", self.run_date.isoformat(), "--no-publish"])

        self.assertEqual(result, 0)
        git_mock.assert_not_called()
        deploy_mock.assert_not_called()
        entry = self.root / "entries" / self.run_date.isoformat()
        page = self.root / "website" / self.run_date.isoformat() / "index.html"
        self.assertTrue((entry / "metadata.json").is_file())
        self.assertTrue(page.is_file())
        self.assertIn("mock-codex-model", page.read_text(encoding="utf-8"))
        state = json.loads(
            (self.root / "state/run-state" / f"{self.run_date.isoformat()}.json").read_text(
                encoding="utf-8"
            )
        )
        self.assertEqual(state["status"], "local-only")
        self.assertEqual(
            (self.root / "state/prompt-history.jsonl").read_text(encoding="utf-8"), ""
        )


if __name__ == "__main__":
    unittest.main()
