# Ipseity Daily canonical data

This document describes the authorized analysis-ready dataset used by Ipseological Analyses. It is based on the current Ipseity Daily documentation, canonical validation code, public export generator, and a schema-only inspection performed on 2026-11-17 UTC. If the supplied file disagrees with this document, stop and report the discrepancy; do not invent an interpretation.

## Collection and row meaning

Ipseity Daily surveys samples of American adults and asks questions of the form, “Does *signifier* describe you today?” A signifier is a word or phrase people may use in self-description, such as `dad`, `American`, or `trusting`.

The canonical file is gzip-compressed UTF-8 CSV. One row is one completed Yes/No response about one signifier on one observation date. Its unique key is:

`hashed_respondent_id`, `observation_date`, `signifier`

`endorsed` is `1` for Yes and `0` for No. A skipped answer is absent. Signifiers are assigned in sets, so the absence of a respondent-signifier row generally does **not** mean No; it can mean that person was not asked about that signifier. Analyses of a signifier must therefore use the rows for that signifier as the denominator.

Responses require a completed survey and consent to continue. If any respondent/date/signifier key is duplicated in the upstream response data, the canonical pipeline excludes that entire respondent-day. Demographics are joined by respondent and observation date. Current collection retrieves the Prolific study identified in the corresponding Qualtrics data.

The project describes each day as a new random sample of American adults, but respondent hashes demonstrate that some people participate on multiple dates. The data do not establish a simple random sample of all U.S. adults, and the analyst should not treat observations as nationally representative or mutually independent without justification.

## Exact 16-column schema

| Column | Meaning |
| --- | --- |
| `endorsed` | Numeric binary answer: `1` Yes, `0` No. |
| `signifier` | Identity signifier that was asked and answered. |
| `observation_date` | Qualtrics survey start date in `YYYY-MM-DD` form. |
| `hashed_respondent_id` | First 12 hexadecimal characters of a SHA-256 participant hash. Use only for grouping/counting; never publish values. |
| `demographics_status` | `available`, `consent_revoked`, or `data_expired`. |
| `age` | Numeric age in years when available. |
| `sex` | Prolific sex field when available. Do not assume only two possible values. |
| `ethnicity` | Prolific ethnicity field when available. |
| `birth_country` | Prolific country of birth field when available. |
| `residence_country` | Prolific country of residence field when available. |
| `nationality` | Prolific nationality field when available. |
| `language` | Prolific language field when available. |
| `student` | Prolific student-status field when available. |
| `employment` | Prolific employment-status field when available. |
| `time_on_task` | Prolific-export time-taken field, retained as supplied and possibly missing. |
| `approvals` | Prolific-export total-approvals field, retained as supplied. |

For `consent_revoked`, all demographic values are missing. `data_expired` means at least one demographic field expired; only expired fields are missing, while other valid demographic values are retained. Overall response analyses retain valid Yes/No rows even when demographics are unavailable. A demographic comparison should filter only on the demographic variables it actually needs and report the resulting denominator.

## Dependence and denominators

There are three commonly useful units:

- **Response observation:** one signifier answer. Suitable for a signifier's raw Yes proportion when its denominator is stated, but answers from the same person are dependent.
- **Respondent-day:** one respondent on one date. Suitable for questions about a day's surveyed participants or summaries across multiple signifiers asked that day.
- **Unique respondent:** one hash across all dates. Suitable for person-level summaries, while recognizing that repeat participants have varying numbers and timing of observations.

Do not sum distinct-respondent counts across signifiers, dates, or groups: the same person may occur in several. For repeated observations, choose a scientifically relevant aggregation or use respondent-clustered uncertainty. Time analyses should distinguish variation in who was sampled from within-person change; these data alone do not automatically identify either one.

## Coverage and known limitations

The snapshot inspected on 2026-11-17 contained 793,108 response rows, 5,464 unique respondent hashes, 10,155 respondent-days, 707 signifiers, and 482 observation dates from 2025-07-08 through 2026-11-16. These counts describe that snapshot only; each daily invocation receives authoritative current counts from Python.

Months or dates with no rows are absent, not zero-prevalence periods. The newest period may be incomplete. A documented collection outage spans 2026-08-31 through 2026-09-03. Six other dates were absent from the historical response checkpoint as of the upstream audit: 2025-12-06, 2026-01-05, 2026-03-15, 2026-04-26, 2026-08-22, and 2026-08-23; their causes were not established.

Signifier assignment may vary by day or survey set, so naive comparisons among signifiers or across time can reflect different respondent composition. Demographic values are self-reported Prolific fields and may be missing. `hashed_respondent_id` is a stable pseudonymous grouping key, not proof that every record corresponds to a distinct natural person. The public data do not include survey weights, question-order metadata in the canonical table, or a documented population sampling frame adequate for population-causal claims.

The data are released by Ipseity Daily under CC BY 4.0. Only the supplied canonical snapshot is authorized for the daily analysis; do not combine it with external data.
