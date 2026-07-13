# Orchestrating the MCP + Skills analysis pipeline

How a natural-language **conservation question** becomes a **traceable, reproducible
output** (markdown report, figures, DOCX brief, NotebookLM artifacts) by chaining
**MCP data access** with **analysis Skills**.

This document is the playbook. It is written so it can later be lifted almost
verbatim into a runnable orchestrator **Skill** (see [§8](#8-promoting-this-to-a-skill)).

---

## 1. Principles

- **Skills decide the science, MCP moves the data.** A Skill fixes method, params,
  seed, and the reference; MCP servers only supply data on the minimal contract.
  Orchestration never invents method choices the Skill did not declare.
- **Traceable end to end.** Every number in an output links back to the Skill that
  produced it, the MCP query that fed it, and the reference case that validated it.
  ChatMPA is "not a black box — a transparent ecosystem."
- **Validated before presented.** No result reaches an output until it has passed
  (or explicitly SKIPPED, with disclosure) the 3 checks. A `PENDING` reference is
  never silently treated as verified.
- **Layered execution.** The same stages run **interactively** (a human in Claude
  Code drives the tool calls) or as an **automated agent chain** (a lead
  orchestrator dispatches one subagent per stage). Stage boundaries are the
  hand-off points either way.
- **CONAPESCA safety is a hard gate.** Sensitive economic-unit records never enter
  an output that could re-identify a unit, and never enter version control.

---

## 2. The pipeline at a glance

```
[0] Intake        parse the conservation question, pin scope (place, time, metric)
      │
[1] Select        pick the Skill(s) blind, from trigger-oriented descriptions
      │
[2] Acquire       pull data via MCP onto the Skill's minimal data contract
      │
[3] Compute       run the Skill (fixed method + seed) → result + method metadata
      │
[4] Validate      3 checks: self-consistency · reference · coherence
      │
[5] Render        markdown report · figures/plots · DOCX brief · NotebookLM
      │
[6] Assemble      stitch provenance + citations, deliver, log the run
```

Each stage has a **contract**: what it receives, what it must produce, and the
gate it must pass before the next stage runs.

---

## 3. Stages in detail

### [0] Intake — frame the question
- Extract the **decision** being asked ("is protection working at Cabo Pulmo?"),
  not just keywords. Pin the **scope**: place/region, time window, metric, unit.
- If scope is ambiguous in a way that changes the analysis (which year? which
  region?), ask **once**, up front — do not guess a load-bearing parameter.
- **Output of stage:** a scoped question object `{decision, place, time, metric}`.

### [1] Select — pick the Skill(s) blind
- Match the scoped question against each Skill's **trigger-oriented `description`**
  (the whole point of that discipline: selection happens by reading, not by
  knowing the internal name).
- One question may fan out to **several Skills** (e.g. biomass + NRSI + temporal
  trend) plus an **output Skill**. Record which Skills were chosen and why.
- Prefer a `per-database/` Skill for a single source; an `interdatabase/` Skill
  when the answer combines sources.
- **Gate:** if no Skill's description fires, stop and say so — do not improvise an
  analysis outside a contract.
- **Output of stage:** an ordered plan of Skill invocations.

### [2] Acquire — data onto the minimal contract
- Read each Skill's `SKILL.md` **Data contract** section: the columns it consumes
  (`lat, lon, time, value`, + any declared extras like `TrophicLevelF`), the
  missing-data rule, and the aggregation unit.
- Pull that data via the appropriate **MCP server** (dev: local staging copy in
  `data/`; prod: MCP). Shape it to the contract — the Skill reads an **interface,
  not a named file**, so the same call works in both environments.
- Apply the Skill's **missing-data rule** here, explicitly (exclude / impute /
  flag). Do not let NAs flow into compute silently.
- **CONAPESCA gate:** strip or aggregate any economic-unit identifier before it
  leaves this stage. If the requested output would expose individual records,
  stop and escalate.
- **Output of stage:** a contract-shaped table per Skill, plus the exact MCP
  query used (kept for provenance).

### [3] Compute — run the Skill
- Invoke the Skill's entry function (`run_skill(data, ...)`), which returns
  `list(value=, method=, params=)`. **Set the declared seed** before any
  stochastic step.
- Do not override documented defaults unless the question explicitly demands a
  declared, in-contract parameter change.
- **Output of stage:** the result object(s), carrying method metadata.

### [4] Validate — the 3 checks
Run `shared/validation/run_checks.R` on each Skill's output:
- **self-consistency** — N runs on the same data agree within tolerance (catches a
  leaky seed).
- **reference** — output matches the Skill's **human-verified** reference within
  tolerance. `status: PENDING` → **SKIP**, and the SKIP is **disclosed** in the
  output ("reference value pending verification"), never hidden.
- **coherence** — the output's declared `method`/`params` match `SKILL.md`.
- **Gate:** a **FAIL** stops the pipeline for that Skill. A **SKIP** may proceed
  **only** with explicit disclosure. Never present a SKIPPED number as verified.
- **Output of stage:** a per-Skill PASS/FAIL/SKIP verdict, attached to the result.

### [5] Render — generate the outputs
Fan the validated results into the requested output types. Each is its own
contract (see [§4](#4-output-contracts)). Figures are produced from the same
validated result objects — never re-computed independently.

### [6] Assemble — provenance and delivery
- Attach to every reported number: **Skill name + version**, the **MCP query**,
  and the **validation verdict**. This is the citation trail.
- Assemble the final artifact(s), deliver, and **log the run** (question, Skills,
  queries, verdicts, outputs) so the analysis is reproducible.

---

## 4. Output contracts

All outputs draw from the **same validated result objects** — they differ in
presentation, not in numbers. No output may introduce a figure or claim that did
not pass stage [4].

| Output | What it is | Produced by | Notes |
|--------|-----------|-------------|-------|
| **Markdown report** | Structured, cited answer to the question | orchestrator + Skill outputs | Default output. Every claim carries its provenance line. |
| **Figures / plots** | Charts from the validated results (R) | the Skill / a shared plotting helper | Same result object as the report; captions state Skill + query. |
| **DOCX / policy brief** | chatMPA-branded brief | pattern of the `marine-prosperity-brief` Skill | For decision-makers; wraps the markdown + figures + map. |
| **NotebookLM artifacts** | Infographic, audio overview, mind map, slides | pattern of the `marine-prosperity-publish` Skill | Publishes an existing brief; requires auth + Studio polling. |

Rendering order is typically **markdown + figures first** (the canonical result),
then **DOCX** (wraps them), then **NotebookLM** (publishes the DOCX/markdown).

---

## 5. Layered execution — interactive vs. agent chain

The stages are the same; only *who* runs them changes.

**Interactive (now).** A human in Claude Code drives it. The model reads this doc,
selects Skills, makes the MCP calls, runs the checks, and renders — pausing for the
human at the one intake question and at any FAIL/SKIP disclosure.

**Agent chain (later).** A **lead orchestrator** owns stages [0], [1], [6] and
dispatches a subagent per heavy stage, passing the stage contract as the hand-off:

| Stage | Subagent role | Receives → Returns |
|-------|---------------|--------------------|
| [2] Acquire | data-fetcher | scoped question + contract → contract-shaped table + query log |
| [3] Compute | skill-runner | table + Skill → result object |
| [4] Validate | validator | result + Skill dir → PASS/FAIL/SKIP verdict |
| [5] Render | renderer | validated results → markdown / figures / DOCX / NotebookLM |

Because each stage already has a defined input/output contract here, promoting from
interactive to agent-chain is wiring, not redesign.

---

## 6. Guardrails (do-not)

- **Do not** present a number whose reference check is `PENDING`/`SKIP` as if it
  were verified. Disclose it.
- **Do not** run an analysis for which no Skill's description fires. Skills are the
  admission criterion; ad-hoc analysis breaks traceability.
- **Do not** let CONAPESCA economic-unit records into any output or commit.
- **Do not** change a Skill's method or default params to make an answer "look
  better" — that is exactly the degree of freedom Skills exist to remove.
- **Do not** re-compute a figure independently of the validated result it depicts.
- **Do not** read a named local file inside a Skill — always the minimal contract.

---

## 7. Worked example (end to end, illustrative — no computed values)

**Question:** "Has protection actually helped the reefs at Cabo Pulmo?"

- **[0] Intake** → `{decision: MPA effectiveness, place: Cabo Pulmo, time: full series, metric: trophic health + biomass}`.
- **[1] Select** → `ltem-nrsi-index` (trophic health fires on "helped the reefs"),
  plus a biomass Skill and a temporal-trend Skill; output = markdown + DOCX.
- **[2] Acquire** → pull LTEM observations for Cabo Pulmo onto each Skill's
  contract via the `ltem` MCP; record the queries.
- **[3] Compute** → run each Skill with its fixed method + seed; get result objects.
- **[4] Validate** → NRSI's Cabo Pulmo reference is currently `PENDING` → the NRSI
  number is reported **with a "reference pending verification" flag**; the other
  Skills PASS.
- **[5] Render** → markdown report with NRSI traffic-light + biomass trend figures;
  then a chatMPA DOCX brief wrapping them.
- **[6] Assemble** → each figure/number carries Skill name+version, MCP query, and
  verdict; log the run.

---

## 8. Promoting this to a Skill

When stable, this playbook becomes an **orchestrator Skill** (likely
`interdatabase/analysis-orchestrator/`) with almost no rewrite:

- **`description` (trigger-oriented):** "Answer a marine-conservation decision
  end to end — pick the right analysis, pull the data, validate it, and produce a
  cited report/brief. Fires on any question that asks for an assessment, comparison,
  trend, or 'is X working' about a reef, MPA, species, or fishery."
- **Method:** stages [0]–[6] above, fixed and ordered (no degrees of freedom in the
  *orchestration* either).
- **Random controls:** none of its own; delegates seeds to the Skills it runs.
- **Reference & tolerance:** a fixed question → a fixed set of Skills selected +
  output shape (validates that selection is deterministic), `status: PENDING`.
- **Do-not rules:** [§6](#6-guardrails-do-not) verbatim.
- **Success criteria:** a delivered output where every reported number carries its
  Skill+version, MCP query, and validation verdict, with any SKIP disclosed.
