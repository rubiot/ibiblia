# iBiblia Context

This context describes the core language for verse association in iBiblia. It exists so architecture discussions use the same words as the project instead of drifting into generic terms.

## Language

**Verse**:
A source or translated Bible verse represented in the project as a selectable text with syntagms.
_Avoid_: line, segment, record

**Verse Pair**:
Two verses that are linked for association work, one source and one translated.
_Avoid_: row, tuple, payload

**Syntagm**:
A selectable text unit inside a verse, such as a word or punctuation token.
_Avoid_: token, fragment, node

**Association**:
A persisted link between selected syntagms in one verse and selected syntagms in its Verse Pair.
_Avoid_: match, mapping, annotation

**Syntagm Selection**:
The set of syntagms chosen in a verse for an association step.
_Avoid_: search result, temporary list

## Relationships

- A **Verse Pair** contains exactly two **Verse** instances
- A **Verse** contains zero or more **Syntagm** instances
- A **Syntagm Selection** belongs to one **Verse** at a time
- An **Association** links one or more selected **Syntagm** instances in a **Verse** to one or more selected **Syntagm** instances in its **Verse Pair**

## Example dialogue

> **Dev:** "When `JoinConnected` is enabled, does the **Syntagm Selection** include the punctuation between two words?"
> **Domain expert:** "Yes. The **Syntagm Selection** should include connected punctuation when it is part of the selected phrase."

> **Dev:** "If a **Verse** already has a current **Syntagm Selection**, should `FindAndSelectSyntagms` preserve it when I search for a new phrase?"
> **Domain expert:** "No. `FindAndSelectSyntagms` should ignore the current **Syntagm Selection** and select only the syntagms resolved from the requested phrase."

## Flagged ambiguities

- "selection" was being used both for matched words only and for the full **Syntagm Selection** including connected punctuation. Resolved: with `JoinConnected`, connected punctuation is part of the **Syntagm Selection**.
- "available for association" was being conflated with current selection state. Resolved: a **Syntagm** becomes unavailable only when it already participates in an **Association**; being in the current **Syntagm Selection** does not make it unavailable by itself.
- "exact sequence" was ambiguous about spaces and punctuation between words. Resolved: sequential resolution for a requested phrase ignores intermediate spaces and punctuation, and only falls back to any-order when no sequential word resolution succeeds.
- `FindAndSelectSyntagms` was ambiguous about whether it augments or replaces the current **Syntagm Selection**. Resolved: each call replaces the **Syntagm Selection** with the syntagms resolved from `Words`.
- `JoinConnected` was ambiguous about when punctuation is added. Resolved: when `JoinConnected=True`, punctuation is added only when it is the sole separator between already selected words; when `JoinConnected=False`, punctuation remains outside the **Syntagm Selection**.
- `Partial=True` was ambiguous about ordering. Resolved: partial resolution preserves the relative order of the requested phrase instead of returning an arbitrary subset order.