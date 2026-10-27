# Preparing an iBiblia release

This repository produces a portable Windows 10/11 x64 release. Version 1.1.0 is prepared from the committed `master` changes following `v1.0.0.68b`; unrelated branches, experimental project formats and private working projects are excluded.

## Build prerequisites

Use the release toolchain: **Lazarus 4.8 with Free Pascal 3.2.2**, targeting Windows x64. Install the Lazarus packages **KControlsLaz**, **lazrichview** and **richmemopackage**, alongside the standard **LCL**, **FCL** and **SynEdit** packages referenced by the application/test projects. The project-local FPC OpenSSL wrappers under `vendor/fpc-openssl/` must remain on the unit search path.

The packaging workstation also needs **Git**, **PowerShell 5.1 or PowerShell 7**, and **Python** for the credential-free local TLS rejection probe. Locked dependency acquisition requires network access; the app regression suite and local TLS fixture do not require provider credentials or paid AI calls. Check `release/dependencies.lock.json` and the build script for the exact runtime versions and archive layout.
## Source and documentation

1. Work in a separate release worktree. Preserve unrelated modifications in the original checkout and in the wiki clone.
2. Set the application file/product version consistently to `1.1.0.0`. Check the About dialog and Windows executable metadata. Compile with explicit Windows x64/Release options; do not rely on the last IDE target.
3. Review `README.md`, `CHANGELOG.md`, `docs/releases/1.1.0.md`, `docs/README-portable.md` and the public wiki against the intended source. English release notes describe user-facing changes. English and Portuguese manuals retain their established wiki URLs.
4. Review dependencies and third-party notices. `release/dependencies.lock.json` is authoritative for runtime download URLs, hashes and archive-member mappings. Do not replace a pinned dependency without reviewing its source, license and compatibility.
5. Commit the intended source and documentation before packaging. The build script requires a clean committed HEAD, so the package records the source that was actually built.

The 1.1.0 notes intentionally reference versioned wiki screenshot URLs. Capture those images from the final extracted ZIP and publish them with the wiki; this avoids changing the main source commit merely to add post-build screenshots.

## Build the portable package

Run from the clean release worktree with the configured Lazarus installation:

```powershell
.\scripts\Build-Release.ps1 -Version 1.1.0 -LazarusDir 'C:\Apps\lazarus' -OutputDirectory dist
```

The release script stages the exact Git source, resolves and verifies locked runtime dependencies, performs a full optimized Windows x64 Release rebuild without rewriting the project file, and runs the FPCUnit suite plus TLS checks. The TLS checks use no provider key or LLM request: a trusted HTTPS request must succeed, and a self-signed endpoint must be rejected.

Expected outputs:

- `dist/iBiblia-1.1.0-windows-x64.zip`
- `dist/SHA256SUMS.txt`
- `dist/iBiblia-1.1.0-windows-x64.manifest.json`

The ZIP has no enclosing top-level directory. Its fixed allowlist includes the application, required runtime libraries, CA bundle, lemma resource, model catalog, language resources, portable instructions, notices/licenses and `release-manifest.json`. The embedded manifest records the source commit, toolchain and packaged-file hashes. See the script and dependency lock for exact names.

The package must not include projects, `iBiblia.ini`, user LLM configuration, credentials, logs, test executables, source/build scratch files or unrelated local resources. Build staging and caches remain under `outputs/`; they are not release assets. A hash manifest traces the artifact; it does not promise byte-identical output across different compilers or environments.

## Verify the extracted ZIP

Extract the produced ZIP into a new temporary writable directory. Verify its SHA-256 and the embedded file manifest, then use this extracted application for smoke checks and screenshots. Do not substitute an executable from an earlier local build.

Check at least:

- Startup without a pre-existing user INI, correct About/file version and English/Portuguese UI.
- New/open/save/Save As using a disposable demo `.bib` project, with no production data or credentials.
- Source/destination Association, visual/Raw verse editing, undo/redo and unaffected-Association preservation.
- Mixed-direction text, titles, rich translator notes and note previews.
- Translation-memory entries, occurrence navigation/previews and custom status filters.
- AI configuration, machine/project settings and example management without calling a paid provider. AI remains labeled experimental.
- Tagged/interlinear/concordance export to disposable files; inspect output, result/warning reporting and the intended existing-reader behavior.
- Preferences, interface language switching and font/icon appearance at the supported display settings.

Record the exact source commit, ZIP hash, build/test results and observed smoke behavior. Distinguish source tests from GUI checks and list any remaining limitations. Do not reuse historical test counts from `RELIABILITY_CHANGES.md` as current evidence.

## Wiki screenshots and links

Preserve the existing local wiki work. Store final captures under `images/1.1.0/en/` and `images/1.1.0/pt/`, using:

```text
main-window.png
new-project.png
visual-editor.png
translator-note.png
translation-memory.png
ai-configuration.png
ai-examples.png
status-filters.png
project-export.png
concordance.png
```

Use the corresponding interface language for each set. Capture only the disposable demo project; key fields must be empty or masked and no private project paths/text should be exposed. Retain older images rather than silently replacing historical captures.

The illustrated release notes use the English `main-window`, `visual-editor`, `translation-memory` and `ai-configuration` captures through URLs beginning with:

```text
https://raw.githubusercontent.com/wiki/rubiot/ibiblia/images/1.1.0/en/
```

Check every relative wiki page/image link and the four remote release-note images after wiki publication. Preserve the established `User-manual-(English)`, `Manual-de-utilização-(Português)` and `Video-tutorials` URLs. Installation/update guides explain writable-folder installation, complete-runtime extraction, project backups, INI/model-catalog preservation, credential portability and rollback to a preserved older project copy.

## Prepare the draft release

Only perform remote writes within the user's explicit authorization. Publishing a final release and keeping a draft are separate actions.

1. Review and commit the final wiki content/images, then publish the approved wiki changes so screenshot URLs resolve.
2. Verify that the intended main source commit has the completed local build/tests and packaged-app evidence. Push/tag only as authorized; the release tag must resolve to the source commit recorded in the manifest.
3. Create the requested **draft** GitHub release for `v1.1.0`, using `docs/releases/1.1.0.md` as its body. Attach the ZIP, SHA-256 file and external manifest from the same build.
4. Recheck the draft body/images, attached filenames/hashes and tag/source identity. Keep it a draft until final publication is explicitly authorized.

If source changes after packaging, rebuild and revalidate the affected checks, replace all three assets together and update the evidence. Never attach an old ZIP to a newer source tag.
