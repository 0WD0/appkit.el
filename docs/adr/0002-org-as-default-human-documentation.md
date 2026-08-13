# Use Org for human-facing documentation

**Status:** accepted

Appkit and its sibling clients are written and read inside Emacs. Human-facing package docs therefore use Org (`README.org`, `docs/*.org`) so the same files open as native manuals. Glossaries (`CONTEXT.md`), ADRs, agent guides, and changelogs stay Markdown because those files are consumed by tools and conventions that already expect that format. GitHub still renders `README.org`; do not leave a parallel `README.md`, or it will hide the Org landing page.
