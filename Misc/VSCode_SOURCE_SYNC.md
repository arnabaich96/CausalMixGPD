# VS Code Source Sync (HTML + PDF)

## HTML: click rendered element -> jump to source line
1. Run task `html-source-sync:index` to annotate `docs/index.html`.
2. Run task `serve-docs-http`.
3. Start debug profile `HTML Preview (docs/index)`.
4. In the browser page, hold `Alt` and click an element.

This opens the matching line in `docs/index.html` using `vscode://file/...`.

## PDF: SyncTeX (Overleaf-like forward/inverse sync)
1. Open a vignette source file (for example `vignettes/introduction.Rmd`).
2. Run task `render-active-rmd-pdf-synctex`.
3. Open the generated PDF next to the source file.
   Usually this is created in the same folder as the `.Rmd`.
4. Use LaTeX Workshop commands:
   - `LaTeX Workshop: SyncTeX from cursor` (source -> PDF)
   - `Ctrl+Alt+J` in PDF (PDF -> source), if keybinding is enabled.

Notes:
- SyncTeX targets the generated `.tex` source from your `.Rmd` render.
- If inverse search does not trigger, ensure extension `James-Yu.latex-workshop` is installed and re-open the PDF.
