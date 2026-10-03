# gptel-status

Colored, fixed-width status indicators for [gptel](https://github.com/karthink/gptel), with an optional Doom Modeline segment.

## Setup

Requires Emacs 29.1+ and gptel. Clone this repo, then add to your config:

```elisp
(add-to-list 'load-path "~/path/to/gptel-status")
(require 'gptel-status)
(gptel-status-mode 1)

;; Optional: add the indicator after line/column/location in Doom Modeline.
(require 'gptel-status-doom)
(doom-modeline-add-segment 'gptel-status 'buffer-position :after 'main)
```

If you define your own Doom layouts, include `gptel-status` in their segment lists. Layout selection belongs in your config. In chat-only layouts, omit `process` to avoid duplicate gptel status text.

## Icons

[Nerd Icons](https://github.com/rainstormstudio/nerd-icons.el) provides the modeline glyphs when loaded; otherwise single-cell fallbacks are used. These Unicode equivalents keep the legend readable everywhere:

| Icon | Status |
| --- | --- |
| ◷ Clock | Waiting for the model |
| ⟳ Sync arrows | Receiving/streaming a response, not refreshing |
| ⚒ Tools | Preparing/running tools or receiving tool results |
| ? Question | Awaiting tool confirmation |
| ✓ Check | Ready/completed |
| · Info | Empty or informational response |
| ■ Stop | Aborted |
| ⓧ Circled X | Error |
| + Overflow | More than two child tasks |

The four fixed cells show **parent · child · child · overflow**. Hover for exact phases and child details. Child tracking requires an agent integration; it is not automatically enabled for every agent package.

Exact state tracking currently uses private gptel lifecycle APIs, so compatibility may change with gptel updates. The Doom segment uses public APIs only.

MIT License.
