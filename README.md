# appkit.el

`appkit.el` contains protocol-independent runtime pieces shared by `disco.el`
and `emacs-qq`:

- app sessions, buffer views, and owned timers/hooks/processes
- explicit generated-content, property-only, and editable transactions
- coalesced invalidation followed by one view-owned synchronization callback
- stable-key EWOC reconciliation and semantic position restoration
- a persistent chat composer, shared rich completion substrate, projected
  Unicode emoji completion, continuous history-window state, and keyed chat
  timeline projection
- reusable prefix, one-line/list-view, mode-line, and two-line avatar geometry
- browser-free media resources, inline image/video previews, and media actions

Application state, protocol adaptation, and client branding stay in the
client packages.  Appkit owns lifecycle/update mechanics and reusable
presentation geometry; it does not define Discord, QQ, channel, or message
models.

The shared history controller owns exact buffer-local window edges,
unknown/partial/loading and authoritative-empty state, opaque request
ownership, strict slicing, automatic edge gates, and passive delimiter
geometry.  Clients still own transport cancellation, cursor and page
semantics, remote-frontier and read observations, exhaustion/error
interpretation, and decisions about when a protocol response reaches latest.

Media callers adapt backend objects into canonical resource descriptors and
explicit preview metadata.  Appkit owns atomic acquisition, byte-level image
cache finalization, and preview processes.  Clients own logical cache keys,
wire-resource state, and branded faces.

## Development

Eask owns package activation and load paths:

```sh
eask recompile
scripts/eask-ert-brief test/*.el
```

The brief runner recompiles immediately before testing, preserves compiler
diagnostics or ERT failure backtraces and the unexpected-results summary, and
reduces a successful run to its final count.  It forwards its arguments to
`eask test ert` and returns Eask's exit status, so sibling clients can invoke
it from their own package directory as well.

Consumers use a local file dependency while the three repositories are
developed together:

```lisp
(depends-on "appkit" :file "../appkit.el")
```

No consumer compatibility aliases are provided.  Shared code uses the
`appkit-chatbuf-*`, `appkit-chat-history-*`, `appkit-chat-timeline-*`,
`appkit-ui-*`, `appkit-view-*`, `appkit-chat-completion-*`,
`appkit-chat-ins-*`, and `appkit-media-*` namespaces directly.

Chat completion is deliberately provider-neutral: appkit owns token bounds,
candidate metadata, CAPF/picker presentation, atomic commit, and ordered TAB
dispatch.  Clients own member and server-emoji lookup, avatar acquisition,
protocol objects, and insertion callbacks.  A selected mention or sticker
therefore remains a real client-defined structured composer object rather
than appkit inventing a wire format.

On Emacs versions that provide the built-in emoji database,
`appkit-chat-emoji-*` adds shared `:unicode_name:` candidates.  Server emoji,
stickers, and favorite-face lookup remain client-owned providers.

## Update model

An application starts an `appkit-app`, attaches one `appkit-view` to each live
buffer, and gives the view a synchronization function.  External events update
the client's canonical state, add opaque entry/resource/part invalidations,
and schedule synchronization.  The synchronization function is the only event
path that projects state back into generated buffer content.

Generated structural edits are undo-free; the editable tail composer keeps
normal Emacs undo behavior.
