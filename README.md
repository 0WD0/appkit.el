# appkit.el

`appkit.el` contains protocol-independent runtime pieces shared by `disco.el`
and `emacs-qq`:

- app sessions, buffer views, and owned timers/hooks/processes
- explicit generated-content, property-only, and editable transactions
- coalesced invalidation followed by one view-owned synchronization callback
- stable-key EWOC reconciliation and semantic position restoration
- a persistent chat composer, shared rich completion substrate, and projected
  chat timeline
- reusable prefix, one-line/list-view, mode-line, and two-line avatar geometry
- browser-free media resources, inline image/video previews, and media actions

Application state, protocol adaptation, and client branding stay in the
client packages.  Appkit owns lifecycle/update mechanics and reusable
presentation geometry; it does not define Discord, QQ, channel, or message
models.

Media callers adapt backend objects into canonical resource descriptors and
explicit preview metadata.  Appkit owns atomic acquisition, byte-level image
cache finalization, and preview processes.  Clients own logical cache keys,
wire-resource state, and branded faces.

## Development

Eask owns package activation and load paths:

```sh
eask recompile
eask test ert test/*.el
```

Consumers use a local file dependency while the three repositories are
developed together:

```lisp
(depends-on "appkit" :file "../appkit.el")
```

No consumer compatibility aliases are provided.  Shared code uses the
`appkit-chatbuf-*`, `appkit-chat-timeline-*`, `appkit-ui-*`, `appkit-view-*`,
`appkit-chat-completion-*`, `appkit-chat-ins-*`, and `appkit-media-*`
namespaces directly.

Chat completion is deliberately provider-neutral: appkit owns token bounds,
candidate metadata, CAPF/picker presentation, atomic commit, and ordered TAB
dispatch.  Clients own member/emoji lookup, avatar acquisition, protocol
objects, and insertion callbacks.  A selected mention or sticker therefore
remains a real client-defined structured composer object rather than appkit
inventing a wire format.

## Update model

An application starts an `appkit-app`, attaches one `appkit-view` to each live
buffer, and gives the view a synchronization function.  External events update
the client's canonical state, add opaque entry/resource/part invalidations,
and schedule synchronization.  The synchronization function is the only event
path that projects state back into generated buffer content.

Generated structural edits are undo-free; the editable tail composer keeps
normal Emacs undo behavior.
