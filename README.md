# appkit.el

`appkit.el` contains protocol-independent runtime pieces shared by `disco.el`
and `emacs-qq`:

- app sessions, buffer views, and owned timers/hooks/processes
- explicit generated-content, property-only, and editable transactions
- coalesced invalidation followed by one view-owned synchronization callback
- stable-key EWOC reconciliation and semantic position restoration
- a persistent chat composer and projected chat timeline

Application state and rendering stay in the client packages.  Appkit owns the
lifecycle and update mechanics; it does not define Discord, QQ, channel, or
message models.

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

No consumer compatibility aliases are provided.  Shared chat code uses the
`appkit-chatbuf-*` and `appkit-chat-timeline-*` namespaces directly.

## Update model

An application starts an `appkit-app`, attaches one `appkit-view` to each live
buffer, and gives the view a synchronization function.  External events update
the client's canonical state, add opaque entry/resource/part invalidations,
and schedule synchronization.  The synchronization function is the only event
path that projects state back into generated buffer content.

Generated structural edits are undo-free; the editable tail composer keeps
normal Emacs undo behavior.
