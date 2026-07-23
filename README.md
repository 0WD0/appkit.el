# appkit.el

`appkit.el` contains protocol-independent runtime pieces shared by `disco.el`,
`emacs-qq`, and `emacs-zulip`:

- app sessions, buffer views, and owned timers/hooks/processes
- owner-scoped keyed FIFO task queues with bounded concurrency
- explicit generated-content, property-only, and editable transactions
- coalesced invalidation followed by one view-owned synchronization callback
- stable-key EWOC reconciliation and independent semantic point/viewport
  restoration for every live window showing a view
- a persistent chat composer, shared rich completion substrate, projected
  Unicode emoji completion, continuous history-window state, and keyed chat
  timeline projection
- reusable prefix, one-line/list-view, mode-line, two-line avatar,
  threaded-discussion, sectioned-directory geometry, and deterministic
  identity-keyed name coloring
- optional Evil integration with state-specific directory bindings and a
  deferred binding helper for client packages, without a runtime dependency
- browser-free media resources, inline image/video previews, and media actions

Application state, protocol adaptation, and client branding stay in the
client packages.  Appkit owns lifecycle/update mechanics and reusable
presentation geometry, including an optional neutral name-color palette;
clients still choose identity keys and self/system overrides.  Appkit does not
define Discord, QQ, channel, or message models.

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
`appkit-task-queue-*`, `appkit-chatbuf-*`, `appkit-chat-history-*`,
`appkit-chat-timeline-*`, `appkit-ui-*`, `appkit-view-*`,
`appkit-chat-completion-*`, `appkit-chat-ins-*`, `appkit-discussion-*`,
`appkit-directory-*`, `appkit-name-color-*`, `appkit-evil-*`, and
`appkit-media-*` namespaces directly.

The shared task queue deduplicates equal keys across active and waiting work,
starts tasks in FIFO order up to a live adjustable limit, and guards every
completion with a one-shot token.  A starter may return an `appkit-handle` or
a zero-argument cancellation function; clients should wrap processes or
backend-specific cancellation objects in a function.  Killing the owning app
or view closes the queue and makes stale completions inert.  Batch removal via
`appkit-task-queue-cancel-keys` retires all matching work before it frees a
slot, so pruning cannot accidentally start another task that is also stale.
Synchronous completion is delivered only after the starter returns its handle,
and starter, finisher, or cancellation errors/quits release their tokens and
slots before they are re-signaled.  Lifecycle cleanup also completes its
remaining siblings across arbitrary nonlocal exits, while the normal bulk path
is iterative and safe for thousands of owned handles.

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
the client's canonical state and call `appkit-request-sync` with opaque
entry/resource/part invalidations.  The synchronization function is the only
event path that projects state back into generated buffer content.

Client callbacks separate logical request settlement from presentation.  A
request generation owns loading flags and tokens even if its original view was
replaced, so its callback must retire that ownership without projecting into a
stale buffer.  Projection is gated separately by the exact live view, and any
deferred user action is tied to the same generation so cancellation or a newer
operation makes it inert.

Each attached buffer also retains a protocol-neutral fingerprint made from the
application kind, stable application id, and view id.  Detaching a view leaves
that fingerprint behind, so a later application session can recover a renamed
buffer and run replacement setup instead of creating a duplicate.  Changing
major mode clears the fingerprint, and Appkit never steals a buffer from a
different live view.  Reopening a still-live view skips replacement setup and
therefore preserves drafts, history windows, and request ownership.
Fingerprint matches are unique: conflicting live owners or ambiguous detached
buffers are rejected, and stale registry views are reaped with their owned
handles before a new attachment is created.  Distinct fingerprints may request
the same fallback display name; Appkit gives the later buffer a normal unique
name instead of treating a title collision as an identity collision.

Generated structural edits are undo-free; the editable tail composer keeps
normal Emacs undo behavior.

Position snapshots belong to Appkit rather than to a client redraw loop.  For
each live window showing the buffer, Appkit preserves `window-point` and
`window-start` by stable entry key plus an offset within the entry, with an
absolute line/column fallback.  Key promotion (for example an optimistic local
message becoming a server message) is applied to every captured window
independently.  A point immediately after a keyed run follows that run's new
end, including a final newline at `point-max`; adjacent intervals carrying
`equal` opaque string keys are treated as one run.

Mode-line cache helpers only update cached presentation state.  Network and
runtime hooks rely on Emacs's normal redisplay cycle; only interactive provider
installation/removal explicitly requests an immediate mode-line refresh.
