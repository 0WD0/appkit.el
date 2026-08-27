# Separate chat compose geometry from generic editing authority

**Status:** accepted

Chirp and prospective social clients need a shared multi-item editor: committed
items are generated timeline rows, while a trailing chat composer edits one
current item.  Other Appkit clients, including mail clients, may instead own a
structured document and a dedicated major mode.  Treating either geometry as a
universal Compose model makes protocol and document ownership leak into Appkit.

Appkit therefore provides two distinct layers:

1. `appkit-chat-compose` owns the concrete chatbuf-based multi-item geometry,
   ordered item sequence, row/input transitions, and generated status/media
   presentation used by social clients.
2. `appkit-compose` owns only protocol-neutral editing authority: a semantic
   generation, immutable capture, one view-local effect owner, stale-callback
   fencing, optional progress, and cancellation requests.

Clients own their document model, draft or workspace persistence, autosave
policy, validation, close policy, attachments and identities, transport,
reconciliation, and all accepted/rejected/unknown semantics.  Finishing an
Appkit effect owner merely retires view-local authority; it never records a
protocol outcome.

The Appkit layer does not acquire a Transient dependency.  Clients may use
Transient, `completing-read`, a dedicated major mode, or the concrete
`appkit-chat-compose` surface.  Shared status and media presentation remain
available without requiring a shared Draft model.

## Consequences

- Chirp uses `appkit-chat-compose` for its X post/thread editor and owns remote
  draft, scheduled-post, and unknown-write state itself.
- A mail client can use a dedicated structured editor while reusing Appkit
  generation/capture and view-local effect fencing.
- Generated chrome uses `appkit-compose-without-tracking`; semantic changes
  outside text call `appkit-compose-touch` exactly once.
- A new generic field, attachment, autosave, or outcome abstraction is added
  only after multiple concrete clients demonstrate the same ownership rule.

## Rejected alternatives

- **Keep the old chat composer named `appkit-compose`:** rejected because its
  item/timeline geometry is not a universal document model.
- **Put a universal Draft lifecycle in Appkit:** rejected because persistence,
  remote publication, reconciliation, and close semantics differ by protocol.
- **Duplicate all edit/effect fencing in every client:** rejected because
  generation capture and stale view-local operation ownership are genuinely
  protocol-neutral.
