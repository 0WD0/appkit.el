# Keep shared compose mechanics in Appkit and protocol semantics in clients

**Status:** accepted

Chirp's X Web composer and the planned `misskey.el` client, initially targeting Sharkey's Misskey-compatible API, need the same standalone compose mechanics while exposing different publishing semantics. Sharkey's current `notes/create` contract includes visibility, specified recipients, content warnings, local-only notes, reaction acceptance, reply and renote targets, channels, up to sixteen drive files, polls, and a separate scheduled-note endpoint; its web form also persists drafts, previews notes, edits media descriptions, and schedules posts. Chirp has the corresponding need for reply/quote context, media descriptions, reply controls, and later polls or scheduling.

Appkit will own a protocol-neutral standalone compose surface: the editable-body boundary, generated read-only context and status presentation, status-field actions, media-attachment presentation, point and modification invariants, and reusable completion/media/UI primitives. The planned surface is `appkit-compose`, not an X, Mastodon, or Misskey model and not a forced specialization of `appkit-chatbuf`, which is designed around persistent chat/timeline input.

Clients will own the opaque draft/context objects, field labels and values, protocol-specific validation and capabilities, attachment metadata, reply or renote semantics, visibility/content-warning meaning, poll and schedule rules, persistence, transport, cancellation, and remote error or unknown-outcome behavior. Appkit will not define universal `Post`, `Note`, `Toot`, `Audience`, `Sensitive`, `Poll`, or `Reply` models. In particular, Misskey visibility is not the same concept as X reply audience, and Misskey `cw`/file sensitivity is not the same wire contract as X `possibly_sensitive`.

The Appkit layer must not acquire a Transient dependency. Clients may use Transient, `completing-read`, or another client-owned editor behind the shared field/action callbacks. Existing `appkit-ui`, `appkit-chat-completion`, and `appkit-media-*` APIs are the first reuse points; a new attachment or field abstraction is justified only by the concrete Chirp and `misskey.el` consumers.

## Consequences

- Chirp's next compose UI work should avoid a Chirp-only status-strip or attachment-row abstraction and should be migratable to `appkit-compose`.
- `misskey.el` becomes the second consumer that validates the shared surface against Sharkey's richer note, poll, draft, and schedule workflow.
- Protocol adapters remain explicit, so shared presentation does not erase differences such as `replyId` versus X reply metadata, `renoteId` versus quote posts, or `visibleUserIds` versus X reply controls.
- The first implementation may keep client-local state while the shared contract is exercised; Appkit code should be added when the first common vertical slice is migrated, not as a speculative universal publishing framework.

## Rejected alternatives

- **Keep complete compose UIs in each client:** rejected because the standalone editor, status strip, attachment rows, and generated-content invariants would be duplicated.
- **Put a universal social-post model in Appkit:** rejected because Sharkey/Misskey and X use different concepts and wire contracts for visibility, sensitivity, reply, quote/renote, polls, and scheduling.
- **Force post composers into `appkit-chatbuf`:** rejected because a standalone post editor does not have chatbuf's trailing timeline composer and command-boundary lifecycle.