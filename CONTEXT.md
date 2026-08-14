# Appkit

Appkit provides protocol-neutral runtime and presentation primitives for stateful Emacs clients.

## Language

**Compose surface**:
A standalone chatbuf used to prepare one outbound social item. Committed draft items are generated timeline rows; the trailing composer holds the current uncommitted or in-edit part.
_Avoid_: Post form, toot form, note form

**Compose part**:
One draft item on a compose surface. Committed parts are rendered; the current part is edited in the composer.
_Avoid_: Tweet, note, thread item

**Status field**:
A client-supplied label, value, and action describing one current compose setting without assigning that setting a cross-protocol meaning.
_Avoid_: Universal option, protocol field

**One-line preview**:
A bounded, protocol-neutral content projection used inside compact containers. It combines optional atomic visual content with text and a textual fallback, but excludes the surrounding row, card, action, and resource lifecycle.
_Avoid_: Message row, thumbnail renderer, protocol summary

**Media attachment view**:
The shared presentation of one attached media item, including its preview, label, description, state, and available actions.
_Avoid_: Media resource (which is a readable or displayable resource)

**Transfer control**:
A timeline presentation of one in-flight or available byte transfer, with a direction, a state, optional 0-1 progress, optional byte counts, and a client action.
_Avoid_: File upload, HTTP request, job, media resource

**Compose submit**:
An in-flight outbound attempt on a compose surface, with optional 0-1 progress and an optional client cancel hook.
_Avoid_: File upload, HTTP request, job

**Client semantics**:
The protocol-specific meaning, validation, capability, persistence, transport, and error behavior supplied by a consuming package.
_Avoid_: Appkit policy

**Discussion connector**:
A prefix mark that continues or ends a linear discussion chain without changing nesting depth.
_Avoid_: Indent, tree depth, thread line

**Discussion depth**:
The visual nesting of a tree reply under its parent.
_Avoid_: Ancestor position, chain index

**Action span**:
A buffer region that carries one client-supplied zero-argument action, activated by RET or mouse-1. Help text describes that action, not a client keymap.
_Avoid_: Hardcoded key cheat sheet, protocol target, nested dispatcher