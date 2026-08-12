# Appkit

Appkit provides protocol-neutral runtime and presentation primitives for stateful Emacs clients.

## Language

**Compose surface**:
A standalone editable surface for preparing one outbound social item, including its context, body, status, and attachments.
_Avoid_: Post form, toot form, note form

**Status field**:
A client-supplied label, value, and action describing one current compose setting without assigning that setting a cross-protocol meaning.
_Avoid_: Universal option, protocol field

**Media attachment view**:
The shared presentation of one attached media item, including its preview, label, description, state, and available actions.
_Avoid_: Media resource (which is a readable or displayable resource)

**Client semantics**:
The protocol-specific meaning, validation, capability, persistence, transport, and error behavior supplied by a consuming package.
_Avoid_: Appkit policy