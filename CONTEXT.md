# Appkit

Appkit provides protocol-neutral runtime and presentation primitives for stateful Emacs clients.

## Language

**Compose surface**:
A standalone editable surface for preparing one outbound social item, including its context, status, attachments, and one or more ordered editable parts. Generated chrome is overlay presentation; buffer text is only the editable bodies and fixed part dividers.
_Avoid_: Post form, toot form, note form

**Compose part**:
One editable body on a compose surface, with optional generated title and attachments.
_Avoid_: Tweet, note, thread item

**Status field**:
A client-supplied label, value, and action describing one current compose setting without assigning that setting a cross-protocol meaning.
_Avoid_: Universal option, protocol field

**Media attachment view**:
The shared presentation of one attached media item, including its preview, label, description, state, and available actions.
_Avoid_: Media resource (which is a readable or displayable resource)

**Compose submit**:
An in-flight outbound attempt on a compose surface, with optional 0-1 progress and an optional client cancel hook.
_Avoid_: File upload, HTTP request, job

**Client semantics**:
The protocol-specific meaning, validation, capability, persistence, transport, and error behavior supplied by a consuming package.
_Avoid_: Appkit policy