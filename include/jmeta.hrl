-record(type, {name, mixins, guards, default, mode}).
-record(tmode, {mixins, guards}).

-record(frame, {name, extend, fields, extended_fields}).
-record(field, {name, class, guards, default, optional}).
