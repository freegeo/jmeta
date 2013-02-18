-record(type, {name, mixins, guards, default, mode, require}).
-record(tmode, {mixins, guards}).

-record(frame, {name, extend, fields, require}).
-record(field, {name, class, method, guards, default, mode}).
-record(fmode, {optional}).
