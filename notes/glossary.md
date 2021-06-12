Object -> A managed object with the 32-bit header, one of datum, string, list, etc... Not /obj. Lives on the type tree

- Datum -> Mostly fully mutable PathObject with user behaviour. Equivalent to /datum ish

  - Atom -> World state, geometry etc. Synced. /atom, pt datum

- List -> etc etc /list

- String -> strings. /string


-- "types"

"Type": Overarching type, including primitives and PathTypes, and unions of such, functions, etc.

"PathType": Anything that lives on the type tree. Possibly primitives..?
