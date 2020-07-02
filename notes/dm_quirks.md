### DQ001

`world << a + b` is fine, `world << a & b` is not, due to precedence


### DQ002

Updating a list by numeric index will wipe out any values associated with the previous value.

```dm
var/list/l = list("a", "a")
l["a"] = "foo"
// "a" is now associated with "foo"
l[1] = "a"
// "a" is no longer associated with anything
```
