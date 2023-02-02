# Changes
* Fallback to `statement.setObject` (to support types like `UUID`)
  - [ ] add test
* Drop `zio-cofig` and `zio-logging` dependencies
* Interpolation of Sql values
* Use Debug level logging
  - [ ] revisit and remove
* Closing statements and resultsets

TODOs
* [ ] Decide on level where use `ZIO.blocking` as its x6 slower, e.g. `zrs.next()` or `zrs.close()`
* [ ] Encapsulate into (akin Doobie):
   - ??? `Query` with `executeQuery` 
   - `Update` with `executeLargeUpdate` and later to add support for `.manyUpdate` and `.withGeneratedKeys`
   - 
