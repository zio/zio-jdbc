# Changes
* Fallback to `statement.setObject` (to support types like `UUID`)
  - [ ] add test
* Drop `zio-cofig` and `zio-logging` dependencies
* Interpolation of Sql values
* Use Debug level logging
  - [ ] revisit and remove

TODOs
* [ ] Closing statements and resultsets
