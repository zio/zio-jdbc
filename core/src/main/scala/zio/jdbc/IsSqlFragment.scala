/*
 * Copyright 2022 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package zio.jdbc

import scala.annotation.implicitNotFound

@implicitNotFound(
  "This method can only be invoked on a fragment of SQL, but the type parameter ${A} " +
    "indicates that you have mapped the result set to a concrete data type. " +
    "If you want to invoke this method, do not map result of the SQL to some other data " +
    "type until after you have finished fully forming the SQL statement."
)
abstract class IsSqlFragment[-A]
object IsSqlFragment {
  implicit val resultSetIsSqlFragment: IsSqlFragment[ZResultSet] = new IsSqlFragment[ZResultSet] {}
}
