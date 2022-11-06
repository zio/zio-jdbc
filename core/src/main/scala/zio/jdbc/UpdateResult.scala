package zio.jdbc

final case class UpdateResult(rowsUpdated: Long, updatedKeys: ZResultSet)
