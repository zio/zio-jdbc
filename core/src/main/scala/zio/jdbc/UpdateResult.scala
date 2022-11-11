package zio.jdbc

import zio.Chunk

final case class UpdateResult(rowsUpdated: Long, updatedKeys: Chunk[Long])
