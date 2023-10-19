package zio.jdbc

import zio.Chunk

final case class UpdateResult[+A](rowsUpdated: Long, updatedKeys: Chunk[A])
