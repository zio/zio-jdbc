package zio.jdbc.model

sealed trait CommentType
object CommentType {
  case object normal extends CommentType
  case object reply extends CommentType
}
