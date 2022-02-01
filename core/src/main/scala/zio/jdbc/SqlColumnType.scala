package zio.jdbc

import java.sql.Blob

trait SqlColumnType  {
  def handle[A](matcher: SqlColumnType.Matcher[A]): A
}
object SqlColumnType {
  trait Matcher[+A] {
    def handleInt(value: Int): A
    def handleLong(value: Long): A
    def handleShort(value: Short): A
    def handleString(value: String): A
    def handleBigDecimal(value: BigDecimal): A
    def handleBoolean(value: Boolean): A
    def handleByte(value: Byte): A
    def handleFloat(value: Float): A
    def handleDouble(value: Double): A
    def handleBlob(blob: Blob): A
    def handleUnknown(other: Any): A
  }
}
