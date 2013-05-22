import java.io.FileOutputStream
import java.util.zip._

object ZipTest {
  val gurka: Array[Byte] = "gurka".getBytes

  def main(args: Array[String]): Unit = {
    val fos = new FileOutputStream("/tmp/foobar.zip");
    val zos = new ZipOutputStream(fos);
    zos.putNextEntry(new ZipEntry("dir/gurka.txt"))
    zos.write(gurka, 0, gurka.length)
    zos.closeEntry
    zos.close
  }
}