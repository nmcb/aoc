package aoc2022

import nmcb.*

object Day07 extends AoC:

  enum Line:
    case LsLine
    case CdLine(name: String)
    case FileLine(name: String, size: Long)
    case DirLine(name: String)

  import Line.*

  def fromString(line: String): Line =
    line match
      case s"$$ cd $name"                      => CdLine(name)
      case s"$$ ls"                            => LsLine
      case s"$size $name" if size.head.isDigit => FileLine(name, size.toLong)
      case s"dir $name"                        => DirLine(name)

  val stdout: Vector[Line] =
    lines.map(fromString)

  type Path = String

  object Path:
    val empty = ""
    val root  = "/"
    val slash = "/"
    val up    = ".."

    def flatten(lines: Vector[String]): String =
      lines.mkString(slash)

  case class File(name: Path, size: Long)

  case class FileSystem(cur: Vector[String], fs: Vector[File] = Vector.empty, ds: Vector[Path]):

    private val path2Size: Map[String, Long] =
      ds.map(name => name -> sizeOf(name)).toMap

    def sizeOf(path: String): Long =
      fs.filter(_.name.startsWith(path)).map(_.size).sum

    val sizes: Vector[Long] =
      path2Size.valuesIterator.toVector

  object FileSystem:
    val empty: FileSystem =
      FileSystem(Vector(Path.empty), Vector.empty, Vector(Path.root))

  lazy val fileSystem: FileSystem =
    import Path.*
    stdout
      .foldLeft(FileSystem.empty): (sys, line) =>
        line match
          case DirLine(name)        => sys.copy(ds = sys.ds :+ flatten(sys.cur :+ name))
          case FileLine(name, size) => sys.copy(fs = sys.fs :+ File(flatten(sys.cur :+ name), size))
          case CdLine(Path.up)      => sys.copy(cur = sys.cur.dropRight(1))
          case CdLine(name)         => sys.copy(cur = sys.cur :+ name)
          case _                    => sys


  lazy val free: Long  = 70000000L - fileSystem.sizeOf("/")
  lazy val clean: Long = 30000000L - free

  override lazy val answer1: Long = fileSystem.sizes.filter(_ <= 100000L).sum
  override lazy val answer2: Long = fileSystem.sizes.filter(_ >= clean).min