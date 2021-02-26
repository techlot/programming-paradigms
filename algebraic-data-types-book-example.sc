case class Section(start: Int, end: Int)
case class Chapter(sections: List[Section])
case class Book(chapters: List[Chapter])

type SectionLength = Int
type ChapterSectionsLengths = List[SectionLength]
type BookDescription = List[ChapterSectionsLengths]

def makeASection(start: Int, len: SectionLength): Section = Section(start, start + len - 1)

def makeAChapter(start: Int, lengths: ChapterSectionsLengths): Chapter = {
  @scala.annotation.tailrec
  def makeChapterInner(start: Int, lengths: ChapterSectionsLengths, chapter: Chapter): Chapter = lengths match {
    case Nil => Chapter(chapter.sections.reverse) //bad because we need to reverse each time
    case hd::tl => makeChapterInner(start + hd, tl, Chapter(makeASection(start, hd)::chapter.sections)) //always use ::
  }
  makeChapterInner(start, lengths, Chapter(List()))
}

def makeChapters(start: Int, description: BookDescription): List[Chapter] = {
  @scala.annotation.tailrec
  def makeChaptersInner(start: Int, description: BookDescription, chapters: List[Chapter]): List[Chapter] = description match {
    case Nil => chapters.reverse
    case hd::tl => makeChaptersInner(start + hd.sum, tl, makeAChapter(start, hd)::chapters)
  }
  makeChaptersInner(start, description, List())
}

def makeABook(description: BookDescription): Book = {
  Book(makeChapters(1, description))
}

val chapter1SectionLengths: ChapterSectionsLengths = List(1, 2, 5)
val chapter2SectionLengths: ChapterSectionsLengths = List(7, 7, 9)
val chapter3SectionLengths: ChapterSectionsLengths = List(2, 3, 4)

val description: BookDescription = List(chapter1SectionLengths, chapter2SectionLengths, chapter3SectionLengths)

makeABook(description)
