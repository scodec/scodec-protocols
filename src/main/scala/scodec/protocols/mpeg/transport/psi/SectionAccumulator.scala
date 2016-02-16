package scodec.protocols.mpeg
package transport
package psi

/** Accumulates sections of the same table id and table id extension. */
private[psi] class SectionAccumulator[A <: ExtendedSection] private (val sections: GroupedSections[A], sectionByNumber: Map[Int, A]) {

  def add(section: A): Either[String, SectionAccumulator[A]] = {
    def validate(err: => String)(f: Boolean): Either[String, Unit] =
      if (f) Right(()) else Left(err)

    def checkEquality[B](name: String)(f: A => B): Either[String, Unit] =
      validate(name + " do not match")(f(section) == f(sections.head))

    val sectionNumber = section.extension.sectionNumber
    for {
      _ <- checkEquality("table ids")(_.tableId).right
      _ <- checkEquality("table id extensions")(_.extension.tableIdExtension).right
      _ <- checkEquality("versions")(_.extension.version).right
      _ <- checkEquality("last section numbers")(_.extension.lastSectionNumber).right
      _ <- validate("invalid section number")(sectionNumber <= sections.head.extension.lastSectionNumber).right
      _ <- validate("duplicate section number")(!sectionByNumber.contains(sectionNumber)).right
    } yield new SectionAccumulator(GroupedSections(section, sections.list), sectionByNumber + (section.extension.sectionNumber -> section))
  }

  def complete: Option[GroupedSections[A]] =
    if (sectionByNumber.size == (sections.head.extension.lastSectionNumber + 1)) Some(sections) else None
}

private[psi] object SectionAccumulator {

  def apply[A <: ExtendedSection](section: A): SectionAccumulator[A] =
    new SectionAccumulator(GroupedSections(section), Map(section.extension.sectionNumber -> section))
}


