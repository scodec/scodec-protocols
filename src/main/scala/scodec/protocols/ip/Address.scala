package scodec.protocols
package ip

case class Address(value: Either[v4.Address, v6.Address]) {

  def fold[A](if4: v4.Address => A, if6: v6.Address => A): A =
    value.fold(if4, if6)

  override def toString = fold(_.toString, _.toString)
}

object Address {

  def apply(address: v4.Address): Address = new Address(Left(address))
  def apply(address: v6.Address): Address = new Address(Right(address))
}
