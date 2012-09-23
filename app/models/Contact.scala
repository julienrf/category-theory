package models

case class Contact(name: String, email: String)

case class Contacts(contacts: Map[Int, Contact]) {
  def all: List[Contact] = contacts.values.toList
  def byId(id: Int): Option[Contact] = contacts.get(id)
}

object Contacts {
  val default = Contacts(Map(
    1 -> Contact("Pierre", "pierre@mail.com"),
    2 -> Contact("Paul", "paul@mail.com"),
    3 -> Contact("Jacques", "jacques@mail.com")
  ))
}