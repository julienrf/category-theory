package controllers

import xml.Node
import play.api.http.Writeable
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import models._

object Application extends Controller {

  val contacts = Contacts.default

  def list = Action { implicit request =>
    render {
      case Accepts.Xml() => Ok(<contacts>{ toXmlF(contacts.all) }</contacts>)
      case Accepts.Json() => Ok(Json.toJson(toJsonF(contacts.all)))
    }
  }

  def read(id: Int) = Action { implicit request =>
    def maybeRender[A : Writeable](toRepr: Option[Contact] => Option[A]) = toRepr(contacts.byId(id)) match {
      case Some(result) => Ok(result)
      case None => NotFound
    }
    render {
      case Accepts.Xml() => maybeRender(toXmlF[Option] _)
      case Accepts.Json() => maybeRender(toJsonF[Option] _)
    }
  }

  def listToXml(contacts: List[Contact]): List[Node] = contacts match {
    case Nil => Nil
    case c :: cs => toXml(c) :: listToXml(cs)
  }

  def listToJson(contacts: List[Contact]): List[JsObject] = contacts match {
    case Nil => Nil
    case c :: cs => toJson(c) :: listToJson(cs)
  }

  def toXml(c: Contact): Node =
    <contact>
      <name>{ c.name }</name>
      <email>{ c.email }</email>
    </contact>

  def toJson(c: Contact): JsObject =
    Json.obj(
      "name" -> c.name,
      "email" -> c.email
    )

  def listMap[A, B](f: A => B)(xs: List[A]): List[B] = xs match {
    case Nil => Nil
    case x :: xs => f(x) :: listMap(f)(xs)
  }

  def optionMap[A, B](f: A => B)(maybeA: Option[A]): Option[B] = maybeA match {
    case None => None
    case Some(a) => Some(f(a))
  }

  trait Functor[F[_]] {
    def fmap[A, B](f: A => B): F[A] => F[B]
  }

  implicit val listFunctor = new Functor[List] {
    def fmap[A, B](f: A => B): List[A] => List[B] = _ match {
      case Nil => Nil
      case a :: as => f(a) :: fmap(f)(as)
    }
  }

  implicit val optionFunctor = new Functor[Option] {
    def fmap[A, B](f: A => B): Option[A] => Option[B] = _ match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def toXmlF[F[_]](fc: F[Contact])(implicit functor: Functor[F]): F[Node] =
    functor.fmap(toXml)(fc)

  def toJsonF[F[_]](fc: F[Contact])(implicit functor: Functor[F]): F[JsObject] =
    functor.fmap(toJson)(fc)

  val optionFunctor2 = new Functor[Option] {
    def fmap[A, B](f: A => B): Option[A] => Option[B] =
      _ => None
  }

  def identity[F[_], A](fa: F[A], functor: Functor[F]): Boolean =
    fa == functor.fmap((a: A) => a)(fa)

}