package nightra.reversi.util

import scala.ref.WeakReference
import scalafx.beans.property.{Property, BooleanProperty, ObjectProperty}
import scalafx.beans.value.ObservableValue
import scalafx.event.subscriptions.Subscription

object JavaFXUtil {
  /**
   * Memory scheme:
   * b = a.map(f)
   * b a strong reference to a,
   * while a does not hold a strong reference to b;
   * If b loses all strong references,
   * the change listener from a will be cleaned,
   * and b will be garbage collected.
  **/
  def weaklyBindOnChange[A, C, B, D](prop: ObservableValue[A,C], to: Property[B, D])(f: A => B): Unit = {
    val weakNewProp: WeakReference[Property[B, D]] = WeakReference(to)
    lazy val subscription: Subscription = prop.onChange((_, _, _) => weakNewProp.get match {
      case None => subscription.cancel()
      case Some(newProp) => newProp.value = f(prop.value)
    })
    subscription // Force the lazy val to tie the recursive knot.
  }

  def mapProp[A, B](prop: ObjectProperty[A])(f: A => B): ObjectProperty[B] = {
    val newProp: ObjectProperty[B] = ObjectProperty(f(prop.value))
    weaklyBindOnChange(prop, newProp)(f)
    newProp // Strong reference.
  }

  def map2Prop[A, B, C](prop1: ObjectProperty[A], prop2: ObjectProperty[B])(f: (A, B) => C): ObjectProperty[C] = {
    val newProp: ObjectProperty[C] = ObjectProperty(f(prop1.value, prop2.value))
    weaklyBindOnChange(prop1, newProp)(s1 => f(s1, prop2.value))
    weaklyBindOnChange(prop2, newProp)(s2 => f(prop1.value, s2))
    newProp // Strong reference.
  }

  def merge[A](props: Vector[ObjectProperty[A]]): ObjectProperty[A] = {
    val newProp: ObjectProperty[A] = ObjectProperty(props.head.value)
    props.foreach(prop => weaklyBindOnChange(prop, newProp)(s => s))
    newProp // Strong reference.
  }

  def toBooleanProp(prop: ObjectProperty[Boolean]): BooleanProperty = {
    val newProp: BooleanProperty = BooleanProperty(prop.value)
    weaklyBindOnChange(prop,newProp)(s => s)
    newProp // Strong reference.
  }
}
