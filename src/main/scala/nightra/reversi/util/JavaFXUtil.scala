package nightra.reversi.util

import scala.ref.WeakReference
import scalafx.beans.property.{ReadOnlyProperty, Property, BooleanProperty, ObjectProperty}
import scalafx.beans.value.ObservableValue
import scalafx.collections.ObservableBuffer
import scalafx.event.subscriptions.Subscription

object JavaFXUtil {
  private def unit[A](a: A): Unit = ()
  /**
   * Memory scheme:
   * b = a.map(f)
   * b a strong reference to a,
   * while a does not hold a strong reference to b;
   * If b loses all strong references,
   * the change listener from a will be cleaned,
   * and b will be garbage collected.
   **/
  def weaklyBindOnChange[A, B](prop: ReadOnlyProperty[A, _], to: Property[B, _])(f: A => B): Subscription = {
    to.value = f(prop.value)
    val parentBinding = to.onChange(unit(prop)) // capture prop as a strong reference in to, so that it won't be released until to is released
    val weakNewProp: WeakReference[Property[B, _]] = WeakReference(to) // Don't capture 'to in 'prop, so 'to can be cleaned without prop being cleaned.
    lazy val subscription: Subscription = prop.onChange((_, _, _) => weakNewProp.get match {
        case None =>
          subscription.cancel()
          println(s"Cancelling subscription from ${prop.name} to ${to.name}")
        case Some(newProp) =>
          newProp.value = f(prop.value)
      })
    val aggregatedSubscription = new Subscription {
      def cancel() = {
        subscription.cancel()
        parentBinding.cancel()
      }
    }
    subscription // Force the lazy val to tie the recursive knot.
    aggregatedSubscription // Allow the parent binding to be released too.
  }

  def liftObservableList[A](prop: ObjectProperty[ObservableBuffer[A]]): ObservableBuffer[A] = {
    val newList = ObservableBuffer(prop.value)
    newList.onChange(unit(prop)) // capture the original property in memory.
    val weakNewList = WeakReference(newList)
    lazy val subscription: Subscription =
      prop.onChange { (_, _, _) =>
        weakNewList.get match {
          case None => subscription.cancel()
          case Some(solidNewList) =>
            newList.clear()
            newList ++= prop.value
        }
      }
    subscription // Force the lazy val to tie the recursive knot.
    newList
  }

  /**
   * Nice that the whole basic hierarchy is here:
   * Functor,
   * Applicative,
   * and Monad.
   * Actually needed each and every one of them.
   **/

  def flattenProp[A](prop: ReadOnlyProperty[ReadOnlyProperty[A, _], _], name: String): ObjectProperty[A] = {
    val newProp: ObjectProperty[A] = ObjectProperty(null, name, prop.value.value)
    var lastSubscription: Subscription = weaklyBindOnChange(prop.value, newProp)(s => s)
    newProp.onChange(unit(prop))
    val weakNewProp = WeakReference(newProp)
    lazy val subscription: Subscription = prop.onChange { (_, _, _) =>
      val innerProp = prop.value
      lastSubscription.cancel()
      weakNewProp.get match {
        case None =>
          subscription.cancel()
        case Some(weaklyNewProp) =>
          lastSubscription = weaklyBindOnChange(innerProp, weaklyNewProp)(s => s)
      }
    }
    subscription
    newProp
  }

  def mapProp[A, B](prop: ReadOnlyProperty[A, _])(f: A => B, name: String): ObjectProperty[B] = {
    val newProp: ObjectProperty[B] = ObjectProperty(null, name, f(prop.value))
    weaklyBindOnChange(prop, newProp)(f)
    newProp // Strong reference.
  }

  def map2Prop[A, B, C](prop1: ReadOnlyProperty[A, _], prop2: ReadOnlyProperty[B, _])(f: (A, B) => C, name: String): ObjectProperty[C] = {
    val newProp: ObjectProperty[C] = ObjectProperty(null, name, f(prop1.value, prop2.value))
    weaklyBindOnChange(prop1, newProp)(s1 => f(s1, prop2.value))
    weaklyBindOnChange(prop2, newProp)(s2 => f(prop1.value, s2))
    newProp // Strong reference.
  }

  def map3Prop[A, B, C, D](prop1: ReadOnlyProperty[A, _], prop2: ReadOnlyProperty[B, _], prop3: ReadOnlyProperty[C, _])(f: (A, B, C) => D, name: String): ObjectProperty[D] = {
    val newProp: ObjectProperty[D] = ObjectProperty(null, name, f(prop1.value, prop2.value, prop3.value))
    weaklyBindOnChange(prop1, newProp)(s1 => f(s1, prop2.value, prop3.value))
    weaklyBindOnChange(prop2, newProp)(s2 => f(prop1.value, s2, prop3.value))
    weaklyBindOnChange(prop3, newProp)(s3 => f(prop1.value, prop2.value, s3))
    newProp // Strong reference.
  }

  def merge[A](props: Vector[ObjectProperty[A]]): ObjectProperty[A] = {
    val newProp: ObjectProperty[A] = ObjectProperty(props.head.value)
    props.foreach(prop => weaklyBindOnChange(prop, newProp)(s => s))
    newProp // Strong reference.
  }

  def toBooleanProp(prop: ObjectProperty[Boolean]): BooleanProperty = {
    val newProp: BooleanProperty = BooleanProperty(prop.value)
    weaklyBindOnChange(prop, newProp)(s => s)
    newProp // Strong reference.
  }
}
