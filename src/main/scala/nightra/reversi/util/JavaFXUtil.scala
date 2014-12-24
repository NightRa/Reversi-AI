package nightra.reversi.util

import scalafx.beans.property.{BooleanProperty, ObjectProperty}

object JavaFXUtil {
  def mapProp[A, B](prop: ObjectProperty[A])(f: A => B): ObjectProperty[B] = {
    val newProp: ObjectProperty[B] = ObjectProperty(f(prop.value))
    prop.onChange((_, _, newState) => newProp.value = f(newState))
    newProp
  }

  def map2Prop[A, B, C](prop1: ObjectProperty[A], prop2: ObjectProperty[B])(f: (A, B) => C): ObjectProperty[C] = {
    val newProp: ObjectProperty[C] = ObjectProperty(f(prop1.value, prop2.value))
    prop1.onChange((_, _, newState) => newProp.value = f(newState, prop2.value))
    prop2.onChange((_, _, newState) => newProp.value = f(prop1.value, newState))
    newProp
  }

  def merge[A](props: Vector[ObjectProperty[A]]): ObjectProperty[A] = {
    val newProp: ObjectProperty[A] = ObjectProperty(props.head.value)
    props.foreach(prop => prop.onChange((_, _, newState) => newProp.value = newState))
    newProp
  }

  def toBooleanProp(prop: ObjectProperty[Boolean]): BooleanProperty = {
    val newProp: BooleanProperty = BooleanProperty(prop.value)
    prop.onChange((_, _, newState) => newProp.value = newState)
    newProp
  }
}
