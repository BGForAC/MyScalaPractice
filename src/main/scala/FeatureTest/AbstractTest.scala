package FeatureTest

class Food

class Grass extends Food

class Fish extends Food

abstract class Animal {
  def eat(food: Food)
}

class Cow extends Animal {
  override def eat(food: Food) = {
    println("eat grass")
  }
}

class AbstractTest {

}

object Run extends App {
  val cow: Animal = new Cow
  cow.eat(new Grass)
}
