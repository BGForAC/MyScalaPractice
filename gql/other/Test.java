package other;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.PriorityQueue;

public class Test {


    public static void main(String[] args) {
        System.out.println("Hello, World!");
        Integer a=123;
        Boolean b=a instanceof Integer;
        PriorityQueue<Integer> pq=new PriorityQueue<>((o1,o2)->o1-o2);
        pq.poll();
        pq.offer(12);
        ArrayList<Integer> list=new ArrayList<>(Arrays.asList(new Integer[]{1,2,3,4}));
    }
}

class Food{}
class Grass extends Food{}
class Fish extends Food{}
abstract class Animal{
    abstract public void eat(Food food);
}
class Cow extends Animal{
    @Override
    public void eat(Food food) {

    }
}

class AbstractTest {

}
