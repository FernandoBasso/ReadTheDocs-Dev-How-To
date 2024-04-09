
public class Main {

    public static void main(String[] args) {

        int[] nums = {1, 2, 3};

        Context contextAddAll = new Context(new SumAll());
        Context contextAddEven = new Context(new SumEven());
        Context contextAddOdd = new Context(new SumOdd());
        Context contextAddNone = new Context(new SumNone());

        System.out.println(contextAddAll.runStrategy(nums));
        System.out.println(contextAddEven.runStrategy(nums));
        System.out.println(contextAddOdd.runStrategy(nums));
        System.out.println(contextAddNone.runStrategy(nums));

    }

}
