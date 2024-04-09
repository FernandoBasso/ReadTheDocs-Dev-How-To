
public class Context {

    private Strategy strategy;

    public Context(Strategy strategy) {
        this.strategy = strategy;
    }

    public int runStrategy(int[] arr) {
        return strategy.sumNums(arr);
    }
}
