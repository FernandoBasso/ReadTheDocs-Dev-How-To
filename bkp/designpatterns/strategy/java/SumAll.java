
public class SumAll implements Strategy {

    @Override
    public int sumNums(int[] arr) {

        int total = 0;

        for (int i = 0; i < arr.length; ++i) {
            total += arr[i];
        }

        return total;
    }

}
