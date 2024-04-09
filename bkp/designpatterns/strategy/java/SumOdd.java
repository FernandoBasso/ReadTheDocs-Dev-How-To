
public class SumOdd implements Strategy {

    @Override
    public int sumNums(int[] arr) {

        int total = 0;

        for (int i = 0; i < arr.length; ++i) {
            if (arr[i] % 2 != 0) {
                total += arr[i];
            }
        }

        return total;
    }

}

