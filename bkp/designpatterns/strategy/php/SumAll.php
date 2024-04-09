<?php

class SumAll implements StrategyInterface {

    public function sumNums(Array $nums) {

        $total = 0;

        for ($i = 0; $i < sizeof($nums); ++$i) {
            $total += $nums[$i];
        }

        return $total;
    }

}
