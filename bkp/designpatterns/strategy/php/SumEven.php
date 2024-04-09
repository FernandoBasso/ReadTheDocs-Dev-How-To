<?php

class SumEven implements StrategyInterface {

    public function sumNums(Array $nums) {

        $total = 0;

        for ($i = 0; $i < sizeof($nums); ++$i) {
            if ($nums[$i] % 2 === 0)
                $total += $nums[$i];
        }

        return $total;
    }

}

