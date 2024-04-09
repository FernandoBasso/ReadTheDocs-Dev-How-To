<?php

class Context {

    /**
     * @var StrategyInterface
     */
    private $strategy;

    public function __construct(StrategyInterface $strategy) {
        $this->strategy = $strategy;
    }

    public function runStrategy($nums) {
        return $this->strategy->sumNums($nums);
    }

}
