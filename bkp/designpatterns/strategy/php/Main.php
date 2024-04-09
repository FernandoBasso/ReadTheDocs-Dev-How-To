<?php

include 'StrategyInterface.php';
include 'Context.php';
include 'SumAll.php';
include 'SumEven.php';
include 'SumOdd.php';
include 'SumNone.php';


$nums = [1, 2, 3];

$contextAddAll = new Context(new SumAll());
$contextAddEven = new Context(new SumEven());
$contextAddOdd = new Context(new SumOdd());
$contextAddNone = new Context(new SumNone());

echo $contextAddAll->runStrategy($nums) . PHP_EOL;
echo $contextAddEven->runStrategy($nums) . PHP_EOL;
echo $contextAddOdd->runStrategy($nums) . PHP_EOL;
echo $contextAddNone->runStrategy($nums) . PHP_EOL;
