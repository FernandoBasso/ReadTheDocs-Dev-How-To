---
title: Unit Tests | Ruby on Rails
description: Some notes on and examples on Unit Testing Ruby on Rails applications.
tags: ror, rails, tdd, test, unit-test
---

# Unit Tests | Ruby on Rails

tags: [ror, rails, tdd, test, unit-test]


## assert_equal wrong number args

I actually spent 15 minutes searching the web for what the problem could be! 😅

```rb
class RepoTest < ActiveSupport::TestCase
  test "assert the ✔" do
	assert_equal "✔" "✔"
  end
end
```

Guess what the problem is‽

PS: the missing comma... 😭
