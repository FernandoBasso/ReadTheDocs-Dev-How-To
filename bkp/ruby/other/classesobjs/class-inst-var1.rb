
class A
  def initialize
    @foo = 10
  end
end

class B < A
  def foo
    @foo # @foo very much _is_ inherited from A
  end
end

b = B.new

p b.foo
# →  10


# 11:52 <FernandoBasso> I am trying to find more about class instance variables in the docs but I can't seem to find it. Any suggestions?
# 11:52 <apeiros> FernandoBasso: they are exactly the same as instance variables
# 11:53 <FernandoBasso> -exactly-?
# 11:53 <apeiros> (because they *are* instance variables)
# 11:53 <apeiros> exactly.
# 11:53 <FernandoBasso> What about setting getters and setters for them?
# 11:53 <FernandoBasso> What about inheritance?
# 11:53 <apeiros> the instance to which a class ivar belongs is the class
# 11:53 <apeiros> that means you define accessors on the class of that instance
# 11:54 <FernandoBasso> How is that done?
# 11:54 <apeiros> >> class Foo; class << self; attr_reader :x; end; @x = 1; end; Foo.x
# 11:54 <ruboto> apeiros # => 1 (https://eval.in/431099)
# 11:54 <apeiros> Foo is an instance of Foo.singleton_class
# 11:54 <apeiros> (also of Class, you could define the accessor there too, but then *all* classes would have that)
# 11:55 <apeiros> inheritance: ivars are never inherited.
# 11:55 <FernandoBasso> What is an ivar?
# 11:55 <FernandoBasso> instance variable?
# 11:55 <apeiros> instance variable, @ivar
# 11:55 <apeiros> class variable: @@cvar
# 11:55 <FernandoBasso> Cool.
# 11:55 <apeiros> global variable $gvar
# 11:55 <apeiros> Constant
# 11:55 <apeiros> local_variable
# 11:56 <apeiros> those are the 5 variable types which exist in ruby
# 11:56 <yorickpeterse> apeiros: instance variables are available to subclasses
# 11:56 <apeiros> yorickpeterse: no
# 11:56 <yorickpeterse> they are
# 11:56 <apeiros> no
# 11:56 <apeiros> you're mistaken
# 11:56 <apeiros> the accessors are
# 11:56 <apeiros> but the variables are not.
# 11:56 <yorickpeterse> >> class A; def foo; @number = 10; end; end; class B < A; def bar; @number; end; end; b = B.new; b.foo; b.bar
# 11:56 <ruboto> yorickpeterse # => 10 (https://eval.in/431104)
# 11:56 <yorickpeterse> they are
# 11:57 <apeiros> you're not calling a method on A
# 11:57 <apeiros> you're calling one on B
# 11:57 <apeiros> the variable is not shared
# 11:57 <yorickpeterse> what?
# 11:58 <yorickpeterse> unless you meant they're not shared between class definitions, you're talking out of your arse
# 11:58 <apeiros> you execute code in the context of b, with self being b, which sets an ivar
# 11:58 <apeiros> yorickpeterse: mind your language
# 11:58 <yorickpeterse> If you meant that they're not shared in the same fashion as class variables, then yes, you're right
# 11:59 <yorickpeterse> But instance variables set in a parent class are very much available to sub classes when they're initialized
# 11:59 <apeiros> class A; @x = 1; end; class B < A; end # class B does *not* have access to @x from A. it can define its own @x. it can do so through a method it inherited from A. but that does NOT mean it accesses A's @x
# 11:59 <apeiros> that's the point. you do NOT set it in the parent class
# 11:59 <apeiros> you use *a method inherited* from the parent class. you don't execute that method in the parent class. you execute it in the descendant.
# 11:59 <yorickpeterse> next time you might want to clarify you specifically meant setting ivars on class level
# 12:00 <yorickpeterse> and not on instance level
# 12:00 <apeiros> same thing
# 12:00 <yorickpeterse> _very_ different
# 12:00 <apeiros> works exactly the same.
# 12:00 <yorickpeterse> class Foo; @x = 10; end # you're setting an ivar on an instance of the singleton class
# 12:00 <yorickpeterse> which is _very_ different from setting an ivar on the instance of the class
# 12:00 <apeiros> the mechanics of what has access to what is precisely the same.
# 12:01 <[k-_> ivars and cvars, the worst things that ever happened to a beginner
# 12:01 <yorickpeterse> ivars are fine, cvars are a farce
# 12:01 <yorickpeterse> They literally should've never existed
# 12:01 <yorickpeterse> but then again constants aren't constant either
# 12:01 <yorickpeterse> ¯\_(ツ)_/¯
# 12:02 <apeiros> yorickpeterse: to understand, change your code slightly. class A; def foo; p self.class; @number = 10; end
# 12:02 <apeiros> you'll see that b.foo will print B, not A
# 12:02 <astrobunny> ho lee fugue
# 12:02 <astrobunny> you are right.
# 12:02 <astrobunny> spec.files = `git ls-files -z`
# 12:02 <astrobunny> i do not know why i did not see that
# 12:02 <astrobunny> but wow.
# 12:03 <yorickpeterse> apeiros: oh wow, I totally hadn't noticed
# 12:03 <yorickpeterse> ...
# 12:03 <apeiros> or even: b = B.new; c = B.new; b.foo; c.instance_variables
# 12:03 <apeiros> b has @number, c does not.
# 12:03 <apeiros> ivars are registered to a single object alone
# 12:03 <yorickpeterse> ... you're missing what I said
# 12:04 <apeiros> 13:56 yorickpeterse: apeiros: instance variables are available to subclasses
# 12:04 <yorickpeterse> instance variables are inherited from parent classes, but scoped to their instances
# 12:04 <apeiros> you say that. and that's not correct.
# 12:04 <yorickpeterse> ...it is
# 12:04 <apeiros> no, they are not inherited. you inherit methods.
# 12:04 <apeiros> not ivars.
# 12:04 <apeiros> if you would inherit ivars, they'd show up on .instance_variables.
# 12:04 <yorickpeterse> https://eval.in/431108 then how would this work?
# 12:05 <yorickpeterse> oh right, because you can access ivars from the ancestor chain
# 12:05 <apeiros> I think your mental model of objects is quite broken.
# 12:05 <yorickpeterse> I'm pretty aware of how this works
# 12:05 <apeiros> it works by you evaluating #initialize in the context of `b`
# 12:05 <apeiros> and that method adds an ivar to `b` and `b` alone.
# 12:06 <apeiros> no variable has been inherited in the process.
# 12:06 <yorickpeterse> The example you've showed, of `class Foo; @number = 10; end` is a very different thing from what I've showed
# 12:06 <apeiros> it's the same as doing: b.instance_eval do @foo = 10 end # I didn't inherit @foo from anywhere either
# 12:07 <apeiros> so in short: what you inherit is #initialize
# 12:07 <apeiros> you don't inherit @foo
# 12:07 <apeiros> the inherited code will define your @foo when it is run. how you get to "that means @foo is inherited" from that is beyond me.
# 12:07 <[k-_> apeiros is so clever!
# 12:08 <[k-_> i thought ivars were inherited too
# 12:08 <[k-_> \¯\_(ツ)_/¯
# 12:08 <yorickpeterse> apeiros: it's inherited in the sense that whatever is defined in the ancestor chain (and is called), is available to sub classes
# 12:08 <yorickpeterse> it's not inherited that there's some field like "superclass" or w/e
# 12:09 <apeiros> again, IMO you misunderstand the object model
# 12:09 <yorickpeterse> That is, there's no hard coded chain so to speak (as is the case with classes)
# 12:09 <apeiros> a newly created object has *zero* ivars. it has its own ivar table. which is empty. it has no reference in that table to any class/superclass/chain
# 12:09 <apeiros> ivars are NOT part of inheritance. at all.
# 12:09 <apeiros> cvars are. in a weird way.
# 12:09 <yorickpeterse> " it's inherited in the sense that whatever is defined in the ancestor chain (and is called)"
# 12:10 <apeiros> that doesn't even make sense.
# 12:10 <apeiros> tell me, what's the super equivalent for ivars?
# 12:11 <yorickpeterse> Your initial note made it sound like ivars are not available to subclasses when they're defined in a method that is called from a child class
# 12:11 <yorickpeterse> That in itself is false, which I probably didn't explain entirely clearly
# 12:11 <apeiros> they are not. you get your very own ivar in that child class.
# 12:11 <yorickpeterse> What I meant is that if an instance of a class has a parent class, where something defines an instance variable (e.g. via a method call), then those variables _are_ available to the instance
# 12:11 <apeiros> which is entirely unrelated to the ivar in the superclass.
# 12:12 <apeiros> again: all you share is the code.
# 12:12 <yorickpeterse> And the difference with that vs `class Foo; @number = 10; end` is that this doesn't even set an ivar on the instance of the class, but instead on the singleton class
# 12:12 <FernandoBasso> apeiros: I still don't see where in the docs I would read about this subject.
# 12:12 <apeiros> and again: this code can very well set/assign/define an ivar. but that ivar has absolutely nothing to do with any ivar the same code sets in any other object.
# 12:12 <apeiros> yorickpeterse: nonsense
# 12:13 <yorickpeterse> FernandoBasso: https://rubymonk.com/learning/books/4-ruby-primer-ascent/chapters/45-more-classes/lessons/110-instance-variables covers it
# 12:13 <[k-_> i have a feeling apeiros likes debating this kinds of stuff
# 12:13 <apeiros> >> class Foo; @x = 1; end; Foo.singleton_class.instance_variables # no, it is not set in the singleton_class yorickpeterse
# 12:13 <ruboto> apeiros # => [] (https://eval.in/431129)
# 12:13 <apeiros> >> class Foo; @x = 1; end; Foo.instance_variables # it is set in Foo itself
# 12:13 <ruboto> apeiros # => [:@x] (https://eval.in/431132)
# 12:13 <yorickpeterse> huh, I thought that would go to the singleton class and not the class itself
# 12:13 <yorickpeterse> maybe I'm mixing singleton classes up again
# 12:14 <apeiros> you define the *accessor* in the singleton class
# 12:14 <FernandoBasso> yorickpeterse: Thanks. Still, I mean, where in the "official" docs is that discussed.
# 12:14 <yorickpeterse> Given a bad headache and a flue that's possible
# 12:14 <apeiros> because the accessor defines an *instance* method
# 12:14 <apeiros> and Foo is an instance of Foo.singleton_class
# 12:14 <yorickpeterse> FernandoBasso: not sure actually
# 12:15 <FernandoBasso> This situation reminds me of http://blog.codinghorror.com/learn-to-read-the-source-luke/ where the author says we should read the source because "all the docs probably sucks"...
# 12:15 <Mon_Ouie> The docs seem very misleading actually. Module#attr_accessor states ' creating an instance variable (@name)' which I don't think it does at all.
# 12:15 <yorickpeterse> hm, ruby-doc only seems to cover core/stdlib and some outdated shit
# 12:15 <FernandoBasso> It is either incomplete or inaccurate.
# 12:16 <apeiros> Mon_Ouie: uh, yes, this is indeed very misleading
# 12:16 <yorickpeterse> apeiros: I don't see where this defines accessors in the attr_accessor sense
# 12:16 <yorickpeterse> unless you're referring to something else when you talk about accessors
# 12:16 <apeiros> yorickpeterse: lost where you are. what do you mean?
# 12:16 <yorickpeterse> FernandoBasso: there's an ISO spec, which only covers 1.8, and costs about 200 Euros
# 12:16 <yorickpeterse> apeiros: "you define the *accessor* in the singleton class"
# 12:16 <apeiros> yorickpeterse: not shown in the code
# 12:17 <yorickpeterse> then I'm confused
# 12:17 <apeiros> yorickpeterse: you'd do class << Foo; attr_reader :x; end to access that @x
# 12:17 <yorickpeterse> oh right, that pattern
# 12:17 <apeiros> >> class Foo; @x = 1; class << self; attr_reader :x; end; end; [Foo.instance_variables, Foo.x]
# 12:17 <ruboto> apeiros # => [[:@x], 1] (https://eval.in/431136)
# 12:17 <FernandoBasso> yorickpeterse: And which would probably look like gobbledygook to me.
# 12:17 <yorickpeterse> FernandoBasso: either way, instance variables are variables available to an instance of something only
# 12:18 <yorickpeterse> since Ruby doesn't let you pre-define them (e.g. as you'd probably do in something like Java) they're usually set from within a method
# 12:18 <apeiros> define the reader as an instance method in Foo's class (where you have 2 options: Class and ancestors, or .singleton_class), define the ivar in the instance (Foo)
# 12:18 <yorickpeterse> e.g. the "initialize" method
# 12:18 <shevy> FernandoBasso the best initial tutorial I always found to be were working examples
# 12:18 <ruby-lang923> !help
# 12:18 <ruby-lang923> me
# 12:18 <yorickpeterse> ruby-lang923: just ask your question
# 12:18 <yorickpeterse> FernandoBasso: and yes, the ISO spec is meh
# 12:19 <shevy> 200 euro for meh documentation hmm
# 12:19 <FernandoBasso> Well, thank you all for your invaluable insights.
# 12:19 <yorickpeterse> shevy: doesn't even cover encodings
# 12:19 <yorickpeterse> or the syntax introduced in 1.9
# 12:19 <Mon_Ouie> Well it is meant to be a spec for Ruby 1.8
# 12:20 <certainty> workmad3: ltns o/
#
#
#
#
# <FernandoBasso> And what is the difference between a variable I defined right inside the body of a class vs those I create inside an initialize method?
# <maloik> don't go stealing my docs now
# <yorickpeterse> also wtf Mail dumps downloads in ~/Library?
# <yorickpeterse> wtf Apple
# <shevy> FernandoBasso do you mean @ivar or var ?
# <yorickpeterse> FernandoBasso: they're available in different contexts
# <FernandoBasso> shevy: Just a moment.
# <shevy> FernandoBasso but a general answer is, the scope is different
# <shevy> class Foo; @foo = 'foo' will be different than class Foo; def initialize; @foo = 'foo'
# <Mon_Ouie> @foo always refers to the instance variable called '@foo' of the object referenced to by self
# <yorickpeterse> FernandoBasso: https://eval.in/431140
# <Mon_Ouie> Inside the body of a class statement, self is the class being defined/re-opened
# <Mon_Ouie> Inside the body of a method, self is the object that the method was called on
# <FernandoBasso> http://sprunge.us/FGfN?ruby
# <Mon_Ouie> And we just happen to call the instance variables of objects that are also classes "class instance variables", so your annotations are correct
# <FernandoBasso> I misunderstood what apeiros said then. "they are exactly the same".
# <yorickpeterse> FernandoBasso: euh, technically I believe "instance variable" and "class instance variable" are the same
# <FernandoBasso> Or my question was ill-asked.
# <shevy> different scope FernandoBasso
# <yorickpeterse> at least in Smalltalk they are 2 terms for the same thing
# <shevy> FernandoBasso I would call both instance variables
# <yorickpeterse> FernandoBasso: when you do `class Foo; @number = 10; end` then @number is available to Foo
# <Mon_Ouie> They work the same way, just with a different object referenced to by 'self'
# <sebyx07> for class instance variable, you can define getter and setters through cattr_(reader/writer/accessor)
# <yorickpeterse> when you do `class Foo; def initialize; @number = 10; end; end` then @number is only available to _instances_ of Foo
# <Mon_Ouie> sebyx07: Those methods are not part of Ruby.
# <shevy> FernandoBasso you can give both the same name btw ... class Foo; @foo = 5; def initialize; @foo = 5 <-- two different @foo variables there
# <yorickpeterse> also blegh, my whole brain is full of hay today
# <Mon_Ouie> And looking them up, they set class variables in Rails
#
#
# <certainty> i believe the implementation is roughly equivalent to what apeiros already show. Defining attr_reader on the singleton_class
# <certainty> showed
# <sebyx07> ah, for normal ruby, you can use class << self; attr_(reader/writer/accessor) :myaccessor; end. You can also checkout my DI library i made for ruby  https://github.com/sebyx07/rubybeans
# <apeiros> 14:25 yorickpeterse: FernandoBasso: euh, technically I believe "instance variable" and "class instance variable" are the same
# <apeiros> yes, that
# <FernandoBasso> In some of my tests, I had to do def self.foo; @foo; end; and def self.foo=(val); @foo = val; end to create getters/setters for "class instance variables".
# <yorickpeterse> don't have the book with me, but I'm pretty sure the Smalltalk blue book uses both to refer to the same
# <apeiros> FernandoBasso: yes, `class Foo; def self.foo; @foo; end; end` is equivalent to `class Foo; class << self; attr_reader :foo; end; end`
# <FernandoBasso> All right.
# <shevy> FernandoBasso I'd call them instance variables too, just that they reside on the class level
# <apeiros> >> class Foo; def self.foo; @foo; end; end; Foo.singleton_class.instance_methods(false) # FernandoBasso
# <ruboto> apeiros # => [:foo] (https://eval.in/431148)
#
#
# <apeiros> a class method on Foo is actually really an instance method in Foo.singleton_class
# <apeiros> and instance methods can access an instance's instance variable - makes sense? :D
# <apeiros> and to be precise: an instance method can access the instance variables of the instance they are called on ( == the receiver)
#
#
# <shevy> this be how often we can use the word "instance" in a single sentence :D



