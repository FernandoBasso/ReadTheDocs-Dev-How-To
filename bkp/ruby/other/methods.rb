class Foo

    def sayFoo
        p 'foo'
    end

    def self.sayFoo
        p 'self foo'
    end
end


Bar = Class.new do
    def sayBar
        p 'bar'
    end

    def self.sayBar
        p 'self bar'
    end
end


Foo.new.sayFoo
Foo.sayFoo

Bar.new.sayBar
Bar.sayBar

#
# Both Foo and Bar are instances of Class
#
p Foo.class
# → Class
p Bar.class
# → Class
