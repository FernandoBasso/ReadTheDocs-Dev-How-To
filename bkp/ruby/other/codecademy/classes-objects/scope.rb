#!/usr/bin/env ruby -wU

class Computer
    $manufacturer = 'Mango Computer, Inc.'
    @@files = {hello: 'Hello, world!'}

    def initialize(username, password)
        @username = username
        @password = password
    end

    def current_user
        @username
    end

    def self.display_files
        @@files
    end
end

# Make a new computer instance.
hal = Computer.new('Dave', 1234)

# @username belongs to the hal instance.
puts "Current user: #{hal.current_user}"

# $manufacturer is global. We can get it directly.
puts "Manufacturer: #{$manufacturer}"

# @@files belongs to the Computer class (not to a single instance).
puts "Files: #{Computer.display_files}"
