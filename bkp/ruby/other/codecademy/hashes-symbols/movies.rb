#!/usr/bin/env ruby -wU

movies = {
    Memento: 3,
    Primer: 4,
    Ishtar: 1
}

puts "What would you like to do?"
puts "-- Type ‘add’ to add a movie."
puts "-- Type ‘update’ to update a movie."
puts "-- Type ‘display’ to display all movies."
puts "-- Type ‘delete’ to delete a movie."

choice = gets.chomp.downcase

case choice
when 'add'
    puts 'What movie would you like to add?'
    title = gets.chomp
    if movies[title.to_sym].nil?
        puts 'What\'s the raging (1 to 4)?'
        rating = gets.chomp
        movies[title.to_sym] = rating.to_i
        puts "#{title} has been added with a rating of #{rating}."
    else
        puts "That movie already exists! It's rating is #{movies[title.to_sym]}."
    end
when 'update'
    puts 'What movie you want to update?'
    title = gets.chomp
    if movies[title.to_sym].nil?
        puts 'Movie not found...'
    else
        puts "What's the new rating (1 to 4)?"
        rating = gets.chomp
        movies[title.to_sym] = rating.to_i
        puts "#{title} has been updated with new rating of #{rating}."
    end
when 'display'
    movies.each do |movie, rating|
        puts "#{movie} → #{rating}."
    end
when 'delete'
    puts 'What is the movie you want to delete?'
    title = gets.chomp
    if movies[title.to_sym].nil?
        puts 'Movie not found.'
    else
        movies.delete(title.to_sym)
        puts "#{title} has been removed."
    end
else
    puts 'Sorry, command not found.'
end

