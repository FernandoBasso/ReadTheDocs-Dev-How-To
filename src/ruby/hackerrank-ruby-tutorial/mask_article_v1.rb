require 'rspec'

##
# Add <strike> HTML tag around `word`.
#
# If both `str` and `word` are empty, we don't wan't an empty
# `<strike><strike>`, therefore, we do a guard check to handle that
# case.
#
# Partial matches (like 'hell' in 'hello' or 'ana' in 'banana') are
# not enclosed by `<strike>` either. Only full matches will do. That
# is why we use word boundaries in the regex.
#
def strike(str, word_to_strike)
  return str if word_to_strike.empty?

  str.gsub(
    /\b#{word_to_strike}\b/,
    "<strike>#{word_to_strike}</strike>"
  )
end

def mask_article(str, words_to_strike)
  words_to_strike.inject(str) do |memo, word_to_strike|
    return memo if word_to_strike.empty?

    memo.gsub(
      /\b#{word_to_strike}\b/,
      "<strike>#{word_to_strike}</strike>"
    )
  end
end

describe 'strike()' do
  it 'should return string unmodified' do
    expect(strike('Hello, world!', 'crap')).to eq 'Hello, world!'
    expect(strike('Hello, world!', '')).to eq 'Hello, world!'
    expect(strike('', 'crap')).to eq ''
    expect(strike('', '')).to eq ''
  end

  it 'should not strike partial matches' do
    expect(
      strike('Hi, hello there!', 'hell')
    ).to eq 'Hi, hello there!'

    expect(
      strike('Oh look! A banana!', 'ana')
    ).to eq 'Oh look! A banana!'
  end

  it 'should add <strike> tag around ‘word’' do
    expect(
      strike('Holly crap!', 'crap')
    ).to eq 'Holly <strike>crap</strike>!'

    expect(
      strike('Holly crap! The word crap is rude!', 'crap')
    ).to eq(
      ##
      # Note the space before the closign quote and the line
      # continuation backslash.
      #
      'Holly <strike>crap</strike>! ' \
      'The word <strike>crap</strike> is rude!'
    )
  end
end
